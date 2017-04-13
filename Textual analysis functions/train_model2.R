train_model2 <- function (container, algorithm = c("SVM", "SLDA", "BOOSTING", 
                                   "BAGGING", "RF", "GLMNET", "TREE", "NNET", "MAXENT"), type = NULL, 
          cross = 0, cost = 100, kernel = "radial", maxitboost = 100, 
          maxitglm = 10^5, size = 1, maxitnnet = 1000, MaxNWts = 10000, 
          rang = 0.1, decay = 5e-04, trace = FALSE, ntree = 200, l1_regularizer = 0, 
          l2_regularizer = 0, use_sgd = FALSE, set_heldout = 0, verbose = FALSE,class.weights=NULL, 
          ...) 
{
  gc()
  if (algorithm == "SVM") {
    print(c(type,kernel,class.weights))
    model <- svm(x = container@training_matrix, y = container@training_codes,type = type, cross = cross, cost = cost, probability = TRUE,kernel = kernel,class.weights=class.weights)
  }
  else if (algorithm == "SLDA") {
    model <- slda(container.training_codes ~ ., data = data.frame(as.matrix(container@training_matrix), 
                                                                  container@training_codes))
  }
  else if (algorithm == "BOOSTING") {
    model <- LogitBoost(xlearn = as.matrix(container@training_matrix), 
                        ylearn = container@training_codes, nIter = maxitboost)
  }
  else if (algorithm == "BAGGING") {
    model <- bagging(container.training_codes ~ ., data = data.frame(as.matrix(container@training_matrix), 
                                                                     container@training_codes))
  }
  else if (algorithm == "RF") {
    model <- randomForest(x = as.matrix(container@training_matrix), 
                          y = container@training_codes, ntree = ntree)
  }
  else if (algorithm == "GLMNET") {
    training_matrix <- as(container@training_matrix, "sparseMatrix")
    model <- glmnet(x = training_matrix, y = container@training_codes, 
                    family = "multinomial", maxit = maxitglm)
  }
  else if (algorithm == "TREE") {
    model <- tree(container.training_codes ~ ., data = data.frame(as.matrix(container@training_matrix), 
                                                                  container@training_codes))
  }
  else if (algorithm == "NNET") {
    model <- nnet(container.training_codes ~ ., data = data.frame(as.matrix(container@training_matrix), 
                                                                  container@training_codes), size = size, maxit = maxitnnet, 
                  MaxNWts = MaxNWts, rang = rang, decay = decay, trace = trace)
  }
  else if (algorithm == "MAXENT") {
    model <- maxent(container@training_matrix, as.vector(container@training_codes), 
                    l1_regularizer, l2_regularizer, use_sgd, set_heldout, 
                    verbose)
  }
  else {
    stop("ERROR: Invalid algorithm specified. Type print_algorithms() for a list of available algorithms.")
  }
  gc()
  return(model)
}
