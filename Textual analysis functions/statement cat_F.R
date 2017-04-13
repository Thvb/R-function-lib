####Statement categorization function####
#########################################

##testset: binary row indices indicating which rows belong to the testset (testset = 1; trainingset = 0).

statement_categorization <- function(codingfile,statcol, categories,algorithms,virgin=F,ngrm=1,original.matrix=NULL,trained.model=NULL,testset=NULL,benchmark=c("individual","combined"),dtmcontrol=NULL,write.files=F,train.perc=NULL,unbalanced.classes=NULL,maxent.control=NULL,svm.control=NULL,GLMNET.control=NULL,tree.control=NULL,rf.control=NULL,nnet.control=NULL,tune=T,tune.controls=NULL){
  require(RTextTools)
  require(tm)
  require(maxent)
  require(e1071)
  require(glmnet)
  require(DMwR)
  require(caret)
  require(randomForest)
  
  stopifnot(is.data.frame(codingfile),is.factor(codingfile[,statcol])||is.character(codingfile[,statcol]))
  benchmark <- match.arg(benchmark,c("individual","combined"),several.ok = T)
  if(is.character(categories)){
    stopifnot(all(categories%in%colnames(codingfile)))
    categories <- match(categories,colnames(codingfile))
  }
  if(!exists("create_matrix2")){
    stop("Function create_matrix2 not found.")
  }
  if(virgin==F&&(!is.null(original.matrix)||!is.null(trained.model))){
    message("Arguments orginal.matrix and trained.model are ignored when virgin is FALSE.")
  }
  if(virgin==T&&(is.null(original.matrix)||is.null(trained.model))){
    message("Virgin is TRUE, yet orginal.matrix and/or trained.model were not supplied.")
  }
  
  if(is.null(dtmcontrol)){
    options <- list(toLower=T,stripWhitespace=T,removePunctuation=TRUE,removeStopwords=TRUE,removeNumbers=TRUE,stemWords=TRUE,weighting=tm::weightTfIdf, ngramLength=ngrm)
  }else{
    options <- list(toLower=T,stripWhitespace=T,removePunctuation=TRUE,removeStopwords=TRUE,removeNumbers=TRUE,stemWords=TRUE,weighting=tm::weightTfIdf, ngramLength=ngrm)
    mtch <- match(names(dtmcontrol),names(options))
    if(any(is.na(mtch))){
      stop("dtmcontrol has unused argument: ",names(dtmcontrol)[!(names(dtmcontrol)%in%names(options))])
    }
    options[mtch] <- dtmcontrol
  }
  if(!is.null(unbalanced.classes)){
  stopifnot(length(unbalanced.classes)==1,is.numeric(unbalanced.classes)||is.character(unbalanced.classes))
  }
  if(is.null(train.perc)){
    train.perc=80
    divisor <- 100/(100-train.perc)
  }else{
      divisor <- 100/(100-train.perc)
  }
  if(virgin==T&&!is.null(benchmark)){
    stop("You cannot benchmark when autocoding virgin data.")
  }
    categor <- colnames(codingfile)[categories]
    Statements <- as.character(codingfile[,statcol])
    
    ANS <- vector("list",length(categor)*length(train.perc))
    Benchmarklist <- vector("list",length(benchmark))
    Benchmarklist<-lapply(Benchmarklist,function(x){ls <- vector("list",length(categor));names(ls)<-categor;return(ls)})
    names(Benchmarklist) <- benchmark
    benchmark <- c(benchmark,"training.percentages")
    names(ANS) <- categor
    
    pb <- txtProgressBar(min = 0, max = 100, style = 3)
    progr <- 0
    for(i in 1:length(categor)){
      
      categoryname <- categor[i]
      category <- data.frame(cbind(c(Statements),codingfile[,categoryname]))
      colnames(category) <- c("Statements",categor[i])
      
      if(i==1){
        # create document term matrix
        if(virgin==F){
          mat= create_matrix2(category$Statements, language="english",options)
        }else{
          mat= create_matrix2(category$Statements, language="english",options,originalMatrix=original.matrix)
        }
      }
      if(("training.percentages"%in%benchmark)||("individual"%in%benchmark)){
        if(length(train.perc)==1){
          extra=1
          nameextra="ignore.column"
        }else{
          extra=0
          nameextra=NULL
        }
        acc.table <- mat.or.vec(length(algorithms)+1,length(train.perc)+extra)
        colnames(acc.table) <- c(paste0(train.perc,"%"),nameextra)

        recall.table <- prec.table <- mat.or.vec(length(algorithms),length(train.perc)*length(unique(category[,categoryname]))+extra)
        colnames(recall.table) <- colnames(prec.table) <- c(paste0(rep(paste0(train.perc,"%."),each=2),unique(category[,categoryname])),nameextra)
        
        }
      for(j in 1:length(train.perc)){
        progr <- progr+1
        prog <- round(progr/(length(categor)*length(train.perc))*100,2)
        
      if(is.null(testset)){
        set.seed(5)
        L <- grep(0,category[,categoryname])
        set <- sample(L,ceiling(length(L)/divisor[j]))
        if(is.numeric(unbalanced.classes)){
          div2 <- 100/(100-unbalanced.classes)
        }else{
          div2 <- divisor[j]
        }
        L2 <- grep(1,category[,categoryname])
        set <- c(set,sample(L2,ceiling(length(L2)/div2)))
      }else{
        set <- testset
      }

      category$testset <- 0
      category$testset[set] <- 1
      
      category <- category[order(category$testset),]
      colnames(category) <- c("Statements",categoryname,"testset")
      category[,categoryname] <- as.factor(category[,categoryname])
      
      #Training
      category <- category[order(category$testset),]
      if(!is.null(unbalanced.classes)){
      if(unbalanced.classes=="SMOTE"){
        mat2 <- data.frame(as.matrix(mat))
        mat2$testset <- category$testset
        mat2[,categoryname] <- as.factor(category[,categoryname])
        
        trainingset <- SMOTE(as.formula(paste0(categoryname,"~ .")), mat2[mat2$testset==0,], perc.over = (table(mat2$testset,mat2[,categoryname])[1,1]/table(mat2$testset,mat2[,categoryname])[1,2])*30, perc.under=100)
        fullmat <- rbind(mat2[mat2$testset==1,],trainingset)
        data.temp <- fullmat[,categoryname]
        fullmat[,categoryname] <- NULL
        }else{
        fullmat <- mat
        fullmat$testset <- category$testset
        data.temp <-  category[,categoryname]
        }
      }else{
        fullmat <- mat
        fullmat$testset <- category$testset
        data.temp <-  category[,categoryname]
      }
      if(virgin==T){
        category[,categoryname] <- as.factor(c(array(0,dim=nrow(codingfile)),as.character(category[category$testset==1,categoryname])))
        trainsize = NULL
        testsize = 1:nrow(codingfile)
      }else{

        trainsize = grep(0,fullmat$testset)
        testsize = grep(1,fullmat$testset)
      }
      fullmat$testset <- NULL
     
      container = create_container(fullmat,data.temp,trainSize=trainsize, testSize=testsize,virgin=virgin)
      
      if(virgin==F){
        models <- list()
        #models = train_models2(container, algorithms=algorithms,ml.control)
        
        if(tune==T){
          tune.cont <- tune.control(random = FALSE, nrepeat = 3, repeat.aggregate = mean,
                                    sampling = c("cross"), sampling.aggregate = mean,
                                    sampling.dispersion = sd,
                                    cross = 4, fix = 2/3, nboot = 10, boot.size = 9/10, best.model = TRUE,
                                    performances = TRUE, error.fun = NULL)
          mtch <- match(names(tune.cont),names(tune.controls))
          tune.cont[mtch] <- tune.controls
        }
        
        if("SVM"%in%algorithms){
          svm.cont <- list(scale = TRUE, type = NULL, kernel="radial", degree = 3, gamma = if (is.vector(container@training_matrix)) 1 else 1 / ncol(container@training_matrix),coef0 = 0, cost = 1, nu = 0.5,class.weights = F, cachesize = 200, tolerance = 0.001, epsilon = 0.0001,shrinking = TRUE, cross = 4, probability = T, fitted = TRUE,subset=NULL, na.action = na.omit)
          mtch <- match(names(svm.control),names(svm.cont))
          svm.cont[mtch] <- svm.control
          if(svm.cont$class.weights){
            cw <- c(100/table(category[,categoryname]))
          }else{
            cw=NULL
          }
          
          if(tune==T){
            expon <- seq(-5,15,by=2)
            costlist <- 2^expon
            kernels <- svm.cont$kernel
  
              if(kernels=="polynomial"){
                expon <- seq(-15,5,by=3)
                gammalist <- 2^expon
                degree=list(3,6);coef0=list(0,1)
              }else{
                degree=3;coef0=0
                if(kernels=="linear"){
                  gammalist=svm.cont$gamma
                }else{
                  expon <- seq(-15,5,by=3)
                  gammalist <- 2^expon
                }
              }
              
              tunelist <- tune.svm(x = container@training_matrix, y = container@training_codes, degree = degree, gamma = gammalist,coef0 = coef0, cost = costlist, nu = NULL, class.weights = cw,kernel=kernels, epsilon = 0.0001,cachesize = 200, tolerance = 0.001,shrinking = TRUE, probability = T, fitted = TRUE,subset=NULL, na.action = na.omit)
                    
              models$SVM <- tunelist$best.model
            }else{
          
              models$SVM <- svm(x = container@training_matrix, y = container@training_codes,scale=svm.cont$scale,type=svm.cont$type,kernel=svm.cont$kernel,degree=svm.cont$degree,gamma=svm.cont$gamma,coef0=svm.cont$coef0,cost=svm.cont$cost,nu=svm.cont$nu,class.weights = cw,cachesize=svm.cont$cachesize,tolerance=svm.cont$tolerance,epsilon=svm.cont$epsilon,shrinking=svm.cont$shrinking,cross=svm.cont$cross,fitted=svm.cont$fitted,subset=svm.cont$subset,na.action=svm.cont$na.action,probability = svm.cont$probability)
            }
          }
        if("MAXENT"%in%algorithms){
          maxent.cont <- list(l1_regularizer=0.0, l2_regularizer=0.0,use_sgd=TRUE, set_heldout=0, verbose=FALSE)
          mtch <- match(names(maxent.control),names(maxent.cont))
          maxent.cont[mtch] <- maxent.control
          if(tune==T){
            tunelist <- data.frame(tune.maxent(container@training_matrix, as.vector(container@training_codes),nfold=4,showall=T))
            tunelist <- tunelist[order(tunelist$accuracy,tunelist$pct_best_fit,decreasing = T),]; tunelist <- tunelist[1,]
            maxent.cont <- list(l1_regularizer=tunelist$l1_regularizer, l2_regularizer=tunelist$l2_regularizer,use_sgd=tunelist$use_sgd, set_heldout=tunelist$set_heldout, verbose=FALSE)
          }
            models$MAXENT <- maxent(container@training_matrix, as.vector(container@training_codes),l1_regularizer=maxent.cont$l1_regularizer, l2_regularizer=maxent.cont$l2_regularizer,use_sgd=maxent.cont$use_sgd, set_heldout=maxent.cont$set_heldout, verbose=maxent.cont$verbose)
        }
        if ("GLMNET"%in%algorithms) {
          
            GLMNET.cont <- list(family=c("multinomial"),
                                weights=F, offset=NULL, alpha = 1, nlambda = 100,
                                lambda.min.ratio = ifelse(length(trainsize)<ncol(mat),0.01,0.0001), lambda=NULL,
                                standardize = TRUE, intercept=TRUE, thresh = 1e-07,  dfmax = ncol(mat) + 1,
                                pmax = min(ncol(mat) + 1 * 2+20, ncol(mat)), exclude=NULL, penalty.factor = rep(1, ncol(mat)),
                                lower.limits=-Inf, upper.limits=Inf, maxit=10^5,
                                type.gaussian=ifelse(ncol(mat)<500,"covariance","naive"),
                                type.logistic=c("Newton"),
                                standardize.response=FALSE, type.multinomial=c("ungrouped"))
            mtch <- match(names(GLMNET.control),names(GLMNET.cont))
            GLMNET.cont[mtch] <- GLMNET.control
            
            training_matrix <- as(container@training_matrix, "sparseMatrix")
            
            if(GLMNET.cont$weights){
              cw <- c(100/table(category[,categoryname]))
              cw <- sapply(container@training_codes,function(x){ifelse(x==0,cw[1],cw[2])})
            }else{
              cw=rep(1, length(trainsize))
            }
            
          if(tune==T){
            lambda.seq <- exp(seq(log(1e-5), log(1e0), length.out = 20));lambda.seq <- lambda.seq[-length(lambda.seq)]
            alpha.seq <- seq(0,1,length.out=20)
            trc <- trainControl(method="cv",number=4,repeats=3,search="grid")
            tunelist <- train(x = training_matrix, y = container@training_codes,trControl=trc,method="glmnet",tuneGrid=expand.grid(alpha=alpha.seq,lambda=lambda.seq),family=GLMNET.cont$family)
            models$GLMNET <- tunelist$finalModel
          }else{
            models$GLMNET <- glmnet(x = training_matrix, y = container@training_codes, 
                                    family = GLMNET.cont$family,weights=cw,offset=GLMNET.cont$offset,alpha=GLMNET.cont$alpha,lambda.min.ratio=GLMNET.cont$lambda.min.ratio,
                                    nlambda=GLMNET.cont$nlambda,lambda=GLMNET.cont$lambda,standardize=GLMNET.cont$standardize,intercept=GLMNET.cont$intercept,thresh=GLMNET.cont$thresh,
                                    pmax=GLMNET.cont$pmax,dfmax=GLMNET.cont$dfmax,exclude=GLMNET.cont$exclude,penalty.factor=GLMNET.cont$penalty.factor,lower.limits=GLMNET.cont$lower.limits,
                                    upper.limits=GLMNET.cont$upper.limits,type.gaussian=GLMNET.cont$type.gaussian,type.logistic=GLMNET.cont$type.logistic,standardize.response=GLMNET.cont$standardize.response,
                                    type.multinomial=GLMNET.cont$type.multinomial,maxit=GLMNET.cont$maxit)
          }
        }
        if("RF"%in%algorithms){
          x = as.matrix(container@training_matrix)
          y = container@training_codes
          
          rf.cont <- list(xtest=NULL, ytest=NULL, ntree=300,
                          mtry=if (!is.null(y) && !is.factor(y))
                            max(floor(ncol(x)/3), 1) else floor(sqrt(ncol(x))),
                          replace=TRUE, classwt=F, 
                          sampsize = nrow(x),
                          nodesize = if (!is.null(y) && !is.factor(y)) 5 else 1,
                          maxnodes = NULL,
                          importance=FALSE, localImp=FALSE, nPerm=1,
                          norm.votes=TRUE, do.trace=FALSE,
                          keep.forest=T, corr.bias=FALSE,
                          keep.inbag=FALSE)
          mtch <- match(names(rf.control),names(rf.cont))
          rf.cont[mtch] <- rf.control
          
          if(rf.cont$classwt){
            cw <- c(100/table(category[,categoryname]))
          }else{
            cw=NULL
          }
          
          if(tune==T){
            
            node.seq <- c(1,2)
            mtry.seq <- c(floor(sqrt(ncol(x)))/2,floor(sqrt(ncol(x))),floor(sqrt(ncol(x)))*2)
            ntry.seq <- c(1500)
            tunelist <- tune.randomForest(x = x, 
                              y = y, nodesize = node.seq,mtry = mtry.seq, ntree = ntry.seq,classwt=cw)
            models$FORESTS <- tunelist$best.model
          }else{
            models$FORESTS <- randomForest(x = x, 
                              y = y, ntree=rf.cont$ntree,xtest=rf.cont$xtest,ytest=rf.cont$ytest,mtry=rf.cont$mtry,replace=rf.cont$replace,
                              classwt=cw,sampsize=rf.cont$sampsize,nodesize=rf.cont$nodesize,maxnodes=rf.cont$maxnodes,
                              importance=rf.cont$importance,localImp=rf.cont$localImp,nPerm=rf.cont$nPerm,norm.votes=rf.cont$norm.votes,
                              do.trace=rf.cont$do.trace,keep.forest=rf.cont$keep.forest,corr.bias=rf.cont$corr.bias,keep.inbag=rf.cont$keep.inbag)
          }
        }
        if("TREE"%in%algorithms){
          tree.cont <- list(weights=F, subset=NULL,
                          na.action = na.pass, control = tree.control(nobs, ...),
                          method = "recursive.partition",
                          split = c("deviance"),
                          model = FALSE, x = FALSE, y = TRUE, wts = TRUE)
          mtch <- match(names(tree.control),names(tree.cont))
          tree.cont[mtch] <- tree.control
          
          if(tree.cont$weights){
            cw <- c(100/table(category[,categoryname]))
          }else{
            cw=NULL
          }
          if(tune==T){
            tunelist <- tune.rpart(container.training_codes ~ ., data = data.frame(as.matrix(container@training_matrix), 
                                                                                   container@training_codes), na.action = na.omit, minsplit = NULL,
                                   minbucket = NULL, cp = NULL, maxcompete = NULL, maxsurrogate = NULL,
                                   usesurrogate = NULL, xval = NULL, surrogatestyle = NULL, maxdepth =
                                     NULL, predict.func = NULL)
          }else{
            models$TREE <- tree(container.training_codes ~ ., data = data.frame(as.matrix(container@training_matrix), 
                                                                      container@training_codes),weights=cw,subset=tree.cont$subset,na.action=tree.cont$na.action,control=tree.cont$control,
                              method=tree.cont$method,split=tree.cont$split,model=tree.cont$model,x=tree.cont$x,y=tree.cont$y,wts=tree.cont$wts)
          }
        }
        if("NNET"%in%algorithms){
          models$NNET <- nnet(container.training_codes ~ ., data = data.frame(as.matrix(container@training_matrix), 
                                                                        container@training_codes), size = size, maxit = maxitnnet, 
                        MaxNWts = MaxNWts, rang = rang, decay = decay, trace = trace)
        }
        #Classify testset
        results = classify_models(container, models)
        analytics = create_analytics(container, results)
        
        if(write.files==T){
          write.csv2(analytics@document_summary,paste0("Categorization/Modelstats/models_DocumentSummary_",categoryname,train.perc[j],format(Sys.time(),"%m-%d-%y"),".csv"))
          write.csv2(analytics@algorithm_summary,paste0("Categorization/Modelstats/models_AlgorithmSummary_",categoryname,train.perc[j],format(Sys.time(),"%m-%d-%y"),".csv"))
          write.csv2(analytics@ensemble_summary,paste0("Categorization/Modelstats/models_EnsembleSummary_",categoryname,train.perc[j],format(Sys.time(),"%m-%d-%y"),".csv"))
        }
        if("training.percentages"%in%benchmark||("individual"%in%benchmark)){
          
          acc.table[1,j] <- round(sort(table(category[testsize,categoryname]),decreasing = T)[1]/length(category[testsize,categoryname]),2)*100
          
          numalg <- grep("LABEL",colnames(analytics@document_summary))
          sequ <- seq(1,ncol(recall.table),by=2)
          for(k in 1:length(numalg)){
            tab <- table(factor(analytics@document_summary[,numalg[k]],levels=sort(unique(category[,categoryname]))),factor(analytics@document_summary$MANUAL_CODE,levels=sort(unique(category[,categoryname]))))
            if(nrow(tab)==1){
              
            }
            acc.table[k+1,j] <- round(sum(diag(tab))/sum(tab),2)*100
            
            recall.table[k,c(sequ[j],sequ[j]+1)] <- round(diag(tab)/colSums(tab),2)*100
            prec.table[k,c(sequ[j],sequ[j]+1)] <- round(diag(tab)/rowSums(tab),2)*100
            
            }
          
          dominant <- names(sort(table(category[,categoryname]),decreasing = T))[1]
          rownames(acc.table) <- c(paste0("Default-",dominant), gsub("_LABEL","",colnames(analytics@document_summary)[numalg]))
          rownames(recall.table) <- rownames(prec.table) <- c(gsub("_LABEL","",colnames(analytics@document_summary)[numalg]))
          }
        
        tablist <- list(acc.table,recall.table,prec.table)
        names(tablist) <- c("Accuracy","Recall","Precision")
        Benchmarklist$individual[[i]] <- tablist
        
       ANS[[i]] <- models
      }else{      
        #categorize sentences with trained model
        #classify using trained models
        
        results = classify_models(container, trained.model[names(trained.model)%in%categoryname])#trained.model name selector not tested. Potential solution if problem = renaming algorithm list entries per category
        analytics <- create_analytics(container, results)
        
        if(write.files==T){
          write.csv(analytics@label_summary,paste0("Categorization/Autocoding/SampleData_LabelSummary_",categoryname,train.perc[j],format(Sys.time(),"%m-%d-%y"),".csv"))
          write.csv(analytics@document_summary,paste0("Categorization/Autocoding/SampleData_DocumentSummary_",categoryname,train.perc[j],format(Sys.time(),"%m-%d-%y"),".csv"))
        }
        ANS[[i]] <- analytics
      }
      setTxtProgressBar(pb,prog)
      }
    }
    ANS$OriginalMatrix <- original.matrix
    ANS$Benchmark <- Benchmarklist
    return(ANS)
}


