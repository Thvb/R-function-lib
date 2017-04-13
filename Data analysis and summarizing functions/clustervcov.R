get_CL_vcov<-function(model, cluster){
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  
  #calculate degree of freedom adjustment
  cluster <- factor(cluster)
  if(!is.null(model$na.action)){
    cluster <- cluster[-as.numeric(model$na.action)]
  }
  M <- nlevels(cluster)
  N <- sum(!is.na(cluster))
  K <- model$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  
  #calculate the uj's
  uj  <- apply(estfun(model),2, function(x) tapply(x, cluster, sum))
  
  #use sandwich to get the var-covar matrix
  vcovCL <- dfc*sandwich(model, meat=crossprod(uj)/N)
  return(vcovCL)
}
