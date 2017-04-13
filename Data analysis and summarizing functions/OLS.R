###############
#OLS estimator#
###############
OLS <- function(data=NULL, DV, IV){
  if(!is.null(data)){
  data <- data
  y <- as.matrix(data[,DV])
  Z <- as.matrix(data[,IV])
  }else{
    y <- as.matrix(DV)
    Z <- as.matrix(IV)
  }
  missingcheck <- cbind(y,Z)
  for(i in 1:ncol(missingcheck)){
    missingcheck <- subset(missingcheck,!is.na(missingcheck[,i]))
  }
  y <- missingcheck[,1]
  Z <- missingcheck[,2:ncol(missingcheck)]
  X <- cbind(array(1, dim=nrow(missingcheck)),Z)
  
  betahats <- (solve((t(X)%*%X))%*%t(X))%*%y
  rownames(betahats)[1]<- c("Intercept")
  rownames(betahats)[2:(ncol(X))] <- paste("Regressor",1:(ncol(X)-1))
  return(betahats)
}