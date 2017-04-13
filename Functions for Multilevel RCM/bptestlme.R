bptest.lme <- function(model,data){
  n <- nrow(model$data[-model$na.action,])
  resi <- resid(model)
  sigma2 <- sum(resi^2)/n
  data$w <- resi^2 - sigma2
  fmla <- eval(model$call$fixed)
  fmlabp <- update(fmla,w~.) 
  fv <- lme(fmlabp,random=~1|Family,data=data,control=list(opt="optim"))$fitted[,1]
  bp <- n * sum(fv^2)/sum(data$w^2)
  method <- "studentized Breusch-Pagan test"
  df = length(model$coef$fixed)-1
  ans <- paste("Breusch-Pagan test","BP=",bp,"df=",df,"p-value=",pchisq(bp, df, lower.tail = FALSE))
  return(ans)
}