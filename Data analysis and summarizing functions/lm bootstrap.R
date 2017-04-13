

bootstrapRR<-function(data, model,reps){
  #data is data, 
  # model is the model to be bootstrapped, and 
  # reps is the number of bootstrap replications
  
  l1<-length(coef(model))
  tmp<-t(coef(model))
  bootvals <- data.frame(matrix(rep(NA,reps*l1), nrow=reps))
  names(bootvals)<-names(coef(model))
  
  bootresults<-data.frame(matrix(NA, nrow=length(tmp),ncol=4))
  rownames(bootresults)<-names(coef(model)); 
  colnames(bootresults) <- c("Estimate","Odds Ratio","Std. Error","p-value")
  
  
  for(i in 1:reps){
    
    c.sample <- sample(rownames(data),replace=T)
    c.boot<-data[c.sample,]
    
    tryCatch({
      x1 <- glm.nb(formula(model),data=c.boot)
      if(length(t(coef(x1)))==l1 ){
        bootvals[i,] <- t(coef(x1))
      }
    },error = function(ex) {
      bootvals[i,] <-NA
    })
    
    
    cat("\r",i*100/reps," % done ");flush.console() #concatenate and print
  }
  
  bootresults[,1]=round(coef(model),digits=2) #coefficient estimates from the main model
  bootresults[,2]=round(exp(bootresults[,1]),digits=2) #odds ratio
  bootresults[,3]=round(apply(bootvals,2,sd,na.rm = TRUE),digits=2) #standard error
  bootresults[,4]=round(2*pt(abs(bootresults[,1]/bootresults[,3]),df=50,lower.tail = FALSE),digits=3) #p-values
  
  return(bootresults)
  
}
