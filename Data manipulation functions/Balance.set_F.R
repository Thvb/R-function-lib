balanced.subset <- function(dat,incl,balancevars,extra.N,control="once"){
  stopifnot(length(incl)==nrow(dat),is.logical(incl))
  ANS <- matrix(NA,length(balancevars)+1,8)
  rownames(ANS) <- c("N",balancevars)
  colnames(ANS) <- c("Current Incl.","Current Excl.","Test stat.","p.val","Plus New Incl.","Min New Excl.","Test stat.","p.val")
  control <- match.arg(control,c("once","optim"))
  
  calc.stat.write <- function(ansmat,balancevars,dat,incl){
    for(i in 1:length(balancevars)){
      var <- dat[,balancevars[i]]
      if(is.numeric(var)&nrow(table(var))>2){
        balance.test <- t.test(var[incl],var[!incl])
        ansmat[i+1,j] <- round(balance.test$estimate[1],3)
        ansmat[i+1,j+1] <- round(balance.test$estimate[2],3)
        ansmat[i+1,j+2] <- round(balance.test$statistic,3)
        ansmat[i+1,j+3] <- round(balance.test$p.value,3)
      }else{
        vartab <- table(incl,var)
        balance.test <- chisq.test(vartab)
        ansmat[i+1,j] <- paste0(round(balance.test$observed[2,2]/sum(balance.test$observed[2,]),3)*100,"%")
        ansmat[i+1,j+1] <- paste0(round(balance.test$observed[1,2]/sum(balance.test$observed[1,]),3)*100,"%")
        ansmat[i+1,j+2] <- round(balance.test$statistic,3)
        ansmat[i+1,j+3] <- round(balance.test$p.value,3)
      }
    }
    return(ansmat)
  }
  
  for(j in c(1,5)){
    if(j==5){
      if(control=="optim"){
        balance<-0
        it <- 0
        incl.orig=incl
        ANS.prev <- ANS
      while(balance==0&it<500){
        incl=incl.orig
        extraobs <- as.numeric(sample(which(!incl),extra.N))
        incl[extraobs] <- TRUE 
        ANS[1,j] <- table(incl)[2]
        ANS[1,j+1] <- table(incl)[1]
        ANS[1,j+2] <- ""
        ANS[1,j+3] <- ""
        ANS <- calc.stat.write(ANS,balancevars,dat,incl)
        if(all(as.numeric(ANS[,8])>.1,na.rm=T)){
          balance<-1
        }else{
          if(sum(as.numeric(ANS.prev[,8])<0.1,na.rm=T)>sum(as.numeric(ANS[,8])<0.1,na.rm=T)){
            ANS.prev <- ANS
          }else{
            if(sum(as.numeric(ANS.prev[,8]),na.rm=T)<sum(as.numeric(ANS[,8]),na.rm=T)){
              ANS.prev <- ANS
            }
          }
        }
        it <- it+1
      }
        if(it==500){
          ANS <- ANS.prev
        }
      }else{
        extraobs <- as.numeric(sample(which(!incl),extra.N))
        incl[extraobs] <- TRUE 
        ANS[1,j] <- table(incl)[2]
        ANS[1,j+1] <- table(incl)[1]
        ANS[1,j+2] <- ""
        ANS[1,j+3] <- ""
        ANS <- calc.stat.write(ANS,balancevars,dat,incl)
      }
    }else{
      ANS[1,j] <- table(incl)[2]
      ANS[1,j+1] <- table(incl)[1]
      ANS[1,j+2] <- ""
      ANS[1,j+3] <- ""
      ANS <- calc.stat.write(ANS,balancevars,dat,incl)
    }

  }
  ANSreturn <- list()
  ANSreturn$result <- ANS
  print(ANS)
  ANSreturn$data <- dat[extraobs,]
  return(ANSreturn)
}