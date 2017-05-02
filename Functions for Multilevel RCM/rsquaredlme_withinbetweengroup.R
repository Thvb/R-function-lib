###############################################################################
#R-squared for LME function: calculation of within and between group R-squared#
###############################################################################

#Arguments: mod = provide one or more lmer or lme mixed effects models as a list
#Requirements: method of mod should be ML not REML for lme        

r.sq.lmer <- function(mod){
  ANS <- data.frame(matrix(c("Within Group R-squared","Between Group R-squared"),2,1))
  if((class(mod)=="lmer")|class(mod)=="lme"){
    mods <- list()
    mods[[1]] <- mod
    nmods=1
  }else{
    mods <- mod
    nmods=length(mods)
  }
  
  for(i in mods){
    stopifnot((class(i)=="lmerMod")|(class(i)=="lme"))
    if(class(i)=="lmerMod"){
    
    nullform.random <- paste0(".~1+(1|",names(ranef(i)),")")
    nullform <- update.formula(i,nullform.random)
    nullmodel <- lmer(nullform,na.action=na.exclude,data=i@frame)
    
    OUTFILE <- table(as.vector(i@frame[names(i@cnms)]))
    vcov <- as.data.frame(VarCorr(nullmodel))
    tau0 <- as.numeric(vcov[1,4])
    sigma0 <- as.numeric(vcov[2,4])
    
    tau0plussigma0 <- sum(tau0,sigma0)
    samplevariance <- sd(i@frame[,1])^2
    
    vcov2 <- as.data.frame(VarCorr(i))
    tau1 <- as.numeric(vcov2[1,4])
    sigma1 <- as.numeric(vcov2[2,4])
    
    }
    if(class(i)=="lme"){
      if(i$method!="ML"){stop("Set estimation method to ML in lme function.")}
      if(is.null(i$na.action)){
        datatable <- i$data
      }else{
        datatable <- i$data[-i$na.action,]
      }
      nullform <- paste0(".~1")
      nullform <- update.formula(i,nullform)
      
      nullmodel <- lme(nullform, random=as.formula(as.character(i$call[4])), na.action=na.exclude, data=datatable, control=list(opt="optim"),method="ML")
      OUTFILE <- table(as.vector(i$groups))
      
      vcov <- VarCorr(nullmodel)
      tau0 <- as.numeric(vcov[1,1])
      sigma0 <- as.numeric(vcov[2,1])
      
      tau0plussigma0 <- sum(tau0,sigma0)
      DVchar <- grep(paste0("^",as.character(mod$terms)[2],"$"),colnames(i$data))
      DV <- datatable[,DVchar]
      samplevariance <- sd(DV)^2
      
      vcov2 <- VarCorr(i)
      tau1 <- as.numeric(vcov2[1,1])
      sigma1 <- as.numeric(vcov2[2,1])
      
    }
    
    #R-squared for mixed effects models
    
    harmean <- (1/mean(1/OUTFILE))
    tau1plussigma1 <- sum(tau1,sigma1)
    withinR <- round(1-(tau1plussigma1/tau0plussigma0),4)
    betweenR <- round(1-((sigma1/harmean)+tau1)/((sigma0/harmean)+tau0),4)
    ANS <- cbind(ANS,c(withinR,betweenR))
  }
  colnames(ANS) <- c("R-squares",paste("Model",1:nmods))
  return(ANS)
}
