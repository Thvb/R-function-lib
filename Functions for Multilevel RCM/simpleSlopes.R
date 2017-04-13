simpleSlopes <- function(model,IV,moderator,SD=1,data=NULL,VCOV=NULL,selection.type="outcome") {
  #use the standardized coefficients
  #model: fitted model
  #SD: how many standard deviations
  #gives the p-values of the simple slope tests
  require(Hmisc)
  
  if(is.null(data)){
    data <- model.frame(model)
  }
  if(is.null(VCOV)){
    VCOV <- vcov(model)
  }
  
  Mod<-rep(NA, 3)
  simpleIntercept<-rep(NA, 3)
  simpleSlope<-rep(NA, 3)
  SE<- rep(NA,3)
  tVal<-rep(NA, 3)
  pval<-rep(NA, 3)
  
  pos <- pmatch(selection.type,c("selection","outcome"))
  
  if(class(model)[1]=="selection"){
    INT <- names(coefficients(model))[grep(":",names(coefficients(model)))[pos]]
  }else{
    if(class(model)[1]=="outcome"){
    pos = pos-1
    }else{
      pos=1
    }
    INT<- tail(names(coefficients(model)),n=1) #name of the interation
  }
  
  stdDev<-SD*sd(data[,moderator],na.rm = TRUE) 
  Mod[2] <- mean(data[,moderator],na.rm = TRUE) #average effect
  Mod[1]<-Mod[2]+ stdDev #high moderator
  Mod[3]<-Mod[2]-stdDev #low moderator
  
  IVmatch <- grep(paste0("^",escapeRegex(IV),"$"),names(coefficients(model)))[pos]
  INTmatch <- grep(paste0("^",escapeRegex(INT),"$"),names(coefficients(model)))[pos]
 
  interceptmatch <- grep(paste0("^",escapeRegex("(Intercept)"),"$"),names(coefficients(model)))[pos]
  moderatormatch <- grep(paste0("^",escapeRegex(moderator),"$"),names(coefficients(model)))[pos]
  
  if(class(model)=="lme"){
    coefs <- coefficients(model)[sample(1:nrow(coef(model)),1),]
  }else{
    coefs <- coefficients(model)
  }
  simpleIntercept[1]=coefs[interceptmatch]+coefs[moderatormatch]*Mod[1]
  simpleIntercept[2]=coefs[interceptmatch]+coefs[moderatormatch]*Mod[2]
  simpleIntercept[3]=coefs[interceptmatch]+coefs[moderatormatch]*Mod[3]
  
  simpleSlope[1]=coefs[IVmatch]+coefs[INTmatch]*Mod[1]
  simpleSlope[2]=coefs[IVmatch]+coefs[INTmatch]*Mod[2]
  simpleSlope[3]=coefs[IVmatch]+coefs[INTmatch]*Mod[3]
  
  simpleIntercept <- unlist(simpleIntercept)
  simpleSlope <- unlist(simpleSlope)
  COVmatrix<-VCOV
  
  #calculate the standard errors
  #formula: SE<-sqrt(varIV+stdDev*stdDev*varINTERACTION+2*stdDev*covarianceIV.MOD)
  IVcolmatch <- grep(paste0("^",escapeRegex(IV),"$"),colnames(COVmatrix))[pos]
  IVrowmatch <- grep(paste0("^",escapeRegex(IV),"$"),row.names(COVmatrix))[pos]
  INTcolmatch <- grep(paste0("^",escapeRegex(INT),"$"),colnames(COVmatrix))[pos]
  INTrowmatch <- grep(paste0("^",escapeRegex(INT),"$"),row.names(COVmatrix))[pos]
  
  SE[1]<-sqrt(COVmatrix[IVrowmatch,IVcolmatch]+2*COVmatrix[IVrowmatch,INTcolmatch]*Mod[1]+COVmatrix[INTrowmatch,INTcolmatch]*Mod[1]^2)
  SE[2]<-sqrt(COVmatrix[IVrowmatch,IVcolmatch]+2*COVmatrix[IVrowmatch,INTcolmatch]*Mod[2]+COVmatrix[INTrowmatch,INTcolmatch]*Mod[2]^2)
  SE[3]<-sqrt(COVmatrix[IVrowmatch,IVcolmatch]+2*COVmatrix[IVrowmatch,INTcolmatch]*Mod[3]+COVmatrix[INTrowmatch,INTcolmatch]*Mod[3]^2)
  
  if(class(model)[1]=="selection"){
    
    df <- model$param$df
  }else{
    if(class(model)=="lme"){
      df <- model$fixDF$X[INTmatch]
    }else{
    df <- df.residual(model)
    }
  }
  
  for(i in 1:length(simpleSlope)){
    stopifnot(length(simpleSlope)==length(SE))
    tVal[i]<- simpleSlope[i]/SE[i]
    #calculate the t value
    
    pval[i]<-2*pt(-abs(tVal[i]),df=(df)) #calculate the p value
  }
  return(cbind(simpleSlope, SE, pval,c(paste0("+",SD," S.D."),"Av.",paste0("-",SD," S.D."))))
  
}