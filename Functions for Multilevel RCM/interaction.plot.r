plot.interaction <- function(model,IV,mod,control=NULL,legendpos="bottomright",frame=F,new.window=F,mod.level=NULL,xsd=NULL,IVrange=NULL,data=NULL){
  require(stringr)
  if(!exists("predict.lme.fixed")){
    message("loading fixed lme predict function from working directory")
    source("predictLMEfixed.R")
  }
  if(new.window==T){
    x11()
  }
  if(!is.null(subset)){
    dat <- data
  }else{
  if(class(model)=="lmerMod"|as.character(class(model))=="merModLmerTest"){
    dat <- model@frame
  }else{
    if(class(model)=="lme"){
      
      rmv<-model$na.action
      if(!is.null(rmv)){
      dat <- model$data[-rmv,]
      }else{
        dat <- model$data
      }
    }else{
      dat <- model$model
    }
  }
}
  clIV <- match(IV,colnames(dat))
  clmod <- match(mod,colnames(dat))
  
  if(is.null(xsd)){
    xsd <- 1
  }
if(is.factor(dat[,clIV])){
  iv = levels(dat[,clIV])
                                                               
}else{
  if(!is.null(IVrange)){
    iv = IVrange
  }else{
    iv = seq(min(dat[,clIV]),max(dat[,clIV]),length.out=20)
    }
}

if(is.factor(dat[,clmod])){
  MOD = levels(dat[,mod])
}else{
  if(is.null(mod.level)){
    MOD = c(mean(dat[,clmod],na.rm=T)-xsd*sd(dat[,clmod],na.rm=T),mean(dat[,clmod],na.rm=T)+xsd*sd(dat[,clmod],na.rm=T))
    }else{
    MOD = c(seq(min(dat[,clmod]),max(dat[,clmod]),length.out=20))
  }
}
  newdat <- expand.grid(IV=iv,mod=MOD)
  colnames(newdat) <- c(IV,mod)

if(!is.null(control)){
  clcont <- match(control,colnames(dat))
  controlvars <- data.frame(lapply(clcont,function(x){if(is.factor(dat[,x])){rep(levels(dat[,x])[2],nrow(newdat))}else{rep(mean(as.numeric(dat[,x]),na.rm=T),nrow(newdat))}}))
  newdat <- cbind(newdat,controlvars)
  colnames(newdat) <- c(IV,mod,control)

}else{
  if(class(model)=="lme"|class(model)=="lm"){
    contrchars <- str_trim(unlist(str_split(as.character(model$terms)[3],"\\+|\\*")),"both")
    IVchar <- c(IV,mod)
    contrchars <- contrchars[-grep(paste0(IVchar,collapse="|"),contrchars)]
    if(length(contrchars)!=0){
      clcont <- match(contrchars,colnames(dat))
      controlvars <-   data.frame(lapply(clcont,function(x){if(is.factor(dat[,x])){rep(levels(dat[,x])[median(length(levels(dat[,x])))],nrow(newdat))}else{rep(mean(as.numeric(dat[,x]),na.rm=T),nrow(newdat))}}))
      newdat <- cbind(newdat,controlvars)
      colnames(newdat) <- c(IV,mod,contrchars)
    }
  }else{
  controlvars <- mat.or.vec(nrow(newdat),0)
  colnames(newdat) <- c(IV,mod)
  }
}

if(class(model)=="lmerMod"){
  newdat$fit <- predict(model,newdata = newdat,re.form=NA)
}else{
  if(class(model)=="lme"){
    newdat$fit <- predict.lme.fixed(model,newdata = newdat,level=0)
  }else{
    newdat$fit <- predict(model,newdata = newdat)
  }

}
plot(newdat$fit~as.numeric(newdat[,IV]),xlab=IV,ylab="DV",type="n")
if(is.factor(dat[,mod])){
  if(length(levels(dat[,mod]))<3){
    mtch1 <- levels(dat[,mod])[1]
    mtch2 <- levels(dat[,mod])[2]
  }
}else{
  mtch1 <- min(newdat[,mod])
  mtch2 <-  max(newdat[,mod])
}
lines(newdat[newdat[,mod]==mtch1,IV],c(newdat$fit[newdat[,mod]==mtch1]),lty = 1)
lines(newdat[newdat[,mod]==mtch2,IV],c(newdat$fit[newdat[,mod]==mtch2]),lty = 2)

if(is.factor(dat[,mod])){
  legend(legendpos,levels(dat[,mod]),lty=c(1,2),title=mod)
}else{
  legend(legendpos,c(paste0("-",xsd," SD"),paste0("+",xsd," SD")),lty=c(1,2),title=mod)
}
if(frame==T){
return(newdat)
}
}