create_table <- function(model,vcov=NULL,model.names=NULL,type=c("lm","selection")){
  require(lmtest)
  
  if(class(model)=="list"){
    stopifnot(all(lapply(model,class)%in%"lm"))
    stopifnot(is.null(vcov)|(is.list(vcov)&all(unlist(lapply(vcov,is.matrix)))))
    
    nMod = length(model)
    X <- lapply(model,function(x){x$model})
    modelterms <- unique(unlist(lapply(model,function(x){names(coef(x))})))
    maxtermLength <- length(modelterms)
    tab <- matrix(NA,maxtermLength+3,nMod)
    row.names(tab) <- c(modelterms,"R-squared","F-statistic","Wald-test")
  }else{
    stopifnot(class(model)=="lm")
    stopifnot(is.null(vcov)|is.matrix(vcov))
    
    nMod = 1
    X <- model$model[,-1]
    tab <- matrix(NA,ncol(X)+3,1)

    row.names(tab) <- c("(Intercept)",colnames(X),"R-squared","F-statistic")
  }
    
  rwz2 <- match("R-squared",row.names(tab))
  rwz3 <- match("F-statistic",row.names(tab))
  rwz4 <- match("Wald-test",row.names(tab))
  
  if(is.null(model.names)){
    colnames(tab) <- c(paste("model",1:nMod))
  }else{
    stopifnot(length(model.names)==nMod)
    colnames(tab) <- model.names
  }
  
  for(j in 1:nMod){
    if(nMod>1){
      mod <- model[[j]]
      cc <- unlist(lapply(X[[j]],class))
    }else{
      mod <- model
      cc <- unlist(lapply(X,class))
    }
    covm <- vcov(mod)
    if(!is.null(vcov)){
      mod.sum <- summary(mod)
      mod.vcov <- coeftest(mod,vcov.=covm)
      coef <- round(mod.vcov,2)
    }else{
      mod.sum <- summary(mod)
      mod.vcov <- summary(mod)
      coef <- round(coef(mod.vcov),2)
    }
    
    sigcode <- sapply(coef[,4],function(x){if(is.na(x)){""}else{if(x<0.01){"**"}else{if(x<0.05){"*"}else{""}}}})
    
    matchclass <- (cc=="factor"|cc=="character")
    matchclass <- matchclass[-1]
    if(any(matchclass)){
      name <- row.names(coef)[c(F,matchclass)]
      L <- nchar(as.character(name))
      name <- strtrim(name,width=L)
      rwnz <- row.names(coef)
      rwnz[c(F,matchclass)]<- name
      rownames(coef) <- rwnz
    }
    rwz <- match(rownames(coef), rownames(tab))
    
    tab[rwz2,j] <- round(mod.sum$r.squared,2)
    tab[rwz3,j] <- round(mod.sum$fstatistic[1],2)
    if(j>1){
      wt <- round(waldtest(mod,model[[j-1]],vcov=covm),2)
      tab[rwz4,j] <- paste(wt$'Pr(>F)'[2],"(",wt$F[2],wt$Df[2],")")
    }
    
    for(i in 1:nrow(coef)){
      tab[rwz[i],j] <- paste(coef[i,1],sigcode[i],paste0("(",coef[i,2],")"))
    }
  }
  
 return(tab)
}