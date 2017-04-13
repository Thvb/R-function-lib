############################################
#Scatter plot function for outlier analysis#
############################################

scatterplot <- function(DV,IV,data=NULL,color,cooks=F,...){
x11()
  if(!is.null(data)){
    namesvars <- c(DV,IV)
    DV <- as.matrix(data[,DV])
    names(DV) <- row.names(data)
    IV <- as.matrix(data[,IV])
    names(IV) <- row.names(data)
    if(is.list(color)){
      colorize <- as.factor(as.matrix(data[,unlist(color)]))
    }else{
      colorize <- as.matrix(data[,color])
    }
  }else{
    stopifnot(cooks==F)
  }
  
  if(is.list(color)){
    colors <- rainbow(length(levels(colorize)))
    percol <- colorize
    levels(percol) <- colors
  }else{
    percol <- ifelse(colorize>mean(colorize,na.rm=T)+sd(colorize,na.rm=T),1,ifelse(colorize<mean(colorize,na.rm=T)-sd(colorize,na.rm=T),-1,0))
    percol[percol==0] <- "blue"
    percol[percol==1] <- "green"
    percol[percol==-1] <- "red"
  }

  
  plot(as.numeric(DV)~IV,col=percol,xlab=namesvars[2],ylab=namesvars[1])
  colorize <- colorize[!is.na(DV)&!is.na(IV)]
  mod <-lm(DV~IV)
  beta <- coef(mod)
  abline(coef=beta)
  
  if(cooks==T){
    d1 <- cooks.distance(mod)
    select <- d1 > 4/length(d1)
    r <- resid(mod)
    a <- cbind(mod$model, d1, r,colorize)
    colnames(a)[c(3,5)] <- c("Cook's distance",color)
    
    a <- a[select, ]
    posi <- ifelse(a$r<0,3,1)
    if(any(select)){
    text(a[,2],a[,1],labels=round(a[,3],2),pos=posi)
    return(a)
    }else{
      message("No outliers detected")
    }
  }
}


