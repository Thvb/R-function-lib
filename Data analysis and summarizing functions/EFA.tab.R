EFA.table <- function(EFA.mod,factor.names=NULL,trim=.2){
  
  tab <- as.matrix(EFA.mod$promax.loadings)
  tab[abs(tab)<trim] <- ""
  varexp <- EFA.mod$promax.SS[3,]
  intercortab <- EFA.mod$corr.factors
  tab <- rbind(tab,rbind(varexp,intercortab),mat.or.vec(1,ncol(tab)))
  
  if(!is.null(factor.names)){
    colnames(tab) <- c(factor.names)
    intercor <- paste("Interfactor correlation -",colnames(tab))
  }else{
    colnames(tab) <- c(paste("Factor",1:ncol(tab)))
    intercor <- paste("Interfactor correlation - Factor",1:ncol(tab))
  }

  row.names(tab)[(nrow(EFA.mod$promax.loadings)+1):nrow(tab)] <- c("Variance explained",intercor,"n =")
  tab[nrow(tab),1] <- EFA.mod$n
  
  return(tab)
  }