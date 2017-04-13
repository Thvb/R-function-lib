cortable.format <- function(cordata,xtable=T,rem=NULL){
  require(Hmisc)
  if(xtable==T){
    require(xtable)
  }
  cordata <- data.frame(lapply(cordata,function(x){
    if(is.factor(x)||is.character(x)){
      x <- as.factor(x)
      if(length(unique(x))>2){
        uniq <- unique(x)
        x <- data.frame(sapply(uniq,function(z){grepl(z,x)}))
      }else{
        x <- as.numeric(x)-1
      }
    }
    return(x)
  }))
b <- as.matrix(cordata)
z <- rcorr(b)

cortab <- round(z[[1]],2)
if(!is.null(rem)){
   cortab[abs(cortab)<rem] <- "-"
}
cortab[cortab==1] <- "-"
tabcustom2 <- matrix("-",nrow(cortab),ncol(cortab))
row.names(tabcustom2) <- paste0(1:nrow(cortab),".",paste(row.names(cortab)))
colnames(tabcustom2) <- 1:ncol(cortab)
sigcode <- apply(z[[3]],2,function(x){sapply(x,function(z){if(is.na(z)){""}else{if(z<=0.01){"**"}else{if(z<=0.05){"*"}else{""}}}})})
for(i in 1:nrow(tabcustom2)){
  for(j in 1:ncol(tabcustom2)){
    tabcustom2[i,j] <- paste0(cortab[i,j],sigcode[i,j])
  }
}
tabcustom2[upper.tri(tabcustom2)] <- "-"

corextend <- apply(b,2,as.numeric)
corextend <- t(round(apply(corextend,2,function(x){c(mean(x,na.rm=T),sd(x,na.rm=T))}),2))
colnames(corextend) <- c("Mean","s.d.")
tabcustom2 <- cbind(corextend,tabcustom2)
tabcustom2 <- gsub("^0.",".",tabcustom2)
tabcustom2 <- gsub("^-0.","-.",tabcustom2)
if(xtable==T){
cortab <- xtable(tabcustom2)
return(cortab)
}else{
  return(tabcustom2)
}
}