dummy.multifactor <- function(vect,sep,colname=NULL,FUN=NULL,...){
  require(stringr)
  stopifnot(is.character(vect)||is.factor(vect))
  dots <- list(...)
  splitted <- sapply(vect,function(x){temp <- str_split(x,sep)})
  if(is.null(FUN)){
    funplied <- splitted
  }else{
    funplied <- lapply(splitted,FUN)
  }
  
  uniq <- unique(unlist(funplied))
  uniq <- uniq[!(uniq%in%c(""))]
  uniq <- uniq[!is.na(uniq)]
  ANS <- data.frame(t(as.data.frame(lapply(funplied,function(z){sapply(uniq,function(x){any(grepl(x,z))})}))))
  if(is.null(colname)){
    colname <- "Dummy."
  }
  colnames(ANS) <- paste0(colname,colnames(ANS))
  return(ANS)
}