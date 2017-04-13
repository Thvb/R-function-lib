writetodata <- function(what,name,data,na.rm=T,bind=F,subset=NULL,IDvec,loop=NULL){
  if(bind==F){
    if(is.na(match(name,colnames(data)))){
      data <- cbind(data,array(NA,dim=nrow(data)))
      colnames(data)[ncol(data)] <- name
    }
    if(na.rm==T){
      what <- what[!is.na(what)]
    }
    if(is.null(loop)){
      if(!is.null(subset)){
        idz <- data[subset,IDvec] 
        what <- what[names(what)%in%idz]
      }else{
        what <- what[names(what)%in%data[,IDvec]]
      }
      if(length(what)>=nrow(data)){
        rwz <- match(names(what),data[,IDvec])
        rwz <- rwz[!is.na(rwz)]
      }else{
        rwz <- match(data[,IDvec],names(what))
      }
    }else{
      rwz <- loop
    }
    if(length(what)==0){
      what <- NA
    }
    if((length(what)>=nrow(data))||!is.null(loop)){
      data[rwz,name] <- what
    }else{
      data[,name] <- what[rwz]
    }
    return(data)
  }else{
    what <- what[rownames(what)%in%data[,IDvec],]
    whatmat <- data.frame(matrix(NA,nrow(data),ncol(what)))
    colnames(whatmat) <- name
    if(nrow(what)>=nrow(data)){
      rwz <- match(rownames(what),data[,IDvec])
      rwz <- rwz[!is.na(rwz)]
      whatmat[rwz,] <- what
    }else{
      rwz <- match(data[,IDvec],rownames(what))
      whatmat <- what[rwz,]
    }
    data <- cbind(data,whatmat)
    return(data)
  }
}