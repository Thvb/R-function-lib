####
#get previous team data

getprevteamdata <- function(variables,data,grouping){
  stopifnot(variables%in%colnames(data)&is.data.frame(data)&grouping%in%colnames(data))
  cls <- match(variables,colnames(data))
  grouping <- data[,match(grouping,colnames(data))]
  j <- grouping
  z <- data.frame(matrix(NA,nrow(data),length(variables)))
  colnames(z) <- c(paste0(variables,".Previous"))
  
  for(i in 1:nrow(data)){
    if(!is.na(grouping[i])&grouping[i]%in%j){
      store <- grep(paste0("\\<",grouping[i],"\\>"), grouping)
      temp1 <- data[store,]
      k <- 2
      
      for(l in 1:(nrow(temp1)-1)){
        rw2 <- match(k, temp1$Internal.code)
        rw1 <- match(k-1, temp1$Internal.code)
        
        rwend <- store[match(k,data[store,"Internal.code"])]
        z[rwend,] <- as.vector(temp1[rw1,cls])
        k <- k+1
      }
      rm(k,rw2,rw1,store,temp1)
      j <- j[-c(grep(paste0("^",grouping[i],"$"),j))]
    }
    #cat("\r",round(i/nrow(data),2),"   |");flush.console()
  }
return(z)
}