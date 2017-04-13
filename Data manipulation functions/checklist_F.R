checklist <- function(remlist, matchingdata,checkname){
  
  tab <- table(matchingdata)
  match <- sapply(remlist,function(x){
    tab[grepl(x,names(tab))]
  })
  
  cat(paste("Current matches found for", checkname))
      print(unlist(match))
      ans=NA
      while(is.na(ans)){
        ans <- readline(prompt="Continue?")
        if(!(ans%in%c("Y","y","N","n","Yes","yes","No","no","YES","NO"))){
          ans = NA
          message("Please enter Yes or No!")
        }
      }
      if(grepl("n",ans)){
        stop(paste("Quiting data processing, please edit",checkname))
      }
  uniqlist <- unique(matchingdata)
  uniqlist <- uniqlist[order(str_length(uniqlist))]
  for(nr in 2:51){
  if((length(uniqlist)%%nr)==0){
    cl <- nr
    break
  }
  }
  View(matrix(uniqlist,nrow=length(uniqlist)/cl,ncol=cl,byrow = T))
  ans = NA
  while(is.na(ans)){
    ans <- readline(prompt="Continue with data processing?")
    if(!(ans%in%c("Y","y","N","n","Yes","yes","No","no","YES","NO"))){
      ans = NA
      message("Please enter Yes or No!")
    }
    } 
if(grepl("n",ans)){
  stop(paste("Quiting data processing, please edit",checkname))
}
}