#################
#Compatible ID's#
#################
#INPUT = dataframe or character vector with identification column containing word 'id' ; and vector of column names to exclude.
#OUTPUT = Input dataframe with replaced identification column
#MODIFICATION = ID's with a dash get the dash replaces with .underscore

compatibleID <- function(dataframe, exclude){
  stopifnot(is.character(dataframe)||is.data.frame(dataframe),is.character(exclude))
  
  cols <- grep("^id$",colnames(dataframe))
  cols <- c(cols,grep("_id$",colnames(dataframe)))
  
  for(i in exclude){
    cols<- cols[!(cols%in%grep(i,colnames(dataframe)))]
  }

  dataframe[,cols] <- apply(dataframe[,cols],2,function(x){gsub("-","._",x)})
  return(dataframe)
}