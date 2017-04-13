reshape_corpus <- function(current.corpus, FUN, ...) {
  # Extract the text from each document in the corpus and put into a list
  text <- lapply(current.corpus, content)
  
  # Basically convert the text
  docs <- lapply(text, FUN)
  docs <- lapply(docs,function(x){str_replace_all(x, "[\r\n]" , " ")})

    ##store meta-data
    lengthdocs <- unlist(lapply(docs,length))
    #newcorpmeta <- current.corpus$meta
    olddocmeta <- lapply(current.corpus,meta)
    uniquetags <- unique(unlist(lapply(olddocmeta,names)))
    newdocmeta <- sapply(uniquetags,function(z){lapply(olddocmeta,function(x){
      if(z=="datetimestamp"){
        if(all(is.na(x[[z]]))){
        NA
        }else{
          strftime(x[[z]])
          }
    }else{
      x[[z]]
      }
      })
    })
    if(length(olddocmeta)==1){
      names(newdocmeta) <- uniquetags
      newdocmeta <- sapply(newdocmeta,function(x){rep(x,lengthdocs)})   
    }else{
    newdocmeta <- apply(newdocmeta,2,function(x){rep(x,lengthdocs)})
    }
    stopifnot(names(newdocmeta)==uniquetags)
  
  docs <- as.vector(unlist(docs))
  
  # Create a new corpus structure and return it
  new.corpus <- Corpus(VectorSource(docs))
  
  #for(it in 1:length(newcorpmeta)){
  #  metadata <- newcorpmeta[[it]]
   # z=0
   # new.corpus = tm_map(new.corpus, function(x) {
   #   z <<- z+1
   #   meta(x,tag=names(newcorpmeta)[it],type="indexed")<-metadata[[z]]
   #   x
   # })
   # }
  
  
  for(it in 1:length(uniquetags)){
      metadata <- newdocmeta[[it]]
    z=0
    new.corpus = tm_map(new.corpus, function(x) {
      z <<- z+1
      meta(x,tag=uniquetags[it],type="indexed")<-metadata[z]
    
      x
    })
  }
  return(new.corpus)
}