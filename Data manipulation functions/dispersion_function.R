#Dispersion function

dispersion <- function(array){
  stopifnot(is.array(array)|is.vector(array))
  data <- array
    if(sum(data)!=0&all(!is.na(data))){
      if(any(data==0)){
		    remv <- data!=0
        q <- data[remv]
        ANS <- as.numeric(sum((q/sum(q))*log(1/(q/sum(q)))))
      }else{
        z <- data
        ANS <- as.numeric(sum((z/sum(z))*log(1/(z/sum(z)))))
      }
    }else{
      stop
      message("There are missing values in array and/or categories contain no information")
    }
  ANS <- as.numeric(ANS)
  return(ANS)
}
