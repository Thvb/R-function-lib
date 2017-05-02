dissim <- function(vect){
  stopifnot(is.vector(vect)|nrow(vect)==1,is.numeric(vect))
  vect <- vect[!is.na(vect)]
  
  answer <- array(NA,length(vect))
  for(i in 1:length(vect)){
    answer[i] <- sqrt(sum(c(vect[i]-vect)^2)/(length(vect)))
  }
  return(answer)
}
