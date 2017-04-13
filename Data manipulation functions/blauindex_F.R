###
#Blau index

blau <- function(vect){
  stopifnot(is.vector(vect)|nrow(vect)==1)
  vect <- vect[!is.na(vect)]
  answer <- 1-sum((table(vect)/length(vect))^2)
  return(answer)
}