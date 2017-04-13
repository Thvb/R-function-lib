#insert function
insert.at <- function(a, pos, ...){
  dots <- list(...)
  stopifnot(length(dots)==length(pos))
  result <- vector("list",2*length(pos)+1)
  result[c(TRUE,FALSE)] <- split(a, as.factor(cumsum(seq_along(a) %in% (pos+1))))
  result[c(FALSE,TRUE)] <- dots
  unlist(result)
}