DirSource2 <- function (directory = ".", encoding = "", pattern = NULL, recursive = FALSE, 
          ignore.case = FALSE, mode = "text") 
{
  if (!identical(mode, "text") && !identical(mode, "binary") && 
        !identical(mode, "")) 
    stop(sprintf("invalid mode '%s'", mode))
  d <- dir(directory, full.names = TRUE, pattern = pattern, 
           recursive = recursive, ignore.case = ignore.case)
  if (!length(d)){
    message(paste("empty directory for search pattern",pattern))
    return(NULL)
  }
  isfile <- !file.info(d)[["isdir"]]
  if (any(is.na(isfile))) 
    stop("non-existent or non-readable file(s): ", paste(d[is.na(isfile)], 
                                                         collapse = " "))
  SimpleSource(encoding = encoding, length = sum(isfile), mode = mode, 
               filelist = d[isfile], class = "DirSource")

}