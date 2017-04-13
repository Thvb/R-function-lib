############################################
#Replace character and structure references#
############################################
#INPUT = Text - character vector or factor vector
#OUTPUT= Text - input vector, manipulated
#MANIPULATION = replace common XML/HTML character references

replace.char.refs <- function(text){
  stopifnot(is.character(text)||is.factor(text))
  
  text <- gsub("&lt;","\\<",text)
  text <- gsub("&gt;","\\>",text)
  text <- gsub("&amp;","\\&",text)
  text <- gsub("&apos;","\\'",text)
  text <- gsub("&rsquo;","\\'",text)
  text <- gsub("&lsquo;","\\'",text)
  text <- gsub("&quot;",'\\"',text)
  text <- gsub("&ldquo;",'\\"',text)
  text <- gsub("&rdquo;",'\\"',text)
  text <- gsub("&nbsp;"," ",text)
  text <- gsub("&ndash;","-",text)
  text <- gsub("<p>","",text)
  text <- gsub("</p>","",text)
  text <- gsub("\\\n","",text)
  
  return(text)
}
