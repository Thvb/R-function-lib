####################
#Sentiment function#
####################
#http://www.r-bloggers.com/twitter-sentiment-analysis-with-r/#

score.sentiment <- function(sentences, ...,score.struct=NULL, color.text=F,.progress='none')
{
  dots <- list(...)
  ndots <- length(dots)
  if(is.null(names(dots))){
    names(dots) <- paste("Sentiment match",1:ndots)
  }
  #stuff
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, dots){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('\\d+', "", sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    
    matches <- as.data.frame(lapply(dots,function(x){
      temp <- match(words, x)  
      temp <- !is.na(temp)
      return(sum(temp))
    }))
    
    for(i in colnames(matches)){
    assign(i,matches[,i])
    }
    if(!is.null(score.struct)){
      scorez <- eval(parse(text=score.struct))
      ANS <- c(scorez,apply(matches,2,sum))
      names(ANS) <- c("score",names(dots))
    }else{
      ANS <- c(apply(matches,2,sum))
      names(ANS) <- c(names(dots))
    }
    return(ANS)
  }, dots, .progress=.progress)
  scores.df <- data.frame(scores, text=sentences)
  
  return(scores.df)
}

