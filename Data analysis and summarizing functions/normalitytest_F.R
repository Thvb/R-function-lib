#################
#normality tests#
#################

normality <- function(data,plot=F){
  require(e1071)
  
  if(is.vector(data)|is.list(data)){
    DNAME <- deparse(substitute(data))
   data <- data.frame(data) 
  }
  if(is.matrix(data)|is.data.frame(data)){
    DNAME <- colnames(data)
    data <- data.frame(data)
  }
  stopifnot(is.data.frame(data)|is.list(data))

  n <- nrow(data)
  if (is.na(n) || n < 3L || n > 5000L){
    stop("sample size must be between 3 and 5000")
  }
  
  ans <- lapply(data,function(x){
    q <- data.frame(Skewness=skewness(x),Kurtosis=kurtosis(x),ShapiroStat.=shapiro.test(x)$statistic,ShapiroPval=shapiro.test(x)$p.value);
    row.names(q) <- "";
    return(q)
  })

  names(ans) <- DNAME
  
  if(plot==T){
    data <- as.matrix(data)
    dat <- list(density(data),data)
  if(ncol(data)==1){
    if(interactive()){
    plots <- list("plot","qqnorm")
    for(i in 1:2){
      eval(call(plots[[i]],dat[[i]]))
      if(i==1){
      ANSWER <- readline("Hit any button to go to next plot.")
      }
    }
    }else{
      message("Interactive session is required.")
    }
  }else{
    message("More than one variable supplied. Argument plot is ignored.")
  }
  }
  
  return(ans)
}