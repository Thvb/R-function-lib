#scale variable to treatment function
scalevariablestotreatment <- function(data,variable){
  cl1 <- grep(paste0("^",paste0(variable,"$")),colnames(data))
  q1 <- data[,cl1]
  q1[data$treatment==0] <- scale(q1[data$treatment==0])
  q1[data$treatment==1] <- scale(q1[data$treatment==1])
  return(q1)
}