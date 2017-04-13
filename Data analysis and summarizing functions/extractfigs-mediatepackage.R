extract.figures.mediation <- function(medmod,table=c("results","conf.int")){
  require(stringr)
  table <- match.arg(table,c("results","conf.int"))
  
  results <- capture.output(summary(medmod))
  if(table=="results"){
  tab <- matrix(NA,6,1)
  row.names(tab) <- c("ACME","ADE","Total Effect","Prop. Mediated","Sample Size Used","Simulations")
  }else{
    tab <- matrix(NA,4,2)
    row.names(tab) <- c("ACME","ADE","Total Effect","Prop. Mediated")
    colnames(tab) <- c("Lower","Upper")
  }
  for(i in 1:length(row.names(tab))){
  
  row <- grep(row.names(tab)[i],results)
  medres <- unlist(str_split(results[row]," "))
  medres <- round(as.numeric(medres[!grepl("[:digits:]",medres)]),2)
  medres <- medres[!is.na(medres)]
  if(table=="results"){
    if(i>4){
      tab[i,] <- medres
    }else{
  tab[i,] <- paste0(medres[1],ifelse(medres[4]<0.05,ifelse(medres[4]<0.01,"**","*"),""))
    }
  }else{
    tab[i,] <- c(medres[2],medres[3])
  }
  }

  return(tab)
}