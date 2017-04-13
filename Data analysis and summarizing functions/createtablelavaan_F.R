####Create table lavaan####
###########################

create_table_lavaan <- function(fitmodel,type=c("regression","mediation","SEM","CFA","moderatedmediation"),visualize=F){

  type <- match.arg(type,c("regression","mediation","SEM","CFA","moderatedmediation"))
  
  gettab <- function(fitmodel,type){
    stopifnot(class(fitmodel)=="lavaan")
    
    lavaan.DF <- parameterEstimates(fitmodel,rsquare=T)
    content <- tapply(lavaan.DF$lhs,lavaan.DF$op,function(x){x})
    lavaan.DF$sigcode <- sapply(lavaan.DF$pvalue,function(x){if(is.na(x)){""}else{if(x<0.01){"**"}else{if(x<0.05){"*"}else{""}}}})
    
    if(type=="mediation"|type=="regression"|type=="moderatedmediation"){
      nMods <- length(unique(content$'~'))
      nModelterms <- table(lavaan.DF$lhs[lavaan.DF$op=='~'],lavaan.DF$rhs[lavaan.DF$op=='~'])
      maxTermLength <- max(rowSums(nModelterms))
      uniqueTerms <- ncol(nModelterms)
      tab <- matrix(NA,uniqueTerms+8,nMods)
      terms <- colnames(nModelterms)[order(rank(match(colnames(nModelterms),lavaan.DF$rhs)))]
      row.names(tab) <- c("(intercept)",terms,"R-squared","chisq","df","pvalue","cfi","rmsea","rmsea.pvalue")
      
      if(type=="mediation"|type=="moderatedmediation"){
        labels <- table(lavaan.DF$label[lavaan.DF$op=='~'],lavaan.DF$rhs[lavaan.DF$op=='~'])
        labels <- labels[row.names(labels)!="",]
        
        mediators <- row.names(nModelterms)[row.names(nModelterms)%in%colnames(nModelterms)]
        temp<- cbind(labels[,colnames(labels)%in%mediators],mat.or.vec(nrow(labels),1))
        medlabels <- row.names(labels)[as.logical(rowSums(temp))]
        nMeds <- length(mediators)
        DV <- row.names(nModelterms)[!(row.names(nModelterms)%in%colnames(nModelterms))]
        stopifnot(length(DV)==1)
        ivlabels <- row.names(labels)[!(unique(row.names(labels))%in%medlabels)]
        IV <- colnames(labels)[colSums(labels[row.names(labels)%in%ivlabels,])>0]
        tab <- cbind(tab,matrix("",nrow(tab),1))
        colnames(tab) <- c(mediators,DV,"Mediation Results") 

      }
      if(type=="regression"){
        DV <- row.names(nModelterms)[!(row.names(nModelterms)%in%colnames(nModelterms))]
        IV <- colnames(nModelterms)
        mediators <- ""
        colnames(tab) <- c(DV) 
      }

      intercepts <- tapply(lavaan.DF$est[lavaan.DF$op=='~1'],lavaan.DF$lhs[lavaan.DF$op=='~1'],function(x){x})
      intercepts <- unlist(intercepts[names(intercepts)%in%c(mediators,DV)])
      it=0
      coefnames <- tapply(lavaan.DF$rhs[lavaan.DF$op=='~'],lavaan.DF$lhs[lavaan.DF$op=='~'],function(x){x})
      coefs <- lapply(tapply(lavaan.DF$est[lavaan.DF$op=='~'],lavaan.DF$lhs[lavaan.DF$op=='~'],function(x){x}),function(z){it<<-it+1; ans <- c(intercepts[it],z); names(ans) <- c("(intercept)",coefnames[[it]]);return(ans)})
      ises <- tapply(lavaan.DF$se[lavaan.DF$op=='~1'],lavaan.DF$lhs[lavaan.DF$op=='~1'],function(x){x})
      ises <- unlist(ises[names(ises)%in%c(mediators,DV)])
      it=0
      ses <- lapply(tapply(lavaan.DF$se[lavaan.DF$op=='~'],lavaan.DF$lhs[lavaan.DF$op=='~'],function(x){x}),function(z){it<<-it+1; ans <- c(ises[it],z); names(ans) <- c("(intercept)",coefnames[[it]]);return(ans)})
      isgn <- tapply(lavaan.DF$sigcode[lavaan.DF$op=='~1'],lavaan.DF$lhs[lavaan.DF$op=='~1'],function(x){x})
      isgn <- unlist(isgn[names(isgn)%in%c(mediators,DV)])
      it=0
      sgnf <- lapply(tapply(lavaan.DF$sigcode[lavaan.DF$op=='~'],lavaan.DF$lhs[lavaan.DF$op=='~'],function(x){x}),function(z){it<<-it+1; ans <- c(isgn[it],z); names(ans) <- c("(intercept)",coefnames[[it]]);return(ans)})
      rsq <- tapply(lavaan.DF$est[lavaan.DF$op=='r2'],lavaan.DF$lhs[lavaan.DF$op=='r2'],function(x){x})
      for(i in 1:ncol(tab)){
        if(i==ncol(tab)&(type=="mediation"|type=="moderatedmediation")){
          labels2 <- table(lavaan.DF$label[lavaan.DF$op==':='],lavaan.DF$rhs[lavaan.DF$op==':='])
          medres <- tapply(lavaan.DF$est[lavaan.DF$op==':='],lavaan.DF$lhs[lavaan.DF$op==':='],function(x){x})
          medres <- medres[match(names(medres),row.names(labels2))]
          medres <- medres%*%labels2
          sgnf2 <- tapply(lavaan.DF$sigcode[lavaan.DF$op==':='],lavaan.DF$lhs[lavaan.DF$op==':='],function(x){x})
          labels2logi <- labels2[match(names(sgnf2),row.names(labels2)),]>0
          sgnf2 <- apply(labels2logi,2,function(x){sgnf2[x]})
          ses2 <- tapply(lavaan.DF$se[lavaan.DF$op==':='],lavaan.DF$lhs[lavaan.DF$op==':='],function(x){x})
          ses2 <- ses2[match(names(ses2),row.names(labels2))]
          ses2 <- ses2%*%labels2
          
          if(type=="moderatedmediation"){
            mtch <- grep(paste0(medlabels,collapse="|"),colnames(medres))
            
            mtch2 <- grep(paste0(ivlabels[length(ivlabels)],collapse="|"),colnames(medres))
            
          }else{
          indirectlab <- paste0(ivlabels[1:length(medlabels)],paste0("*",medlabels))
          mtch <- match(indirectlab,colnames(medres))
          
          directlab <- ivlabels[length(ivlabels)]
          mtch2 <- grep(paste0("^",directlab,"+","."),colnames(medres))
          }
          stopifnot(all(colnames(medres)==names(sgnf2))&all(colnames(medres)==colnames(ses2)))
          indirect <- paste0(round(medres[mtch],2),sgnf2[mtch],"(m)",paste0("(",round(ses2[mtch],2),")"))
          direct <- paste0(round(medres[mtch2],2),sgnf2[mtch2],"(iv-total)",paste0("(",round(ses2[mtch2],2),")"))
          
          if(type=="moderatedmediation"){
            names(indirect) <- paste(mediators,c("control","treatment"))
            
            names(direct) <- paste(IV[2],c("control","treatment"))
            
            tab[1:2,i] <- c(names(direct)[1],direct[1])
            tab[3:4,i] <- c(names(direct)[2],direct[2])
            tab[5:6,i] <- c(names(indirect)[1],indirect[1])
            tab[7:8,i] <- c(names(indirect)[2],indirect[2])
          }else{
            names(indirect) <- mediators
            names(direct) <- IV
            
            coef <- c(direct,indirect)
            rwz <- match(names(coef),row.names(tab))
            tab[rwz,i] <- coef
          }
                    
        }else{
        cf <- paste0(round(coefs[[i]],2),sgnf[[i]],paste0("(",round(ses[[i]],2),")"))
        names(cf) <- c(names(coefs[[i]]))
        fitm <- round(unlist(fitmeasures(fitmodel,c("chisq","df","pvalue","cfi","rmsea","rmsea.pvalue"))),3)
        cf <- c(cf,"R-squared"=round(rsq[[i]],2),fitm)
        rwz <- match(names(cf),row.names(tab))
        tab[rwz,i] <- cf
        }
      }
    }
    return(tab)
  }
  
  
  if(is.list(fitmodel)){
    stopifnot(all(lapply(fitmodel,class)%in%"lavaan"))
    nIt <- length(fitmodel)
    
    tab <- list()
    for(j in 1:nIt){
      fm <- fitmodel[[j]]
      tab <- c(tab,list(gettab(fm,type)))
    }
    
  }else{
    tab <- gettab(fitmodel,type)
  }
  
  return(data.frame(tab))
}