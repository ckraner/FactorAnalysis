### R LIBRARIES FOR EXPOLORATORY FACTOR ANALYSIS ###
################# CHRIS KRANER #####################
######### NORTHERN ILLINOIS UNIVERSITY #############
#################### 12/2016 #######################
####################################################
####################################################


#' Creates a list of values relating to Chi-Sq Tests.
#' 
#' @param x1 Value for first fit
#' @param x2 Value for second fit
#' @param df1 Degrees of freedom for first fit
#' @param df2 Degrees of freedom for second fit
#' @return List consisting of: differince in fit; difference in degrees of freedom; p-value of Chi-Sq test
#' @export
#' @examples
#' PreviousChiSq=with(PreviousFit,calcpchisq(null.model,objective,null.dof,dof))
calcpchisq=function(x1,x2,df1,df2){
  dx=x1-x2
  df=df1-df2
  chi=pchisq(dx,df)
  newList=list("dx"=dx,"df"=df,"chi"=chi)
  return(newList)
}


#' Creates a matrix out of a long list of loadings (or more generally, any list from the psych package)
#' 
#' @param FitData Data frame from statistical test.
#' @param loadings List element of data frame.
#' @param factors Number of factors.
#' @param cutpoint Value with which you would like data below which to be neglected. Taken as absolute value.
#' @return Matrix to be used as table.
#' @export
#' @examples 
#' PreFitTable7=matrixification(PreFit7,"loadings",c("Pre1","Pre2","Pre3","Pre4","Pre5","Pre6","Pre7"),.3)
matrixification <- function(FitData, loadings, factors, cutpoint) {
  nobs=nrow(FitData[[loadings[1]]])
  nfactors=length(factors)
  mydf=data.frame(blah=double(nobs))
  for(i in length(loadings)){
    if(i==1){
      mydf[[i]]=FitData[[loadings[i]]]
    }
    if(i!=1){
      mydf[[i]]=c(mydf,FitData[[loadings[i]]])
    }
  }
  z1=1
  mydf2=data.frame(name=c(1:nobs))
  for(myf in {length(names(mydf))}){
    for(l in 1:nfactors){
      if(l==1){
        mydf2[[factors[z1]]]=mydf[[myf]][1:nobs]
      }
      if (l!=1){
        mydf2[[factors[z1]]]=mydf[[myf]][{((l-1)*nobs)+1}:{l*nobs}]
      }
      z1=z1+1
    }
  }
  for(i in 1:nfactors){
    mydf2[[i+1]]=ifelse(abs(mydf2[[i+1]])>=cutpoint | abs(mydf2[[i+1]]<={-cutpoint}),mydf2[[i+1]],NA)
  }
  return(mydf2)
}



#' Creates Skree Plots and returns them as plot for later use.
#' 
#' @param data Data frame containing only the questions to be analyzed. All data must be mean-imputed.
#' @param title Custom titles.
#' @return Actual saved plot of skree plots.
#' @export
#' @examples 
#' PreScree=skreeplot(PreDUPI_I,"Skree Plot for Pre-Test \nRespondents")
skreeplot=function(data,title){
  library(grid)
  library(nFactors)
  EV=eigen(cor(data))
  ap=nFactors::parallel(subject=nrow(data),var=ncol(data),rep=100,cent=.05)
  nS=nFactors::nScree(x=EV$values,aparallel=ap$eigen$qevpea)
  nFactors::plotnScree(nS,main=title)
  Scree=grid::grid.grab()
  Scree
  Scree=recordPlot()
  return(Scree)
}
