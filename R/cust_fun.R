#' Customer numbers function
#'
#' Formula for calculating the number of customers in each dnsp
#'
#'
#' @param yearslabel label of projection years from 2020
#' @param other.df dataset of DNSP static vectors
#' @param cust.in dynamic variable of customer growth percentage
#'
#' @export
#'
cust_fun=function(yearslabel,other.df,cust.in){

  custnum= other.df[which(other.df$name=="cust numbers"),grep("^1$",colnames(other.df)):grep("^5$",colnames(other.df))] #just first 5 years 2020 to 2024
  #custgrowth= other.df[which(other.df$name=="cust growth rate"),grep("^all.years$",colnames(other.df))]
  custgrowth=cust.in/100

    tmp <- matrix(NA, ncol=length(yearslabel), nrow=1)
    tmp=as.data.frame(tmp)
    names(tmp)=yearslabel
    cust=tmp

    for(i in 1:5)
      cust[i]=custnum[i]
    for(i in 6:length(cust))
      cust[i]=round((cust[i-1]*(1+custgrowth)),digits=0)

    return(cust)
  }

