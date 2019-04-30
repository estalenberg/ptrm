#' Customer numbers function
#'
#' Formula for calculating the number of customers in each dnsp
#'
#'
#' @param yearslabel label of projection years from 2020
#' @param custnum vector of customer numbers from cust_fun
#'
#' @export
#'
newcust_fun=function(yearslabel,custnum){


  tmp <- matrix(NA, ncol=length(yearslabel-1), nrow=1)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel
  newcust=tmp

  for(i in 1)
    newcust[i]=0
  for(i in 2:length(newcust))
    newcust[i]=(custnum[i]-custnum[i-1])

    return(newcust)
  }

