#' Rate of equity function
#'
#' Varying rate of equity for each year
#'
#' Rate of equity is static in ptrm by with new rba cash we need to vary it
#'
#' Prerequisites that need to be defined in ptrm model (i.e. run above code):
#' year, rba, startyearend, projyearend
#'
#' @param other.df DNSP other.df dataframe
#' @param yearslabel label of projection years from 2020
#' @param rba dynamic input of rba cash rate
#' @keywords rba, debt, return on debt
#' @export
#'
#'
requity_fun=function(other.df,yearslabel,rba)
{
  equitystart=other.df$all.years[other.df$name=="Return on Equity"]
  todayrba=other.df$all.years[other.df$name=="rba today"]

  tmp <- matrix(NA, ncol=length(yearslabel), nrow=1)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel
  re=tmp

  for(i in 1:5)
    re[i]=((equitystart)) #make each year decrease by debt reduct until year 5
  for (i in 6:length(re))
    re[i]=ifelse((yearslabel[i]<40),(re[i-1]+((rba-todayrba)/15)),(re[5]+(rba-todayrba)))
  re

}

