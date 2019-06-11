#' Proportion of customers with solar
#'
#' Proportion of customers with solar in each year
#'
#' @param other.df DNSP other.df dataframe
#' @param yearslabel label of projection years from 2020
#' @param solar.in dynamic input of prop customers offgrid by 2060
#' @keywords solar, customers
#' @export
#'
#'
psolar_fun=function(other.df,yearslabel,solar.in)
{
  todaysolar=other.df$all.years[other.df$name=="solar penetration 2020"]
  solar=solar.in/100

  tmp <- matrix(NA, ncol=length(yearslabel), nrow=1)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel
  sol=tmp

  sol[1]=todaysolar
  for (i in 2:length(sol))
    sol[i]=ifelse((yearslabel[i]<41),(sol[i-1]+(solar-todaysolar)/39*1.5),
                 (ifelse((yearslabel[i]>59),solar,(sol[i-1]+(solar-todaysolar)/39*0.5))))
  sol

}

