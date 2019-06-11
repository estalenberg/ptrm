#' Proportion of customers with batteries
#'
#' Proportion of customers with batters in each year
#'
#' @param other.df DNSP other.df dataframe
#' @param yearslabel label of projection years from 2020
#' @param batt.in dynamic input of prop customers offgrid by 2060
#' @keywords solar, customers
#' @export
#'
#'
pbatt_fun=function(other.df,yearslabel,batt.in)
{
  todaybatt=other.df$all.years[other.df$name=="battery 2020"]
  batt=batt.in/100

  tmp <- matrix(NA, ncol=length(yearslabel), nrow=1)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel
  bt=tmp

  bt[1]=todaybatt
  for (i in 2:length(bt))
    bt[i]=ifelse((yearslabel[i]<41),(bt[i-1]+(batt-todaybatt)/39*1.5),
                 (ifelse((yearslabel[i]>59),batt,(bt[i-1]+(batt-todaybatt)/39*0.5))))
  bt

}

