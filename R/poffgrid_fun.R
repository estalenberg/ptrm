#' Proportion of offgrid customers function
#'
#' Proportion of customers offgrid in each year
#'
#' @param other.df DNSP other.df dataframe
#' @param yearslabel label of projection years from 2020
#' @param ogrid.in dynamic input of prop customers offgrid by 2060
#' @keywords offgrid
#' @export
#'
#'
poffgrid_fun=function(other.df,yearslabel,ogrid.in)
{
  todayogrid=other.df$all.years[other.df$name=="offgrid cust 2020"]
  ogrid=ogrid.in/100

  tmp <- matrix(NA, ncol=length(yearslabel), nrow=1)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel
  og=tmp

  og[1]=todayogrid
  for (i in 2:length(og))
    og[i]=ifelse((yearslabel[i]<41),(og[i-1]+(ogrid-todayogrid)/39*0.5),
                 (ifelse((yearslabel[i]>59),ogrid,(og[i-1]+(ogrid-todayogrid)/39*1.5))))
  og

}

