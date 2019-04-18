#' Real RAB end period function
#'
#' Formula for calculating the real residual RAB (end period)
#'
#' Real residual RAB (end period) for each year. PTRM excel assets row 466.
#' real RAB = prevyearRAB - deprec + realcapex. Equal to the PTEM excel sheet assets row 466
#'
#' Returns a vector of rRABend for each year.
#'
#' Prerequisites that need to be defined in ptrm model (i.e. run above code):
#' noyears,RABstart,deprec,realcapexall
#'
#' @param realcapexall vector of sum of capex over all asset categories for each year (realcapex_fun)
#' @param noyears count of projection years
#' @param deprec sum of the regdeprec.all across all years for each asset class (WARL or yoy)
#' @param RABstart opening RAB PTRM input (oav+auc)
#' @param startyearend first year of model: 19 or 2019
#' @param projyearend end year of projection

#' @keywords RAB, real RAB, deprec, realcapex
#' @export
#'
rRABend_fun=function(noyears,RABstart,deprec,realcapexall,startyearend,projyearend)
{
  y=(1:(noyears+1))
  if(length(y)<2){y=RABstart}else{# pardon the case where x is of length 1 or 0
    for(i in 1:length(y))
      if(y[i]<2){y[i]=RABstart}else{
        (y[i]=(y[i-1]-deprec[(y[i])-1]+realcapexall[(y[i])-1]))}
  }
  label=(startyearend:projyearend)
  df=as.data.frame(matrix(NA,ncol=length(label),nrow=1))
  names(df)=label
  df[1,]=y
  y=df
  y
}


