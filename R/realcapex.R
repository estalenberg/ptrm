#' Real net capex function
#'
#' Formula for sum of real net capex in each year across all asset classes
#'
#' Returns a vector of sumcapex for all years. Equal to excel PTRM assets row 10.
#' Sum of the half vanilla WACC of the netcapex of all assets in each year
#'
#' Prerequisites that need to be defined in ptrm model (i.e. run above code):
#' fcnetavg.full: all years netcapex dataframe, years, noassets, fcnetavg.full, rvanilla, yearslabel

#' @param fcnetavg.full dataframe of all projected netcapex for dnsp (from netcapex_fun)
#' @param yearslabel from 2020 to projected year end
#' @param noyears count of number of projected years
#' @param noassets count of asset classes
#' @param rvanilla real vanilla WACC vector for all years
#' @keywords netcapex, capex, dnsp, asset class, asset class type, vanilla WACC
#' @export
#'


realcapex_fun=function(noyears,noassets,fcnetavg.full,rvanilla,yearslabel){
  years=1:noyears
  res <- matrix(NA, nrow=noassets, ncol=length(years))
  res=as.data.frame(res)
  names(res)=yearslabel

  for (j in years) {
    for(i in 1:noassets){
      assetclass=i
      res[i,j]=(fcnetavg.full[i,j]*(1+rvanilla[,j])^0.5)}
  }

  realcapex.df=res
  realcapex= colSums (realcapex.df, na.rm = T,dims=1)
  realcapexall=realcapex

  return(realcapexall) }

