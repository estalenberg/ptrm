#real net capex ----
#G10=real net capital expenditure =SUM(G11:G40) = real capex of all asset classes for each year
#G11='PTRM input'!G143*(1+rvanilla01)^0.5 #vanilla goes up with the capex stage
#G11=fcnetassets*(1+rvanilla01)^0.5 -  per asset class - so g11-g40 are all the 30 asset classes with this formula
#fcnetcapex.df[assetclass,capexstage]*(1+rvanilla[capexstage])^0.5


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
#' @param years number of all years from 1
#' @param noassets count of asset classes
#' @param rvanilla real vanilla WACC vector for all years
#' @keywords netcapex, capex, dnsp, asset class, asset class type, vanilla WACC
#' @export
#' @examples
#' projyearend = 97
#' startyearend = 19
#' years = seq(1:(projyearend-startyearend))
#' noassets = 22
#'


realcapex_fun=function(years,noassets,fcnetavg.full,rvanilla,yearslabel){

  assetclass=1:noassets
  res <- matrix(NA, nrow=length(assetclass), ncol=length(years))
  res=as.data.frame(res)
  names(res)=yearslabel

  for (j in years) {
    for(i in assetclass)
      res[i,j]=(fcnetavg.full[i,j]*(1+rvanilla[,j])^0.5)
  }

  realcapex.df=res
  realcapex= colSums (realcapex.df, na.rm = T,dims=1)
  realcapexall=realcapex

  return(realcapexall) }

