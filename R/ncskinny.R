#' Netcapex functions: ii Skinny
#'
#' Second formula for extending netcapex beyond five years March 22 2019
#'
#' Returns a database of projected netcapex:ii skinny for all assets across all years.
#'
#' Prerequisites that need to be defined in ptrm model (i.e. run above code) are defined in arguments below.
#'
#' @param yearslabel label of years from 2020
#' @param fcnetcapex dataframe of PTRM input net capex for 2020 to 2024
#' @param ncreplace.df dataframe of replacement net capex calculate from function ncreplace_fun
#' @param noassets count of asset classes
#' @param retireslim retire or slim assets as a percent of prev year, DNSP yearly vector
#'
#' @export
#'
#'
ncskinny_fun <- function(noassets,yearslabel, fcnetcapex, ncreplace.df, retireslim){

  tmp <- matrix(NA, ncol=length(yearslabel-1), nrow=noassets)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel
  assetclass=1:noassets

  nc=tmp
  for(i in 1:6)
    nc[assetclass,i]=0
  for(i in 7:length(nc))
    nc[assetclass,i]=ncreplace.df[assetclass,i]*retireslim[i]

  return(nc)
}



