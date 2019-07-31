#' Netcapex functions: iii Augex
#'
#' Third formula for extending netcapex beyond five years March 22 2019
#'
#' Returns a database of projected netcapex:iii augex for all assets across all years.
#'
#' Prerequisites that need to be defined in ptrm model (i.e. run above code) are defined in arguments below.
#'
#' @param fcnetavg empty dataframe with first 5 years from DNSP and an avg
#' @param noassets count of asset classes
#' @param growth.df dataframe of capex growth function
#' @param augexratio augex ratio, DNSP static variable
#' @param assetcode if asset class is coded as 1 or 2
#'
#' @export
#'
ncaugex_fun <- function(fcnetavg,augexratio,growth.df,noassets,assetcode){

  augexdf=fcnetavg*augexratio

  #year 6 onwards:
  #=I3*(1+'Growth capex model'!H$13)
  #growth capex model is an entire new function
  #h13 = change final row in growth.df

  for(i in 6:length(augexdf)){
    for (j in 1:noassets){
    augexdf[j,i]=augexdf[j,(i-1)]*(1+growth.df[8,i])
    }
  }

  #asset code
  augexdf$code=assetcode

  #change all asset code 2 to 0
  for (i in 1:(length(augexdf)-1)){
    for (j in 1:noassets){
      ifelse((augexdf[j,(grep("code", colnames(augexdf)))]==2),(augexdf[j,i]=0),(augexdf[j,i]=augexdf[j,i]))}
  }

  augexdf=augexdf[1:(length(augexdf)-1)]


  return(augexdf)

    }



