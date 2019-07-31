#' Netcapex functions: v Non-network
#'
#' Fifth formula for extending netcapex beyond five years March 22 2019
#'
#' Returns a database of projected netcapex:v non-network for all assets across all years.
#'
#' Prerequisites that need to be defined in ptrm model (i.e. run above code) are defined in arguments below.
#'
#' @param yearslabel Label of years from 2020
#' @param fcnetcapex Dataframe of PTRM input net capex for 2020 to 2024
#' @param noassets Count of asset classes
#' @param assetcode If asset class is coded as 1 or 2
#'
#' @keywords netcapex, system capital overhead, capex, dnsp, asset class
#' @export
#'
#'
ncnonnetwk_fun <- function(noassets,yearslabel, fcnetcapex, assetcode){

  tmp <- matrix(NA, ncol=length(yearslabel), nrow=noassets)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel
  assetclass=1:noassets
  nc=tmp
  avg=rowMeans(fcnetcapex)

  for(i in 1:5){
    nc[assetclass,i]=fcnetcapex[assetclass,i]}
  for(i in 6:length(nc)){
    nc[assetclass,i]=avg[assetclass]}

  nc$code=assetcode

  #change all asset code 1 to 0
  for (i in 1:(length(nc)-1)){
    for (j in 1:noassets){
    ifelse((nc[j,(grep("code", colnames(nc)))]==1),(nc[j,i]=0),(nc[j,i]=nc[j,i]))}
  }
  nc=nc[1:(length(nc)-1)]

  return(nc)
}



