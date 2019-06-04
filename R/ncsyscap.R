#' Netcapex functions: iv System capital overhead
#'
#' Fourth formula for extending netcapex beyond five years March 22 2019
#'
#' Returns a database of projected netcapex:iv system capital overhead for all assets across all years.
#'
#' Prerequisites that need to be defined in ptrm model (i.e. run above code) are defined in arguments below.
#'
#' @param yearslabel label of years from y1
#' @param fcnetavg blank dataframe of PTRM input net capex for y1 to end with PTRM inputs
#' @param noassets count of asset classes
#' @param syscapratio system capital overhead ratio, DNSP static variable
#' @param assetcode if asset class is coded as 1 or 2
#'
#' @export
#'
#'
ncsyscap_fun <- function(noassets,yearslabel, fcnetavg, syscapratio,assetcode){

  tmp <- matrix(NA, ncol=length(yearslabel), nrow=noassets)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel
  nc=tmp

  assetclass=1:noassets
  syscapdf=fcnetavg*syscapratio
  sysavg=rowMeans(syscapdf[1:5])
  syscapdf[,6]=sysavg

  for(i in 1:6){
    nc[assetclass,i]=syscapdf[assetclass,i]}
  for(i in 7:length(nc)){
    for (j in 1:noassets){
    if(nc[j,(i-1)]==0){nc[j,i]=0}else
    {(nc[j,i]=nc[j,(i-1)])}
    }
  }

  nc$code=assetcode

  for (i in 6:(length(nc)-1)){
    for (j in 1:noassets)
      ifelse((nc[j,(grep("code", colnames(nc)))]==2),(nc[j,i]=0),(nc[j,i]=nc[j,i]))}


  return(nc)
}



