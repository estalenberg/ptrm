#' New netcapex function
#'
#' Formula for extending netcapex beyond five years
#'
#' Returns a database of projected netcapex for all assets x all years.
#' 2025 netcapex is first determined by averaging the years from 2020-24.
#' For the following years a dnsp-specific formula is used to predict netcapex until projected year end.
#'
#' Prerequisites that need to be defined in ptrm model (i.e. run above code):
#' fcnetcapex:2020-2024 dataframe, yearslabel, noassets, noyears
#' and the dynamic inputs: age, productivity and networksize. Currently only defined for Ausgrid.
#' Eventually this will by updated to specify a unique formula for each asset class type and dnsp.

#' @param fcnetcapex dataframe of first 5 years of netcapex for dnsp
#' @param yearslabel from 2020 to projected year end
#' @param noyears count of years
#' @param noassets count of asset classes
#' @param age dynamic input age of assets
#' @param productivity dynamic input productivity
#' @param networksize dynamic input network size
#' @keywords netcapex, capex, dnsp, asset class, asset class type
#' @export
#' @examples
#'
#' projyearend = 97
#' startyearend = 19
#' noyears = projyearend-startyearend
#' noassets = 22
#' age = 50
#' productivity = 0
#' networksize = 0
#'
netcapex_fun <- function(fcnetcapex,yearslabel,noassets,noyears, age, productivity, networksize){
  fcnetavg=fcnetcapex
  fcnetavg$fccapex25=rowMeans(fcnetavg)
  #make an empty df for all years and then put in the first 5 years
  res <- matrix(NA, ncol=length(yearslabel-1), nrow=noassets)
  res=as.data.frame(res)
  names(res)=yearslabel
  fcnetyears=(1:length(fcnetavg))
  tmp=res
  tmp[, fcnetyears]=fcnetavg
  fcnetavg=tmp
  avg=fcnetavg$`25`

  #make a smaller df for the 6 years and above predictions
  ncyears=6:noyears
  tmp=res[,6:length(res)]
  nc=tmp

  #formulas:
  for (j in 1:noassets) {
    assetclass=j
    if(length(ncyears)<2){nc[assetclass,1]=avg[assetclass]}else{
      for(i in 1:length(ncyears))
        nc[assetclass,i]=avg[assetclass]
      for(i in 2:length(ncyears))
        nc[assetclass,i]=((((3-(age-50)*0.1)*0.01)+1)*(nc[assetclass,i-1]))*((1-productivity))*(1+networksize)}
    }

  #attach to previous dataframe
  fcnetavg[,6:length(fcnetavg)]=nc #make the rest equal to the final year (2024)
  fcnetavg.full=fcnetavg

  return(fcnetavg.full)}

