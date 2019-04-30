#' Netcapex functions: iii Augex
#'
#' Third formula for extending netcapex beyond five years March 22 2019
#'
#' Returns a database of projected netcapex:iii augex for all assets across all years.
#'
#' Prerequisites that need to be defined in ptrm model (i.e. run above code) are defined in arguments below.
#'
#' @param fcnetavg empty dataframe with first 5 years from DNSP and an avg
#' @param noyears count of years
#' @param yearslabel label pf projected years
#' @param projyearend final projected year
#' @param noassets count of asset classes
#' @param augexratio augex ratio, DNSP static variable
#' @param addnew add new asset as a percent of previous year, DNSP yearly vector
#' @param assetcode if asset class is coded as 1 or 2
#'
#' @export
#'
ncaugex_fun <- function(yearslabel,fcnetavg,noyears,projyearend,noassets, augexratio, addnew, assetcode){

  startncyears=yearslabel[6]
  noncyears=6:noyears
  ncyearslab=startncyears:projyearend
  augexdf=fcnetavg*augexratio
  tmp=augexdf[,6:length(augexdf)]
  nc=tmp

  ncaddnew=addnew[6:length(addnew)]

  assetclass=1:noassets
  avgaug=as.numeric(augexdf[,6])


  for (j in 1:noassets) {
    assetclass=j
    if(length(ncyearslab)<2){y=avgaug[assetclass]}else{
      for(i in 1:length(ncyearslab))
        nc[assetclass,i]=avgaug[assetclass]
      for(i in 2:length(ncyearslab)){
        nc[assetclass,i]=avgaug[assetclass]*(1+ncaddnew[i])}
    }
  }
  nc$code=assetcode
  #attach to previous dataframe
  ncaugex.df=augexdf
  ncaugex.df$code=assetcode
  ncaugex.df[,6:length(ncaugex.df)]=nc #
  nc=ncaugex.df

  #change all asset code 2 to 0
  for (i in 1:(length(nc)-1)){
    for (j in 1:noassets)
      ifelse((nc[j,(grep("code", colnames(nc)))]==2),(nc[j,i]=0),(nc[j,i]=nc[j,i]))}

  ncaugex.df=nc

  return(ncaugex.df)

    }



