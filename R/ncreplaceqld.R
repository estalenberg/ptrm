#' Netcapex functions: i Replacement
#'
#' First formula for extending netcapex beyond five years March 22 2019
#'
#' Returns a database of projected netcapex: i replacement for all assets across all years.
#'
#' Prerequisites that need to be defined in ptrm model (i.e. run above code) are defined in arguments below.
#'
#' @param fcnetavg empty dataframe with first 5 years from DNSP and an avg for 2025
#' @param noyears count of years
#' @param yearslabel label pf projected years
#' @param projyearend final projected year
#' @param noassets count of asset classes
#' @param assetcode if asset type is coded as 1 or 2
#' @param age age of assets in years, DNSP yearly vector
#' @param repratio replacement ratio percent, DNSP Static variable
#' @param modrepcost  modern day replacement cost, DNSP Static variable
#' @param startincline  Start incline of net capex, DNSP Static variable
#' @param ratedecline  Rate of decline, DNSP Static variable
#'
#' @export
#'
#'
ncreplaceqld_fun <- function(yearslabel, fcnetavg,noyears,projyearend,noassets, assetcode, repratio,
                          age, modrepcost,startincline,ratedecline){

  startncyears=yearslabel[6]
  noncyears=6:noyears
  ncyearslab=startncyears:projyearend
  repdf=fcnetavg*repratio #replacement ratio of system assets
  repdf[,6]=rowMeans(repdf[1:5]) #might need to do this in original augex calc too

  tmp=repdf[,6:length(repdf)]
  nc=tmp

  ncage=age[6:length(age)]

  assetclass=1:noassets
  nc$code=assetcode
  avgrep=as.numeric(repdf[,6])

  avgrepdf=cbind.data.frame(assetclass,assetcode,avgrep)
  sumcode1=sum(avgrepdf$avgrep[avgrepdf$assetcode==1],na.rm=T)


  for (j in 1:noassets) {
    assetclass=j
      for(i in 1:length(ncyearslab))
        nc[assetclass,i]=avgrep[assetclass] #make 6th year avg
      for(i in 2:length(ncyearslab)){
        if(ncyearslab[i]<70){
          nc[assetclass,i]=nc[assetclass,(i-1)]*((startincline-((ncage[i]-50)*ratedecline)))
          } #=L41*((L42-(L43*L44)))

         else {
          (1/ncage[i]*modrepcost*avgrep[assetclass]/sumcode1)}
          }
      }

  nc$code=assetcode

  ncreplace.df=repdf
  ncreplace.df$code=assetcode
  #attach to previous dataframe
  ncreplace.df[,6:length(ncreplace.df)]=nc #make the rest equal to the final year (2024)
  nc=ncreplace.df

  #change all asset code 2 to zero
  for (i in 1:(length(nc)-1)){
    for (j in 1:noassets)
      ifelse((nc[j,(grep("code", colnames(nc)))]==2),(nc[j,i]=0),(nc[j,i]=nc[j,i]))}
  ncreplace.df=nc


  ncreplace.df=ncreplace.df[1:(length(ncreplace.df)-1)]
#  write.csv(nc, "replaceenergex.csv")


  return(ncreplace.df)
}



