#' YOY capex function
#'
#' Additive formula for capex depreciation in each year for a single asset.
#' All assets must be calculated by looping this function
#'
#' This is used in the year on year depreciation calculations.
#' Returns the capex for one asset class for all years.
#'
#' Prerequisites that need to be defined in ptrm model (i.e. run above code):
#' remlife,oavalue,assetclass
#'
#' @param y vector of years from 2019:projyearend, 2019 gets removed later
#' @param capexstage corresponds to the year number or 'years'
#' @param assetclass single asset class to model
#' @param stdlife standard life for the asset
#' @param startyearend 2019 or 19
#' @param fcnetavg.full dataframe of the forecasted netcapex determined above
#' @param rvanilla real vanilla WACC vector by years
#'
#'
#' @keywords year on year tracking, capex, asset class, stdlife, depreciation, real vanilla WACC
#' @export
#'
#'
#capex in year calculations
yoydeprec_fun=function(y,capexstage,assetclass, stdlife,startyearend,fcnetavg.full,rvanilla)
{
  if(length(y)<2){y[i]=0}else{
    for(i in 1:length(y))
      if(is.na(stdlife[assetclass])|((y[i]-startyearend)<(capexstage+1))){y[i]=NA}else{
        if((y[i]-startyearend)<(stdlife[assetclass]+capexstage)){
          y[i]=(fcnetavg.full[assetclass,capexstage]*(1+rvanilla[,capexstage])^0.5)/stdlife[assetclass]}else{
            y[i]=(fcnetavg.full[assetclass,capexstage]*(1+rvanilla[,capexstage])^0.5)-sum(y[1:(i-1)],na.rm=T)}
      }
  }
  y
}


