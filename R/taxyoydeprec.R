#' TAX: YOY depreciation function
#'
#' Additive formula for tax year on year depreciation in each year for a single asset.
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
#' @param taxstdlife standard life for the asset
#' @param startyearend 2019 or 19
#' @param fctaxcapex.df dataframe of the forecasted netcapex plus the customer contributions
#' @param cif.df cumulative inflation index from 2020
#'
#' @export
#'
#'
taxyoy_fun=function(y,capexstage,assetclass, taxstdlife, startyearend, cif.df, fctaxcapex.df)
{
  if(length(y)<2){y[i]=0}else{# pardon the case where x is of length 1 or 0
    for(i in 1:length(y))
      if(is.na(taxstdlife[assetclass])|((y[i]-startyearend)<(capexstage+1))){y[i]=NA}else{
        ifelse((y[i]-startyearend<=(taxstdlife[assetclass]+capexstage)),
               (y[i]=(fctaxcapex.df[assetclass,capexstage]*(cif.df[,capexstage]))/taxstdlife[assetclass]),
               (y[i]=(fctaxcapex.df[assetclass,capexstage]*(cif.df[,capexstage]))-sum(y[1:(i-1)],na.rm=T)))
      }
  }
  y
}



