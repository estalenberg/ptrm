#' TAX: Initial asset base function
#'
#' Formula for tax initial asset base for a single asset. All assets must be calculated by looping this function
#'
#' This is used in the year on year tax depreciation calculations.
#' Returns the tax iab for one asset class for all years.
#'
#'
#' @param y vector of years from 1
#' @param taxremlife tax remaining life of asset from assets.df
#' @param taxoavalue tax opening asset value from assets.df
#' @param assetclass id of asset class to be modelled
#' @keywords tax, year on year tracking, capex, tax iab, asset class, remlife, oavalue,
#' @export
#'
taxiab_fun=function(y,taxremlife,taxoavalue,assetclass)
{
  taxiabstart=ifelse(taxremlife>0,taxoavalue/taxremlife,0) #vector for each asset class - but get rid of infinite values

  if(length(y)<2){y=taxiabstart[assetclass]}else{
    for(i in 1:length(y))
      if(y[i]<2){y[i]=taxiabstart[assetclass]}else{
        if(is.na(taxremlife[assetclass])){y[i]=NA}else{ #use this to keep consistent with PTRM
          if(y[i]>taxremlife[assetclass]){
            y[i]=(taxoavalue[assetclass]-sum(y[1:(i-1)]))}else{
              y[i]=taxiabstart[assetclass]
            }
        }
      }
  }
  y
}


