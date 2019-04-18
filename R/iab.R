#' Initial asset base function
#'
#' Formula for initial asset base for a single asset. All assets must be calculated by looping this function
#'
#' This is used in the year on year depreciation calculations.
#' PTRM excel has iab as a macro for each asset class e.g. asset class 1 is A1iab:G76.
#' Returns the iab for one asset class for all years.
#'
#' Prerequisites that need to be defined in ptrm model (i.e. run above code):
#' remlife,oavalue,assetclass
#'
#' @param y vector of years from 1
#' @param remlife remaining life of asset from assets.df
#' @param oavalue opening asset value from assets.df
#' @param assetclass id of asset class to be modelled
#' @keywords year on year tracking, capex, iab, asset class, remlife, oavalue,
#' @export
#'
#'
iab_fun=function(y,remlife,oavalue,assetclass)
{
  iabstart=ifelse(remlife>0,oavalue/remlife,0) #vector for each asset class - but get rid of infinite values
  if(length(y)<2){y=iabstart[assetclass]}else{
    for(i in 1:length(y))
      if(y[i]<2){y[i]=iabstart[assetclass]}else{
        if(is.na(remlife[assetclass])){y[i]=NA}else{ #use this to keep consistent with PTRM
          if(y[i]>remlife[assetclass]){#get rid of this line to smooth out predictions
            y[i]=(oavalue[assetclass]-sum(y[1:(i-1)]))}else{ #here too
              y[i]=iabstart[assetclass]
            } #end of decreasing loop
        }
      }
  }
  y
}
