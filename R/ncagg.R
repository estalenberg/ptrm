#' Netcapex functions: vi Aggregate
#'
#' Final aggregate formula for extending netcapex beyond five years March 22 2019
#'
#' Returns a database of projected netcapex:vi aggregate for all assets across all years.
#' Prerequisites that need to be defined in ptrm model (i.e. run above code) are defined in 'arguments' below.
#'
#'
#' @param yearslabel label of years from 2020
#' @param fcnetcapex dataframe of PTRM input net capex for 2020-24
#' @param noassets count of asset classes
#' @param productivity productivity DNSP yearly vector
#' @param ncreplace.df dataframe of netcapex i replacement from ncreplace_fun
#' @param ncskinny.df dataframe of netcapex ii skinny from ncskinny_fun
#' @param ncaugex.df dataframe of netcapex iii augex from ncaugex_fun
#' @param ncsyscap.df dataframe of netcapex iv system capital overhead from ncsyscap_fun
#' @param ncnonnetwk.df dataframe of netcapex v non-network from ncnonnetwk_fun
#'
#' @export
#'
ncagg_fun <- function(noassets,yearslabel, productivity, fcnetcapex, ncreplace.df, ncskinny.df, ncaugex.df,
                      ncsyscap.df, ncnonnetwk.df){

  tmp <- matrix(NA, ncol=length(yearslabel-1), nrow=noassets)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel
  assetclass=1:noassets
  nc=tmp
  aggend=as.numeric(yearslabel[6])
  avg=rowMeans(fcnetcapex)

  for(i in 1:5)
    nc[assetclass,i]=fcnetcapex[assetclass,i]
  for(i in 6:length(nc))
    nc[assetclass,i]=avg[assetclass]
  for(i in 7:length(nc))
    nc[assetclass,i]= ((ncreplace.df[assetclass,i]-ncskinny.df[assetclass,i]+
                          ncaugex.df[assetclass,i]+ncsyscap.df[assetclass,i]+ncnonnetwk.df[assetclass,i])
                       *(1-productivity[i])^(yearslabel[i]-aggend))

  return(nc)
}


