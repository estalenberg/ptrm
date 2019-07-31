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
#' @param productivity.in dynamic variable of productivity
#' @param ncreplace.df dataframe of netcapex i replacement from ncreplace_fun
#' @param ncskinny.df dataframe of netcapex ii skinny from ncskinny_fun
#' @param ncaugex.df dataframe of netcapex iii augex from ncaugex_fun
#' @param ncsyscap.df dataframe of netcapex iv system capital overhead from ncsyscap_fun
#' @param ncnonnetwk.df dataframe of netcapex v non-network from ncnonnetwk_fun
#'
#' @export
#'
ncagg_fun <- function(noassets,yearslabel, productivity.in, fcnetcapex, ncreplace.df, ncskinny.df, ncaugex.df,
                      ncsyscap.df, ncnonnetwk.df){

  tmp <- matrix(NA, ncol=length(yearslabel), nrow=noassets)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel
  nc=tmp
  productivity=productivity.in/100

  aggend=as.numeric(yearslabel[6])
  #avg=rowMeans(fcnetcapex)

  for(i in 1:5){
    nc[,i]=fcnetcapex[,i]
  }

  #6 onwards=('SAPN Repex'!J3-' SAPN Skinny'!J3+'SAPN New augex'!J3+'SAPN sys overhead'!J3+'SAPN non-network'!J3)*
  #(1-'SAPN Dynamic variables'!$G$4)^(J$2-2026)

  for(i in 6:length(nc)){
    for (j in 1:noassets){
    nc[j,i]= ((ncreplace.df[j,i]-ncskinny.df[j,i]+ncaugex.df[j,i]+ncsyscap.df[j,i]+ncnonnetwk.df[j,i])
                       *(1-productivity)^(yearslabel[i]-aggend))
    }
  }


  return(nc)
}


