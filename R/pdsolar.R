#' Peak demand profile for energy efficiency
#'
#' Formula for calculating the peak demand profile for energy efficiency
#'
#' @param yearslabel label of projection years from 2020
#' @param use.in dynamic variable of energy use
#' @param pdoffgrid peak demand adjusted for off grid customers
#'
#' @export
#'
pdsolar_fun=function(yearslabel,use.in,pdoffgrid){

  #need an empty matrix for noyears*24
  tmp <- matrix(NA, ncol=length(yearslabel), nrow=24)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel
  pd=tmp

  for(i in 1:length(pd)){
    #=G47*(1+'Dynamic variables'!$F$11)^('1a Model variables updated'!D$6-'1a Model variables updated'!$C$6)
    pd[i]=pdoffgrid[i]*(1+(use.in/100))^(yearslabel[i+1]-yearslabel[1])
  }
  #fix the final column for final years +1
  pd[length(pd)]=pdoffgrid[length(pd)]*(1+(use.in/100))^(yearslabel[length(yearslabel)]+1-yearslabel[1])

    return(pd)
  }

