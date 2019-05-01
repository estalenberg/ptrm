#' OPEX function
#'
#' Formula for calculating the controllable OPEX for all projected years
#'
#' Returns a vector of OPEX for each year.
#'
#' Prerequisites that need to be defined in ptrm model (i.e. run above code) are in arguments below:
#'
#' @param yearslabel label of projection years from 2020
#' @param controlopex vector of controllable opex for 2020-24 from DNSP ptrm input
#' @param productivity productivity percent, DNSP dynamic yearly vector
#' @param other.df dataset of DNSP static vectors
#'
#' @export
#'
opex_fun=function(yearslabel, controlopex, productivity,other.df){

  opexfactor = as.numeric(other.df$all.years[other.df$name=="Opex output factor"])
  opexrealcost = as.numeric(other.df$all.years[other.df$name=="opex real cost"])
  opexstepchange = as.numeric(other.df$all.years[other.df$name=="opex step change"])

    tmp <- matrix(NA, ncol=length(yearslabel), nrow=1)
    tmp=as.data.frame(tmp)
    names(tmp)=yearslabel
    ox=tmp

  #  '=(I3+opexstepchange)*(1+opexfactor+opexrealcost)*(1-productivity)

    for(i in 1:5)
      ox[i]=controlopex[i]
    for(i in 6:length(ox))
      ox[i]=((ox[i-1]+opexstepchange)*(1+opexfactor+opexrealcost))*(1-productivity[i])

    return(ox)
  }

