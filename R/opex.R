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
#' @param opexfactor opex factor, DNSP static vector
#'
#' @export
#'
opex_fun=function(yearslabel, controlopex, productivity,opexfactor){

    tmp <- matrix(NA, ncol=length(yearslabel-1), nrow=1)
    tmp=as.data.frame(tmp)
    names(tmp)=yearslabel
    ox=tmp

    #=(y(i-1)*(1+opexfactor))*(1-productivity[i])

    for(i in 1:5)
      ox[i]=controlopex[i]
    for(i in 6:length(ox))
      ox[i]=(ox[i-1]*(1+opexfactor))*(1-productivity[i])

    return(ox)
  }
