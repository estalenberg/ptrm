#' OPEX function
#'
#' Formula for calculating the controllable OPEX for all projected years
#'
#' Returns a vector of OPEX for each year.
#'
#' Prerequisites that need to be defined in ptrm model (i.e. run above code) are in arguments below:
#'
#' @param yearslabel label of projection years from year start
#' @param opexdrc vector of opex debt raising cost for first 5 years from DNSP ptrm input
#'
#' @export
#'
opexdrc_fun=function(yearslabel, opexdrc){

    tmp <- matrix(NA, ncol=length(yearslabel-1), nrow=1)
    tmp=as.data.frame(tmp)
    names(tmp)=yearslabel
    ox=tmp
    oxavg=rowMeans(opexdrc)

    for(i in 1:5)
      ox[i]=opexdrc[i]
    for(i in 6:length(ox))
      ox[i]=oxavg

    return(ox)
  }
