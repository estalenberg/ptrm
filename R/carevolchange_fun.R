#' Change in electric vehicle energy volume function
#'
#' Formula for calculating the change in electric vehicle energy volume
#'
#'
#' @param yearslabel label of projection years from 2020
#' @param carenergyvol vector of absolute energy volumes from electric vehicles from carenergyvol_fun
#'
#' @export
#'
carevolchange_fun=function(yearslabel,carenergyvol){


  tmp <- matrix(NA, ncol=length(yearslabel), nrow=1)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel
  chg=tmp

  for(i in 1)
    chg[i]=0
  for(i in 2:length(chg))
    chg[i]=(carenergyvol[i]-carenergyvol[i-1])

    return(chg)
  }

