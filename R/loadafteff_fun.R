#' Load after energy efficiency function
#'
#' Formula for calculating the load of electricity after energy efficiencies
#'
#'
#' @param yearslabel label of projection years from 2020
#' @param other.df dataset of DNSP static vectors
#' @param use.in dynamic variable of energy use per customer growth rate
#' @param loadaft_offgrid energy load ater offgrid customers exited
#'
#' @export
#'
loadafteff_fun=function(yearslabel,other.df,use.in,loadaft_offgrid){

  use=use.in/100

    tmp <- matrix(NA, ncol=length(yearslabel), nrow=1)
    tmp=as.data.frame(tmp)
    names(tmp)=yearslabel
    load=tmp

    for(i in 1:length(load))
    load[i]=loadaft_offgrid[i]*(1+use)^((yearslabel[i]+1)-yearslabel[1])
    load=round(load,digits=2)


    return(load)
  }

