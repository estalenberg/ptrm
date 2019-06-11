#' Reduction in energy load from solar
#'
#' Formula for calculating tbe cumulative reduction in energy load GWh from increased solar per year
#'
#'
#' @param yearslabel label of projection years from 2020
#' @param other.df dataset of DNSP static vectors
#' @param custongrid number of customers on grid
#' @param psolar percent of customers on solar
#'
#' @export
#'
rloadsolar_fun=function(yearslabel,other.df,custongrid,psolar){

  energysolar=other.df$all.years[other.df$name=="energy from solar"]
  solarexport=other.df$all.years[other.df$name=="solar export to grid"]

    tmp <- matrix(NA, ncol=length(yearslabel), nrow=1)
    tmp=as.data.frame(tmp)
    names(tmp)=yearslabel
    load=tmp

    load[1]=0

    for(i in 2:length(load))
    load[i]=custongrid[i]*(psolar[i]-psolar[1])*energysolar*(1-solarexport)/1000000

    load=round(load,digits=2)


    return(load)
  }

