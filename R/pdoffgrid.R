#' Peak demand profile for customers on grid
#'
#' Formula for calculating the peak demand profile for on grid customers
#'
#' @param yearslabel label of projection years from 2020
#' @param energyvol.df dataset calculated from energy volumes function
#' @param pdprof.df dataset of DNSP static vectors
#'
#' @export
#'
pdoffgrid_fun=function(yearslabel,energyvol.df,pdprof.df){

  fy21=pdprof.df$`21`

  custnum=subset(energyvol.df,energyvol.df$names=="Customers on grid")
  custnum=as.numeric(custnum[1:length(custnum)-1])

  #need an empty matrix for noyears*24
  tmp <- matrix(NA, ncol=length(yearslabel), nrow=24)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel
  pdoff=tmp

  pdoff[1]=pdprof.df$`21`

  for(i in 2:length(pdoff))
    pdoff[i]=pdoff[i-1]*(1+((custnum[i]-custnum[i-1])/custnum[i-1]))


    return(pdoff)
  }

