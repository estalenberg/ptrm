#' Car numbers function
#'
#' Formula for calculating the cars in each dnsp region
#'
#'
#' @param yearslabel label of projection years from 2020
#' @param other.df database of static dnsp inputs
#'
#' @export
#'
cars_fun=function(yearslabel,other.df){

  cars1=other.df$all.years[other.df$name=="cars in 2018"]
  cargrowth=other.df$all.years[other.df$name=="car growth rate"]

  tmp <- matrix(NA, ncol=length(yearslabel), nrow=1)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel
  cars=tmp


  cars[1]=cars1
  for(i in 2:length(cars))
  cars[i]=(cars[i-1]*(1+cargrowth))

    return(cars)
  }

