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

  cars1=other.df[which(other.df$name=="cars in 2018"),grep("^all.years$",colnames(other.df))]
  cargrowth=other.df[which(other.df$name=="car growth rate"),grep("^all.years$",colnames(other.df))]

  tmp <- matrix(NA, ncol=length(yearslabel-1), nrow=1)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel
  cars=tmp

  for(i in 1)
    cars[i]=cars1
  for(i in 2:length(cars))
    cars[i]=(cars[i-1]*(1+cargrowth))

    return(cars)
  }

