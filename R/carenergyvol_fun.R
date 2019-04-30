#' Car penetraion function
#'
#' Formula for calculating the penetration of electric vehicles over time
#'
#'
#' @param yearslabel label of projection years from 2020
#' @param other.df database of static dnsp inputs
#' @param electriccars number of electric vehicles
#'
#' @export
#'
carenergyvol_fun=function(yearslabel,other.df,electriccars){

  electcarload=other.df[which(other.df$name=="load of electric car"),grep("^all.years$",colnames(other.df))]

  tmp <- matrix(NA, ncol=length(yearslabel-1), nrow=1)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel
  carv=tmp

  for(i in 1:5)
    carv[i]=0
  for(i in 6:length(carv))
    carv[i]=(electriccars[i]*electcarload)


    return(carv)
  }

