#' Car penetraion function
#'
#' Formula for calculating the penetration of electric vehicles over time
#'
#'
#' @param yearslabel label of projection years from 2020
#' @param cars.in dynamic input of electric car penetration by 2060
#' @param other.df dataset of DNSP static vectors
#'
#' @export
#'
carpenet_fun=function(yearslabel,cars.in,other.df){

  cars60=cars.in/100 #decimal percent
  todayev=other.df$all.years[other.df$name=="ev penetration 2020"]

  tmp <- matrix(NA, ncol=length(yearslabel), nrow=1)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel
  carp=tmp


  carp[1]=todayev
  for (i in 2:length(carp))
    carp[i]=ifelse((yearslabel[i]<31),(carp[i-1]+(cars60-todayev)/39*0.2),
                 (ifelse((yearslabel[i]>60),cars60,(carp[i-1]+(cars60-todayev)/39*1.2))))
    return(carp)
  }

