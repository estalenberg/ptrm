#' Car penetraion function
#'
#' Formula for calculating the penetration of electric vehicles over time
#'
#'
#' @param yearslabel label of projection years from 2020
#' @param cars.in dynamic input of electric car penetration by 2060
#'
#' @export
#'
carpenet_fun=function(yearslabel,cars.in){

  cars60=cars.in/100 #decimal percent

  tmp <- matrix(NA, ncol=length(yearslabel-1), nrow=1)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel
  carp=tmp

  for(i in 1:5)
    carp[i]=0
  for(i in 6:length(carp))
    carp[i]=ifelse(((((1+(cars60*0.02))^((yearslabel[i]+2000)-2026))-1)>1),1,
                   (((1+(cars60*0.02))^((yearslabel[i]+2000)-2026))-1))

  #=if(((1+(cars60*0.02))^((yearslabel[i]+2000)-2026))-1)>1){carp[i]=1}else
  #{(((1+(cars60*0.02))^((yearslabel[i]+2000)-2026))-1)}

    return(carp)
  }

