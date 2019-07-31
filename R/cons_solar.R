#' Consumption load profile for solar customers
#'
#' Formula for calculating the consumption load profile for solar customers without ev
#' #'
#' @param yearslabel label of projection years from 2020
#' @param priceprof.df dataframe of static load profile for year 1
#' @param unit.rate energy unit rate $/kwh
#' @param signal.in dynamic variable of dynamic price signal
#'
#' @export
#'
cons_solar_fun=function(yearslabel,priceprof.df,unit.rate,signal.in){

  sig.num=c(0,0.375,0.75)
  sig.lab=c("none","mid-strength","strong")
  sig.df=cbind.data.frame(sig.num,sig.lab)
  signal=as.numeric(sig.df$sig.num[sig.df$sig.lab==signal.in])
  signal.in.num=signal/100 #convert to percent

  #steps 26 and 27
  #need an empty matrix for noyears*24
  tmp <- matrix(NA, ncol=length(yearslabel), nrow=24)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel
  con=tmp

  con[,1]=priceprof.df$solar.load

  #grouped into hours
  #1-2am
  #=D133+('SAPN Dynamic variables'!$F$12*60) = f12 signal.in.num
  for (i in 2:length(con)){
    for (j in 1:2){
      con[j,i]=con[j,i-1]+(signal.in.num*60)
    }
  }

  #3-8am
  for (i in 2:length(con)){
    for (j in 3:8){
    con[j,i]=con[j,i-1]+(signal.in.num*20)
    }
  }

  #9am to 2pm
  for (i in 2:length(con)){
    for (j in 9:14){
    con[j,i]=con[j,i-1]+(signal.in.num*0)
    }
  }

  #3 to 5pm
  for (i in 2:length(con)){
    for (j in 15:17){
      con[j,i]=con[j,i-1]+(signal.in.num*20)
    }
  }
  #6 to 11pm
  for (i in 2:length(con)){
    for (j in 18:23){
      con[j,i]=con[j,i-1]-(signal.in.num*60)
    }
  }
  #12am
  for (i in 2:length(con)){
    for (j in 24){
      con[j,i]=con[j,i-1]+(signal.in.num*60)
    }
  }

    return(con)
  }

