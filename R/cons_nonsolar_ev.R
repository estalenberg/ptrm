#' Consumption load profile for non-solar customers with 1 ev
#'
#' Formula for calculating the consumption load profile for non-solar customers with 1 ev
#' #'
#' @param yearslabel label of projection years from 2020
#' @param priceprof.df dataframe of static load profile for year 1
#' @param unit.rate energy unit rate $/kwh
#' @param signal.in dynamic variable of dynamic price signal
#'
#' @export
#'
cons_nonsolarev_fun=function(yearslabel,priceprof.df,unit.rate,signal.in){

  sig.num=c(0,0.375,0.75)
  sig.lab=c("none","mid-strength","strong")
  sig.df=cbind.data.frame(sig.num,sig.lab)
  signal=as.numeric(sig.df$sig.num[sig.df$sig.lab==signal.in])
  signal.in.num=signal/100 #convert to percent

  #steps 37 and 38
  #need an empty matrix for noyears*24
  tmp <- matrix(NA, ncol=length(yearslabel), nrow=24)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel
  con=tmp

  con[,1]=priceprof.df$no.solar.ev.load

  #grouped into hours
  #1-10am
  #=D254+('SAPN Dynamic variables'!$F$12*100) = f12 signal.in.num
  for (i in 2:length(con)){
    for (j in 1:10){
      con[j,i]=con[j,i-1]+(signal.in.num*100)
    }
  }

  #11-16
  for (i in 2:length(con)){
    for (j in 11:16){
    con[j,i]=con[j,i-1]+(signal.in.num*240)
    }
  }

  #5pm
  for (i in 2:length(con)){
    for (j in 17){
      con[j,i]=con[j,i-1]-(signal.in.num*120)
    }
  }

  #6pm-9pm
  for (i in 2:length(con)){
    for (j in 18:21){
    con[j,i]=con[j,i-1]-(signal.in.num*600)
    }
  }

  #10pm
  for (i in 2:length(con)){
    for (j in 22){
      con[j,i]=con[j,i-1]-(signal.in.num*120)
    }
  }
  #11-12am
  for (i in 2:length(con)){
    for (j in 23:24){
      con[j,i]=con[j,i-1]+(signal.in.num*100)
    }
  }

    return(con)
  }

