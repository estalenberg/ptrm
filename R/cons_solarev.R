#' Consumption load profile for solar ev customers
#'
#' Formula for calculating the consumption load profile for solar customers with 1 ev
#' #'
#' @param yearslabel label of projection years from 2020
#' @param priceprof.df dataframe of static load profile for year 1
#' @param unit.rate energy unit rate $/kwh
#' @param signal.in dynamic variable of dynamic price signal
#'
#' @export
#'
cons_solarev_fun=function(yearslabel,priceprof.df,unit.rate,signal.in){

  sig.num=c(0,0.375,0.75)
  sig.lab=c("none","mid-strength","strong")
  sig.df=cbind.data.frame(sig.num,sig.lab)
  signal=as.numeric(sig.df$sig.num[sig.df$sig.lab==signal.in])
  signal.in.num=signal/100 #convert to percent

  #steps 44 and 45
  #need an empty matrix for noyears*24
  tmp <- matrix(NA, ncol=length(yearslabel), nrow=24)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel
  con=tmp

  con[,1]=priceprof.df$solar.ev.load

  #grouped into hours
  #1-8am
  #=D314+('SAPN Dynamic variables'!$F$12*100) = f12 signal.in.num
  for (i in 2:length(con)){
    for (j in 1:8){
      con[j,i]=con[j,i-1]+(signal.in.num*100)
    }
  }

  #9-5pm
  for (i in 2:length(con)){
    for (j in 9:17){
    con[j,i]=con[j,i-1]+(signal.in.num*0)
    }
  }

  #6pm
  for (i in 2:length(con)){
    for (j in 18){
    con[j,i]=con[j,i-1]-(signal.in.num*200)
    }
  }

  #7 to 8pm
  for (i in 2:length(con)){
    for (j in 19:20){
      con[j,i]=con[j,i-1]-(signal.in.num*300)
    }
  }

  #9-10pm
  for (i in 2:length(con)){
    for (j in 21:22){
      con[j,i]=con[j,i-1]-(signal.in.num*200)
    }
  }

  #11pm to 12am
  for (i in 2:length(con)){
    for (j in 23:24){
      con[j,i]=con[j,i-1]+(signal.in.num*200)
    }
  }


    return(con)
  }

