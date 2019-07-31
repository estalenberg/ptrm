#' Consumption load profile for non-solar customers
#'
#' Formula for calculating the consumption load profile for non-solar customers
#' #'
#' @param yearslabel label of projection years from 2020
#' @param priceprof.df dataframe of static load profile for year 1
#' @param unit.rate energy unit rate $/kwh
#' @param signal.in dynamic variable of dynamic price signal
#'
#' @export
#'
cons_nonsolar_fun=function(yearslabel,priceprof.df,unit.rate,signal.in){

  sig.num=c(0,0.375,0.75)
  sig.lab=c("none","mid-strength","strong")
  sig.df=cbind.data.frame(sig.num,sig.lab)
  signal=as.numeric(sig.df$sig.num[sig.df$sig.lab==signal.in])
  signal.in.num=signal/100 #convert to percent

  #steps 19 and 20
  #need an empty matrix for noyears*24
  tmp <- matrix(NA, ncol=length(yearslabel), nrow=24)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel
  con=tmp

  con[,1]=priceprof.df$non.solar.load

  #grouped into hours
  #1-10am
  #=D71+('SAPN Dynamic variables'!$F$12*10) = f12 signal.in.num
  for (i in 2:length(con)){
    for (j in 1:10){
      con[j,i]=con[j,i-1]+(signal.in.num*10)
    }
  }

  #11-16
  for (i in 2:length(con)){
    for (j in 11:16){
    con[j,i]=con[j,i-1]+(signal.in.num*60)
    }
  }

  for (i in 2:length(con)){
    for (j in 17:22){
    con[j,i]=con[j,i-1]-(signal.in.num*80)
    }
  }

  for (i in 2:length(con)){
    for (j in 23:24){
      con[j,i]=con[j,i-1]+(signal.in.num*10)
    }
  }

    return(con)
  }

