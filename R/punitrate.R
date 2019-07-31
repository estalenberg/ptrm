#' Energy unit rate by hour
#'
#' Formula for calculating the energy unit rate per hour for calculating price
#'
#' @param yearslabel label of projection years from 2020
#' @param unit.rate energy unit rate $/kwh
#' @param signal.in dynamic variable of dynamic price signal
#'
#' @export
#'
punitrate_fun=function(yearslabel,unit.rate,signal.in){

  sig.num=c(0,0.375,0.75)
  sig.lab=c("none","mid-strength","strong")
  sig.df=cbind.data.frame(sig.num,sig.lab)
  signal=as.numeric(sig.df$sig.num[sig.df$sig.lab==signal.in])
  signal.in.num=signal/100 #convert to percent

  #steps 17 and 18 energy unit rate by hour
  #need an empty matrix for noyears*24
  tmp <- matrix(NA, ncol=length(yearslabel), nrow=24)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel
  ur=tmp
  ur[,1]=as.numeric(unit.rate[1])

  #grouped into hours
  #='SAPN final price'!E$16-('SAPN Dynamic variables'!$F$12*0.1*(E$39-$D$39))
  #1-10am
  for (i in 2:length(ur)){
    ur[1:10,i]=unit.rate[i]-(signal.in.num*0.1*(yearslabel[i]-yearslabel[1]))
  }

  #11-16
  for (i in 2:length(ur)){
    ur[11:16,i]=unit.rate[i]-(signal.in.num*0.2*(yearslabel[i]-yearslabel[1]))
  }

  for (i in 2:length(ur)){
    ur[17:22,i]=unit.rate[i]+(signal.in.num*0.25*(yearslabel[i]-yearslabel[1]))
  }

  for (i in 2:length(ur)){
    ur[23:24,i]=unit.rate[i]-(signal.in.num*0.1*(yearslabel[i]-yearslabel[1]))
  }

    return(ur)
  }

