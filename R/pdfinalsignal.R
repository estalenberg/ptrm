#' Final dynamic price signal
#'
#' Formula for calculating the final dynamic price signal over hours and years
#'
#' @param yearslabel label of projection years from 2020
#' @param pd_ev peak demand adjusted for offgrid, ee, solar, batteries and ev
#' @param signal.in dynamic variable of dynamic price signal
#'
#' @export
#'
pdfinalsignal_fun=function(yearslabel,pd_ev,signal.in){

  #final dynamic price signal
  #grouped by hours
  #need an empty matrix for noyears*24
  tmp <- matrix(NA, ncol=length(yearslabel), nrow=24)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel
  pd=tmp

  #row numbers 1:24 refer to 24 hour time
  #step 20: 5pm = 17
  pd[17,]=round(pd_ev[17,],digits=0) #step19[17]

  #step 21: 10pm-midnight
  pd[22,]=round(pd_ev[22,],digits=0) #step19[17]
  pd[23,]=round(pd_ev[23,],digits=0) #step19[17]
  pd[24,]=round(pd_ev[24,],digits=0) #step19[17]

  #step 22:6pm - 9pm (18-21)
  #=G290*(1-'SAPN Dynamic variables'!$F$12)^('SAPN Repex'!F$2-'SAPN Repex'!$E$2)

  for(i in 1:length(pd)){
    for(j in 18:21){
      pd[j,i]=round(pd_ev[j,i]*(1-signal.in)^((yearslabel[i]+1)-yearslabel[1]),digits = 0)
    }
  }

  #step 23: 1am-8am (1:8)
  #=(G273)+((SUM(G$290:G$293)-SUM(G$378:G$381))/8*20%)
  for(i in 1:length(pd)){
    for(j in 1:8){
      pd[j,i]=round((pd_ev[j,i]+((sum(pd_ev[18:21,i])-sum(pd[18:21,i]))/8*0.2)),digits = 0)
    }
  }

  #step 24: 9am-4pm (9:16)
  #=(G281)+((SUM(G$290:G$293)-SUM(G$378:G$381))/8*80%)
  for(i in 1:length(pd)){
    for(j in 9:16){
      pd[j,i]=round((pd_ev[j,i]+((sum(pd_ev[18:21,i])-sum(pd[18:21,i]))/8*0.8)),digits = 0)
    }
  }

    return(pd)
  }

