#' Growth capex model functions
#'
#' Formula for calculating the growth capex model
#'
#'
#' @param dnsp.in DNSP selected for function loop
#' @param other.df database of static dnsp inputs
#' @param pdprof.df database of static dnsp inputs for peak demand profile
#' @param energyvol.df database of energy volumes function ouput
#' @param peakdemand.df database of peak demand function ouput
#' @param projyearend.in dynamic variable of final year
#' @param demand.in dynamic variable of demand management intensity
#' @param solar.in dynamic variable of percent customers with solar by 2060
#' @param batt.in dynamic variable of percent customers with batteries by 2060
#' @param age.in dynamic variable of age of assets
#' @param cust.in dynamic variable of customer numbers growth
#' @param cars.in dynamic variable of proportion of electric vehicle penetration by 2060
#'
#' @export
#'
allgrowth_fun=function(dnsp.in,other.df,pdprof.df,projyearend.in,energyvol.df,peakdemand.df,
                       demand.in,solar.in, batt.in, age.in, cust.in,cars.in){
  #cut to dnsp
  d.name=c("Energex","SAPN")
  d.code=1:2
  dnsp.df=cbind.data.frame(d.code,d.name)
  dnsp=as.numeric(dnsp.df$d.code[dnsp.df$d.name==dnsp.in])
  dnsp.label=as.character(dnsp.in)

  other.df=other.df[other.df$dnsp==(as.numeric(dnsp)),]
  other.df=subset(other.df,(!is.na(other.df$dnsp)))

  pdprof.df=pdprof.df[pdprof.df$dnsp==(as.numeric(dnsp)),]
  pdprof.df=subset(pdprof.df,(!is.na(pdprof.df$dnsp)))

  #steps 1-5 ----
  df=subset(other.df,other.df$type=="forecast capex")
  df=cbind.data.frame(df$name,df$`1`,df$`2`,df$`3`,df$`4`,df$`5`)
  names(df)=c("name","1","2","3","4","5")

  #time line ----
  projyearend = projyearend.in
  startyearend=other.df$all.years[other.df$name=="year start"]
  noyears=(projyearend-startyearend)
  years=1:noyears
  yearslabel=(startyearend:projyearend)
  yearslabel=yearslabel[2:length(yearslabel)]

  #tmp dataframe
  tmp <- matrix(NA, ncol=length(yearslabel), nrow=7)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel

  #step 6 ----
  tmp[1:6, 1:5]=df[1:6,2:6]
  tot=colSums(df[2:6])
  tmp[7,1:5]=tot
  growth.df=tmp

  #step 7 ----
  tmp <- matrix(NA, ncol=length(yearslabel), nrow=8)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel
  capac.df=tmp

  #=MAX('SAPN Peak demand profile'!G361:G384)
  colMax <- function(data) sapply(data, max, na.rm = TRUE)
  maxdemand=colMax(peakdemand.df)

  for(i in 1:length(capac.df)){
  capac.df[1,i]=maxdemand[i]
  }
  #step 8-12 ----
 #8
   for(i in 6:length(capac.df)){
    capac.df[2,i]=capac.df[1,i]-capac.df[1,1]
   }

  #9/10
  for(i in 6:length(capac.df)){
    capac.df[3,i]=capac.df[2,i]/capac.df[1,1]
  }

  #step 11
  #=IF(H19>('SAPN Static variables'!$F$70-'SAPN Static variables'!$F$71)
  #yes:(H19-('SAPN Static variables'!$F$70-'SAPN Static variables'!$F$71))*'SAPN Static variables'!$F$28*0.5*
  #(1-'SAPN Dynamic variables'!$F$13)
  #no = 0)

  #f13 dynamic=demand.in
  maxuse=other.df$all.years[other.df$name=="max utilisation"] #f70
  c.maxuse=other.df$all.years[other.df$name=="current max utilisation"] #f71
  mod.rep.cost=other.df$all.years[other.df$name=="Modern rep cost"] #f28

  for(i in 6:length(capac.df)){
    capac.df[4,i]=ifelse((capac.df[3,i]>(maxuse-c.maxuse)),
      ((capac.df[3,i]-(maxuse-c.maxuse))*mod.rep.cost*0.5*(1-demand.in)),
      0)
    }

  for(i in 7:length(capac.df)){
    capac.df[5,i]=capac.df[4,i]-capac.df[4,(i-1)]
  }

  capac.df[5,6]=0

  #step 13 capacity ----
  avg.cap=rowMeans(growth.df[1,1:5],na.rm=T)
  for(i in 6:length(growth.df)){
  growth.df[1,i]=avg.cap+capac.df[5,i]
  }

  #step 14 and 15 der----
  #der capex
  #=(AVERAGE($C$6:$G$6)/('SAPN Static variables'!$F$51+'SAPN Static variables'!$F$54)*('SAPN load growth'!E15+'SAPN load growth'!E20))

  avg.der=rowMeans(growth.df[2,1:5],na.rm=T)
  solarpen=other.df$all.years[other.df$name=="solar penetration 2020"] #f51
  battpen=other.df$all.years[other.df$name=="battery 2020"] #f54
  solarp=psolar_fun(other.df, yearslabel, solar.in) #e15
  battp=pbatt_fun(other.df, yearslabel, batt.in) #e20

  #check----
  for(i in 6:length(growth.df)){
    growth.df[2,i]=avg.der/(solarpen+battpen)*(solarp[i-5]+battp[i-5]) #here zubi uses 2021 percent solar to predict 2026
  }

  #16 & 17 reliability ----
  #=AVERAGE($C$8:$G$8)*(1+(('SAPN Dynamic variables'!$F$2-60)/100))
  avg.rel=rowMeans(growth.df[4,1:5],na.rm=T)
 #f2 age.in

  for(i in 6:length(growth.df)){
    growth.df[4,i]=avg.rel*(1+((age.in-60)/100))
    }

  #18 & 19 connections ----
  #=($G$7/'SAPN Static variables'!$F$75)*'SAPN Dynamic variables'!$F$9
  #f9 cust.in
  congrth=other.df$all.years[other.df$name=="Forecast connection growth"] #f75
  #g7 2025 con (growth.df row 3)

  for(i in 6:length(growth.df)){
    growth.df[3,i]=(growth.df[3,5]/congrth)*(cust.in/100)
  }

  #20 ev units cumulative ----
  #=('SAPN Static variables'!$F$72*'SAPN load growth'!J26)*('SAPN Static variables'!$F$45/'SAPN Static variables'!$F$73)

  pubev=other.df$all.years[other.df$name=="Public EV chargers"] #f72
  car_nat=other.df$all.years[other.df$name=="Aus cars in 2018"] #f73
  car_dnsp=other.df$all.years[other.df$name=="cars in 2018"] #f45
  #j26
  carpen=carpenet_fun(yearslabel,cars.in,other.df) #j26

  for(i in 6:length(capac.df)){
    capac.df[6,i]=(pubev*carpen[i])*(car_dnsp/car_nat)
  }

  #21 ev units per year ----
  for(i in 7:length(capac.df)){
    capac.df[7,i]=capac.df[6,i]-capac.df[6,(i-1)]
  }

  capac.df[7,6]=capac.df[6,6]

  #22 ev capex cost per year ----
  #=H38*'SAPN Static variables'!$F$74/1000000
  pubevcost=other.df$all.years[other.df$name=="Public EV charger cost per unit"] #f74

  for(i in 6:length(capac.df)){
    capac.df[8,i]=capac.df[7,i]*pubevcost/1000000
  }

  #23 ev capex----
  growth.df[5,6]=sum(capac.df[8,(1:6)],na.rm=T)

  for(i in 7:length(growth.df)){
    growth.df[5,i]=capac.df[8,i]
  }

  #24 & 25 other ----
  avg.oth=rowMeans(growth.df[6,1:5],na.rm=T)
  growth.df[6,6:length(growth.df)]=avg.oth

  #total step 6 ----
  growth.df[7,]=0
  growth.df[7,]=round(colSums(growth.df, na.rm=T),2)

  #26 change
  for(i in 6:length(growth.df)){
    growth.df[8,i]=(growth.df[7,i]-growth.df[7,(i-1)])/growth.df[7,(i-1)]
  }

#what do we need?
  #rows 5-13 are growth.df
  #17-21 and 37-39 are capac.df


  return(growth.df)
  }

