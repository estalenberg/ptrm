#' Price per GWh and per customer
#'
#' Formulas for calculating the relative price for the six types of customers
#'
#' @param dnsp.in DNSP selected for function loop
#' @param projyearend.in dynamic variable of final year
#' @param df.real dataframe of real revenue calculated from ptrm_fun
#' @param energyvol.df dataframe of relative energy volumes from allenergyvol_fun
#' @param other.df dataframe of static dnsp inputs
#' @param priceprof.df dataframe of static dnsp price profile inputs
#' @param cust.in dynamic variable of customer growth percentage
#' @param signal.in dynamic variable of price signal text input
#'
#' @export
#'
price_fun=function(dnsp.in,df.real,energyvol.df,other.df,priceprof.df,projyearend.in,cust.in,signal.in){
  #cut to dnsp
  d.name=c("Energex","SAPN")
  d.code=1:2
  dnsp.df=cbind.data.frame(d.code,d.name)
  dnsp=as.numeric(dnsp.df$d.code[dnsp.df$d.name==dnsp.in])
  dnsp.label=as.character(dnsp.in)

  other.df=other.df[other.df$dnsp==(as.numeric(dnsp)),]
  other.df=subset(other.df,(!is.na(other.df$dnsp)))

  priceprof.df=priceprof.df[priceprof.df$dnsp==(as.numeric(dnsp)),]
  priceprof.df=subset(priceprof.df,(!is.na(priceprof.df$dnsp)))

  #time line ----
  projyearend = projyearend.in
  startyearend=other.df$all.years[other.df$name=="year start"]
  noyears=(projyearend-startyearend)
  years=1:noyears
  yearslabel=(startyearend:projyearend)
  yearslabel=yearslabel[2:length(yearslabel)]

  #step 1
  #subset revenue.real
  rev.df=subset(df.real,df.real$type=="Revenue (real)")
  revenue.real=rev.df$cost*1000000
  revenue.real=revenue.real[2:length(revenue.real)]

  #setwd("c://R")
  #write.csv(revenue.real,"rev.csv")

  #prestep 1
  #customers = E3*'SAPN Static variables'!$F$61
  custnum=cust_fun(yearslabel,other.df,cust.in) #=e3
  p.res.cust=other.df$all.years[other.df$name=="residential customers"] #f61
  res.customers=custnum*p.res.cust

  #prestep 2
  #=E4*'SAPN Static variables'!$F$63
  load.p.cust=other.df$all.years[other.df$name=="energy p cust 2020"] #f50
  res.load=other.df$all.years[other.df$name=="residential load"] #f63
  loadcust=custnum*load.p.cust #e4
  load.aft.cust=loadcust*res.load

  #prestep 3
  #=E5*'SAPN Static variables'!$F$61
  custnum.ogrid=subset(energyvol.df, energyvol.df$name=="Customers on grid")
  custnum.ogrid=custnum.ogrid[1:length(custnum.ogrid)-1]
  cust.ogrid=round(p.res.cust*custnum.ogrid,digits=0)

  #prestep 4
  #=E6*'SAPN Static variables'!$F$63
  loadaft_offgrid=subset(energyvol.df, energyvol.df$name=="loadaft_offgrid")
  loadaft_offgrid=loadaft_offgrid[1:length(loadaft_offgrid)-1]
  loadaft_offgrid=loadaft_offgrid*res.load

  #prestep 5
  #=E7*'SAPN Static variables'!$F$63
  loadaft_eeffic=subset(energyvol.df, energyvol.df$name=="loadaft_eeffic")
  loadaft_eeffic=loadaft_eeffic[1:length(loadaft_eeffic)-1]
  loadaft_eeffic=loadaft_eeffic*res.load

  #prestep 6
  #loadaft_solar
  #=E54-('SAPN Static variables'!$F$64*E16)
  #e54=loadaft_eeffic
  res.solar=other.df$all.years[other.df$name=="res solar"] #f64
  #e16=rloadsolar
  rloadsolar=subset(energyvol.df, energyvol.df$name=="rloadsolar")
  rloadsolar=rloadsolar[1:length(rloadsolar)-1]
  loadaft_solar=loadaft_eeffic-(res.solar*rloadsolar)

  #prestep 7
  #loadaft_batt
  #=E55-('SAPN Static variables'!$F$65*E21)
  #e55=loadaft_solar
  res.batt=other.df$all.years[other.df$name=="res batteries"] #f65
  #e21=rloadbatt
  rloadbatt=subset(energyvol.df, energyvol.df$name=="rloadbatt")
  rloadbatt=rloadbatt[1:length(rloadbatt)-1]
  loadaft_batt=loadaft_solar-(res.batt*rloadbatt)

  #prestep 8 #load aft ev
  #=E56+('SAPN Static variables'!$F$62*'SAPN load growth'!E28)
  #e56=loadaft_batt
  res.ev=other.df$all.years[other.df$name=="residential EV"] #f65
  #e28=rloadev
  rloadev=subset(energyvol.df, energyvol.df$name=="rloadev")
  rloadev=rloadev[1:length(rloadev)-1]
  loadaft_ev=loadaft_batt+(res.ev*rloadev)

  #prestep 9 energy volumes=loadaft_ev (loadaft_ev includes solar, batteries,ev and offgrid)
  energy_vol=loadaft_ev


  #step2 and 3 rev.real.res
  tmp <- matrix(NA, ncol=length(yearslabel), nrow=1)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel
  rev.real.res=tmp
  #step 2
  #rev.real.res[1]=(D5*'SAPN Static variables'!F66)+('SAPN final price'!D6*1000000*'SAPN Static variables'!F67)
  #d5= ='SAPN load growth'!E52 = no customers on grid in 2021 = pre step 3 = cust.ogrid
  #d6 ='SAPN load growth'!E58 = energy volumes in 2021 = pre step 9 = energy_vol
  fixed.rate=other.df$all.years[other.df$name=="fixed rate"] #f66
  energy.rate=other.df$all.years[other.df$name=="energy rate"] #f67

  rev.real.res[1]=(cust.ogrid[1]*fixed.rate)+(round(energy_vol[1],digits=2)*1000000*energy.rate)
  #this changes depending on the number of decimal places - better to round everything
  #470307118 = excel (when both rounded)

  #step 3
  #=E3*($D$4/$D$3)
  rev.ratio=round(rev.real.res[1]/revenue.real,7)
  for(i in 2:length(rev.real.res)){
    rev.real.res[i]=(revenue.real[i])*rev.ratio
  }

  #step 4 percent fixed revenue
  #=('SAPN Static variables'!F66*'SAPN final price'!D6)/D5
  pfixed.rev=as.numeric(round(((fixed.rate*cust.ogrid[1])/rev.real.res[1]),digits=5))

  #step 5 percent energyvol
  penergy_vol=as.numeric(1-pfixed.rev)

  #step 6 fixed revenue
  #=$D$10*D5 = pfixed.rev * rev.real.res
  fixed.rev=pfixed.rev*rev.real.res

  #step 7 energy volumes revenue
  #=$D$11*D5 = penergy_vol*rev.real.res
  energy.vol.rev = penergy_vol*rev.real.res

  #step 8 and 9 fixed rate ($/res cust)
  #done backwards because it doesn't use i-1
  fixed.rate.pcust=fixed.rev/cust.ogrid
  fixed.rate.pcust[1]=fixed.rate #'SAPN Static variables'!F66
  fixed.rate.pcust=round(fixed.rate.pcust,digits=5)


  #steps 10 and 11 energy unit rate
  unit.rate = round((energy.vol.rev/round(energy_vol,digits = 0)/1000000),digits = 4) #=E14/E7/1000000
  unit.rate[1]=energy.rate
  unit.rate=round(unit.rate,digits=5)

  #step 13 (step 12 depreciated) 4000MW cust
  #cust4mw=D16+('SAPN Static variables'!$F$68*D17)
  #d16 = fixed.rate.pcust
  #f68=load.res.cust
  load.res.cust=other.df$all.years[other.df$name=="load res customer"] #f68 (percent)
  #d17=unit.rate
  cust4mw=fixed.rate.pcust+(load.res.cust*unit.rate)

  #step 14 4000mw cust with 1 ev
  #cust4mw.ev=D16+(('SAPN Static variables'!$F$68+'SAPN Static variables'!$F$69)*'SAPN final price'!D17)
  #d16= fixed.rate.pcust
  #f68=load.res.cust
  #f69 = load.p.ev
  load.p.ev=other.df$all.years[other.df$name=="load per EV"] #f69 (percent)
  #d17 = unit.rate
  cust4mw.ev=fixed.rate.pcust+((load.res.cust+load.p.ev)*unit.rate)

  #step 15 revenue from flat tariffs for 4000MW cust
  cust4mw.flat=cust4mw*cust.ogrid #D20*D6

  #step 16 revenue from 4000MW cust with ev
  cust4mw.ev.flat=cust4mw.ev*cust.ogrid #D21*D6

  #steps 17 and 18 energy unit rate by hour
  p.unitrate=punitrate_fun(yearslabel,unit.rate,signal.in)

  #nonsolar----
  #steps 19 and 20 consumption load profile for non-solar customers
  cons_nonsolar=cons_nonsolar_fun(yearslabel,priceprof.df,unit.rate,signal.in)

  #step 21 energy charges per hour non-solar customers
  #=D71*D40 (p.unitrate*cons_nonsolar)
  chargephr_nonsolar=p.unitrate*cons_nonsolar

  #step 22 yearly sum of energy charges non-solar
  chargesyr_nonsolar=colSums(chargephr_nonsolar)

  #step 23 fixed charges = fixed.rate per customer
  fixed.charges=fixed.rate.pcust

  #step 24 total annual bill before adjustment non-solar
  billyr_nonadj_nonsolar=round(chargesyr_nonsolar+fixed.charges,digits=5)

  #skip step 25

  #solar (no ev)----
  #steps 26-31
  #steps 26 and 27 consumption load profile for solar customers
  cons_solar=cons_solar_fun(yearslabel,priceprof.df,unit.rate,signal.in)

  #step 28 energy charges per hour solar customers
  #=D133*D40 (p.unitrate*cons_solar)
  chargephr_solar=p.unitrate*cons_solar

  #step 28(b) yearly sum of energy charges solar
  chargesyr_solar=colSums(chargephr_solar)

  #step 29 fixed charges = fixed.rate per customer
  fixed.charges=fixed.rate.pcust

  #step 30 total annual bill before adjustment solar
  billyr_nonadj_solar=round(chargesyr_solar+fixed.charges,digits=5)


  #pre steps a-e
  #step a
  #=(1-'SAPN load growth'!E15)*'SAPN final price'!D125*'SAPN final price'!D6
  #e15 %customers with solar  from energyvol.df step 6
  solar.pen=subset(energyvol.df, energyvol.df$name=="psolar")
  solar.pen=round(solar.pen[1:length(solar.pen)-1],digits=5)
  #d125 = billyr_nonadj_nonsolar #step 24
  #d6 = cust_ogrid
  a=(1-solar.pen)*billyr_nonadj_nonsolar*cust.ogrid # from tou 4000MW customer

  #b=D186*'SAPN load growth'!E15*'SAPN final price'!D6
  #d186 =billyr_nonadj_solar
  b= billyr_nonadj_solar*solar.pen*cust.ogrid #revenue from solar tou customers without ev
  c=a+b #total revenue under tou
  tot.rev.tou=c

  rev.gap=cust4mw.flat-tot.rev.tou #d=step 15 - c
  adjust.meet.rev=rev.gap/cust.ogrid #e

  #step 25 total annual bill with adjustment non-solar
  billyr_nonsolar=billyr_nonadj_nonsolar+adjust.meet.rev

  #step 31 total annual bill with adjustment solar (no ev)
  billyr_solar=billyr_nonadj_solar+adjust.meet.rev

  #step 32-51 repeat of above but with batteries
  #solar and battery customers (no ev)

  #solar and battery----
  #steps 32 and 33 consumption load profile for solar and battery customers
  cons_solarbatt=cons_solarbatt_fun(yearslabel,priceprof.df,unit.rate,signal.in)

  #step 32 (should be 34 but ignore for now) energy charges per hour solar and batt customers
  #=D193*D40 (p.unitrate*cons_solarbatt)
  chargephr_solarbatt=p.unitrate*cons_solarbatt

  #step 34 yearly sum of energy charges solar and batt
  chargesyr_solarbatt=colSums(chargephr_solarbatt)

  #step 35 fixed charges = fixed.rate per customer
  fixed.charges=fixed.rate.pcust

  #step 36 total annual bill before adjustment solar and batt
  billyr_solarbatt=round(chargesyr_solarbatt+fixed.charges,digits=5)

  #no adjustment

  #nonsolar ev----
  #steps 37-42
  #steps 37 and 38 consumption load profile for non-solar customers with 1 ev
  cons_nonsolarev=cons_nonsolarev_fun(yearslabel,priceprof.df,unit.rate,signal.in)

  #step 39 energy charges per hour nonsolarev customers
  #=D254*D40 (p.unitrate*cons_nonsolarev)
  chargephr_nonsolarev=p.unitrate*cons_nonsolarev

  #step 40 yearly sum of energy charges solar
  chargesyr_nonsolarev=colSums(chargephr_nonsolarev)

  #step 41 fixed charges = fixed.rate per customer
  fixed.charges=fixed.rate.pcust

  #step 42 total annual bill before adjustment nonsolarev
  billyr_nonadj_nonsolarev=round(chargesyr_nonsolarev+fixed.charges,digits=5)

#  now need steps 44-49
  #solar ev----
  #steps 44-49
  #steps 44 and 45 consumption load profile for solarev customers
  cons_solarev=cons_solarev_fun(yearslabel,priceprof.df,unit.rate,signal.in)

  #step 46 energy charges per hour solarev customers
  #=D314*D40 (p.unitrate*cons_solar)
  chargephr_solarev=p.unitrate*cons_solarev

  #step 47 yearly sum of energy charges solarev
  chargesyr_solarev=colSums(chargephr_solarev)

  #step 48 fixed charges = fixed.rate per customer
  fixed.charges=fixed.rate.pcust

  #step 49 total annual bill before adjustment solarev
  billyr_nonadj_solarev=round(chargesyr_solarev+fixed.charges,digits=5)


  #pre steps a-e
  #step a
  #=(1-'SAPN load growth'!E$15)*'SAPN final price'!D$6*'SAPN final price'!D307
  #e15 %customers with solar  from energyvol.df step 6 = solar.pen
  #d307 = billyr_nonadj_nonsolarev #step 42
  #d6 = cust_ogrid
  a=(1-solar.pen)*billyr_nonadj_nonsolarev*cust.ogrid # from tou 4000MW customers with ev

  #b=D367*'SAPN load growth'!E15*'SAPN final price'!D6
  #d367 =billyr_nonadj_solarev - step 49
  b= billyr_nonadj_solarev*solar.pen*cust.ogrid #revenue from solarev tou customers with ev
  c=a+b #total revenue under tou with ev
  tot.rev.touev=c

  rev.gap.ev=cust4mw.ev.flat-tot.rev.touev #d=step 16 - c
  adjust.meet.rev.ev=rev.gap.ev/cust.ogrid #e

  #step 43 total annual bill with adjustment non-solar with ev
  billyr_nonsolarev=billyr_nonadj_nonsolarev+adjust.meet.rev.ev

  #step 50 total annual bill with adjustment solar with ev
  billyr_solarev=billyr_nonadj_solarev+adjust.meet.rev.ev

  #solar and batt ev----
  #steps 51 and 52 consumption load profile for solar and battery ev customers
  cons_solarbattev=cons_solarbattev_fun(yearslabel,priceprof.df,unit.rate,signal.in)

  #step 53 energy charges per hour solar and batt ev customers
  #=D374*D40 (p.unitrate*cons_solarbattev)
  chargephr_solarbattev=p.unitrate*cons_solarbattev

  #step 54 yearly sum of energy charges solar and batt ev
  chargesyr_solarbattev=colSums(chargephr_solarbattev)

  #step 55 fixed charges = fixed.rate per customer
  fixed.charges=fixed.rate.pcust

  #step 56 total annual bill before adjustment solar and batt
  billyr_solarbattev=round(chargesyr_solarbattev+fixed.charges,digits=5)
  #no adjustment

  #export data ----

  #no ev
  billyr_nonsolart=as.data.frame(t(billyr_nonsolar))
  billyr_nonsolart$name="billyr_nonsolar"
  billyr_nonsolart$label="Non-solar customer"

  billyr_solart=as.data.frame(t(billyr_solar))
  billyr_solart$name="billyr_solar"
  billyr_solart$label="Solar customer"

  billyr_solarbattt=as.data.frame(t(billyr_solarbatt))
  billyr_solarbattt$name="billyr_solarbatt"
  billyr_solarbattt$label="Solar and batteries customer"


  data=rbind.data.frame(billyr_nonsolart,billyr_solart,billyr_solarbattt)
  yeard=c(yearslabel+2000,yearslabel+2000,yearslabel+2000)
  data$year=yeard
  data$type="non-ev"
  names(data)=c("price","name","label","year","type")


  #ev
  #billyr_nonsolarev
  #billyr_solarev
  #billyr_solarbattev
  billyr_nonsolarevt=as.data.frame(t(billyr_nonsolarev))
  billyr_nonsolarevt$name="billyr_nonsolarev"
  billyr_nonsolarevt$label="Non-solar EV customer"

  billyr_solarevt=as.data.frame(t(billyr_solarev))
  billyr_solarevt$name="billyr_solarev"
  billyr_solarevt$label="Solar & EV customer"

  billyr_solarbattevt=as.data.frame(t(billyr_solarbattev))
  billyr_solarbattevt$name="billyr_solarbattev"
  billyr_solarbattevt$label="Solar, batteries & EV customer"



  dataev=rbind.data.frame(billyr_nonsolarevt,billyr_solarevt,billyr_solarbattevt)
  dataev$year=yeard
  dataev$type="ev"
  names(dataev)=c("price","name","label","year","type")

  dataf=rbind(data,dataev)

  return(dataf)

  }

