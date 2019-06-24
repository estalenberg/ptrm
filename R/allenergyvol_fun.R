#' Energy volume functions
#'
#' Formula for calculating the energy volume distribution among customers
#'
#'
#' @param dnsp.in DNSP selected for function loop
#' @param other.df database of static dnsp inputs
#' @param cars.in dynamic variable for electric vehicle penetration by 2060 as a percent
#' @param projyearend.in dynamic variable of final year
#' @param cust.in dynamic variable of growth of customers percent
#' @param ogrid.in dynamic variable of offgrid customers by 2060 percent
#' @param solar.in dynamic variable of percent of solar penetration by 2060
#' @param batt.in dynamic variable of percent of battery penetration by 2060
#' @param use.in dynamic variable of growth of energy use per customer
#' @export
#'
allenergyvol_fun=function(dnsp.in,other.df,projyearend.in,use.in, cust.in,cars.in,ogrid.in,solar.in,batt.in){
  #cut to dnsp
  d.name=c("Energex","SAPN")
  d.code=1:2
  dnsp.df=cbind.data.frame(d.code,d.name)
  dnsp=as.numeric(dnsp.df$d.code[dnsp.df$d.name==dnsp.in])
  dnsp.label=as.character(dnsp.in)

  other.df=other.df[other.df$dnsp==(as.numeric(dnsp)),]
  other.df=subset(other.df,(!is.na(other.df$dnsp)))

  #time line ----
  projyearend = projyearend.in
  startyearend=other.df$all.years[other.df$name=="year start"]
  noyears=(projyearend-startyearend)
  years=1:noyears
  yearslabel=(startyearend:projyearend)
  yearslabel=yearslabel[2:length(yearslabel)]


# customer number functions
  #1
  custnum=cust_fun(yearslabel,other.df,cust.in)
  #2
  poffgrid=poffgrid_fun(other.df,yearslabel,ogrid.in)
  #3
  custongrid=custnum*(1-poffgrid)
  #4
  epcust2020=other.df$all.years[other.df$name=="energy p cust 2020"]
  loadaft_offgrid=custongrid*epcust2020
  #5
  loadaft_eeffic=loadafteff_fun(yearslabel,other.df,use.in,loadaft_offgrid)
  #6
  psolar=psolar_fun(other.df,yearslabel,solar.in)
  #7
  rloadsolar=rloadsolar_fun(yearslabel,other.df,custongrid,psolar)
  #8
  loadaft_solar=loadaft_eeffic-rloadsolar
  #9
  pbatt=pbatt_fun(other.df,yearslabel,batt.in)
  #10
  rloadbatt=rloadbatt_fun(yearslabel,other.df,custongrid,pbatt)
  #11
  loadaft_batt=loadaft_solar-rloadbatt
  #12
  carnum=cars_fun(yearslabel,other.df)
  #13
  evpenetration=carpenet_fun(yearslabel,cars.in,other.df)
  #14
  numberev=carnum*evpenetration
  #15
  evload=other.df$all.years[other.df$name=="ev load"]
  loadinc_ev=numberev*evload
  #16 (11+15)
  loadaft_ev=loadinc_ev+loadaft_batt
  #17
  energyvol=loadaft_ev


  #for price functions
  tmp <- matrix(NA, ncol=length(yearslabel), nrow=1)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel
  energygrowthrate=tmp
  energypcustgrowthrate=tmp

  #energy growth rate
    energygrowthrate[1]=0
  for(i in 2:length(tmp))
    energygrowthrate[i]= (energyvol[i]-energyvol[i-1])/energyvol[i-1]

  #energy per customer (on grid)
  energypcust=energyvol/custongrid

  #energy per customer growth rate
    energypcustgrowthrate[1]=0
  for(i in 2:length(tmp))
    energypcustgrowthrate[i]=(energypcust[i]-energypcust[i-1])/energypcust[i-1]

  data=rbind.data.frame(custongrid,energyvol,energypcustgrowthrate,rloadsolar,rloadbatt,loadinc_ev)
  names=c("Customers on grid", "Energy volumes", "energypcustgrowthrate","rloadsolar","rloadbatt","rloadev")
  data$names=names

  return(data)
  }

