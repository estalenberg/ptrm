#' Peak demand functions
#'
#' Formula for calculating the peak demand profile with dynamic price signal
#'
#'
#' @param dnsp.in DNSP selected for function loop
#' @param other.df database of static dnsp inputs
#' @param pdprof.df database of static dnsp inputs for peak demand profile
#' @param energyvol.df database of energy vol function ouput
#' @param projyearend.in dynamic variable of final year
#' @param use.in dynamic variable of growth of energy use per customer
#' @param signal.in dynamic variable of dynamic price signal
#' @export
#'
allpeakdemand_fun=function(dnsp.in,other.df,pdprof.df,projyearend.in,energyvol.df,use.in,signal.in){
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


  #time line ----
  projyearend = projyearend.in
  startyearend=other.df$all.years[other.df$name=="year start"]
  noyears=(projyearend-startyearend)
  years=1:noyears
  yearslabel=(startyearend:projyearend)
  yearslabel=yearslabel[2:length(yearslabel)]

  #step 2 and 3 #pd adjusted for off grid customers
  pdoffgrid=pdoffgrid_fun(yearslabel,energyvol.df,pdprof.df)

  #step 4 #pd adjusted for energy efficiency
  pdeeffic=pdenergyeffic_fun(yearslabel,use.in,pdoffgrid)

  #steps 5 to 9 pd adjusted for offgrid cust, ee and solar
  pdsolar=pdsolar_fun(yearslabel,pdprof.df,energyvol.df,pdeeffic)

  #steps 10 -14 pd adjusted for offgrid cust, ee, solar and batteries
  pdbatt=pdbattery_fun(yearslabel,pdprof.df,energyvol.df,pdsolar)

  #steps 15-19 pd adjusted fro offgrid, ee, solar, batteries and electric vehicles
  pd_ev=pdevehicles_fun(yearslabel,pdprof.df,energyvol.df,pdbatt)

  #steps 20-24: final dynamic price signal
  pd_finalsignal=pdfinalsignal_fun(yearslabel,pd_ev,signal.in)

  return(pd_finalsignal)
  }

