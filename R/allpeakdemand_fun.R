#' Energy volume functions
#'
#' Formula for calculating the energy volume distribution among customers
#'
#'
#' @param dnsp.in DNSP selected for function loop
#' @param other.df database of static dnsp inputs
#' @param pdprof.df database of static dnsp inputs for peak demand profile
#' @param energyvol.df database of energy vol function ouput
#' @param projyearend.in dynamic variable of final year
#' @param use.in dynamic variable of growth of energy use per customer
#' @export
#'
allenergyvol_fun=function(dnsp.in,other.df,pdprof.df,projyearend.in,energyvol.df,use.in){
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

  #step 5-9 #pd adjusted for solar
  #step 5
  #% reduction to load from solar (vector across years)
  rloadsolar=(subset(energyvol.df,energyvol.df$names=="rloadsolar"))
  rloadsolar=as.numeric(rloadsolar[1:length(rloadsolar)-1])
  evolumes=(subset(energyvol.df,energyvol.df$names=="Energy volumes"))
  evolumes=as.numeric(evolumes[1:length(evolumes)-1])

  #rloadsolarprop='SAPN load growth  '!E16/'SAPN load growth  '!E11
  rloadsolarp=rloadsolar/evolumes
  #step 6  sum of peak demand with energy effic (pdeeffic)
  tot_eeffic=colSums(pdeeffic)
  #prop reduction to solar in MW
  solarmw=tot_eeffic*rloadsolarp

  #step 7
  solarconsum=pdprof.df$solar



  data=rbind.data.frame(custongrid,energyvol,energypcustgrowthrate)
  names=c("Customers on grid", "Energy volumes", "energypcustgrowthrate")
  data$names=names

  return(data)
  }

