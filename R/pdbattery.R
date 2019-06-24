#' Peak demand profile for batteries
#'
#' Formula for calculating the peak demand profile after adjusting for off grid customers, energy efficiency,
#' solar and batteries
#'
#' @param yearslabel label of projection years from 2020
#' @param pdprof.df peak demand profile per hour input dataframe
#' @param energyvol.df modeled outputs of energy volumes from allenergyvol_fun
#' @param pdsolar peak demand adjusted for offgrid, ee and solar
#'
#' @export
#'
pdbattery_fun=function(yearslabel,pdprof.df,energyvol.df,pdsolar){

  #step 10
  #% reduction to load from solar (vector across years)
  rloadbatt=(subset(energyvol.df,energyvol.df$names=="rloadbatt"))
  rloadbatt=as.numeric(rloadbatt[1:length(rloadbatt)-1])
  evolumes=(subset(energyvol.df,energyvol.df$names=="Energy volumes"))
  evolumes=as.numeric(evolumes[1:length(evolumes)-1])
  rloadbattp=rloadbatt/evolumes

  #step 11  sum of peak demand with energy effic and solar (pdsolar) sum of step 9
  tot_solar=colSums(pdsolar)
  #prop reduction to batteries in MW
  battmw=tot_solar*rloadbattp

  #step 12
  battconsum=pdprof.df$battery

  #step 13 correct for reduction in load by batt per hour profile
  #need an empty matrix for noyears*24
  tmp <- matrix(NA, ncol=length(yearslabel), nrow=24)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel
  pd=tmp

  for(i in 1:length(pd)){
    pd[i]=battconsum*battmw[i]
  }

  prbattmw=pd

#  step 14 = #peak demand after adjusting for offgrid, energy efficiency, solar and batteries  (steps 10 - 14)

  for(i in 1:length(pd)){
    pd[i]=pdsolar[i] - prbattmw[i]
  }

    return(pd)
  }

