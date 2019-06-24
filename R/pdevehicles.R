#' Peak demand profile for electric vehicles
#'
#' Formula for calculating the peak demand profile after adjusting for off grid customers, energy efficiency,
#' solar, batteries and electric vehicles
#'
#' @param yearslabel label of projection years from 2020
#' @param pdprof.df peak demand profile per hour input dataframe
#' @param energyvol.df modeled outputs of energy volumes from allenergyvol_fun
#' @param pdbatt peak demand adjusted for offgrid, ee, solar and batteries
#'
#' @export
#'
pdevehicles_fun=function(yearslabel,pdprof.df,energyvol.df,pdbatt){

  #step 15
  #% reduction to load from ev
  rloadev=(subset(energyvol.df,energyvol.df$names=="rloadev"))
  rloadev=as.numeric(rloadev[1:length(rloadev)-1])
  evolumes=(subset(energyvol.df,energyvol.df$names=="Energy volumes"))
  evolumes=as.numeric(evolumes[1:length(evolumes)-1])
  rloadevp=rloadev/evolumes

  #step 16  sum of peak demand with off grid, energy effic, solar and batteries (pdbatt) sum of step 14
  tot_batt=colSums(pdbatt)
  #prop reduction to batteries in MW
  ev_mw=tot_batt*rloadevp

  #step 17
  evconsum=pdprof.df$ev

  #step 18 correct for reduction in load by ev per hour profile
  #need an empty matrix for noyears*24
  tmp <- matrix(NA, ncol=length(yearslabel), nrow=24)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel
  pd=tmp

  for(i in 1:length(pd)){
    pd[i]=evconsum*ev_mw[i]
  }

  prevmw=pd

#  step 19 = #peak demand after adjusting for offgrid, energy efficiency, solar and batteries  (steps 15 - 19)
#step 14 + 18
  for(i in 1:length(pd)){
    pd[i]=pdbatt[i] + prevmw[i]
  }

    return(pd)
  }

