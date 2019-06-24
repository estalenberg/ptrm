#' Peak demand profile for solar
#'
#' Formula for calculating the peak demand profile after adjusting for off grid customers, energy efficiency and solar
#'
#' @param yearslabel label of projection years from 2020
#' @param pdprof.df peak demand profile per hour input dataframe
#' @param energyvol.df modeled outputs of energy volumes from allenergyvol_fun
#' @param pdeeffic peak demand adjusted for energy efficiency
#'
#' @export
#'
pdsolar_fun=function(yearslabel,pdprof.df,energyvol.df,pdeeffic){

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

  #step 8 correct for reduction in load by solar per hour profile
  #need an empty matrix for noyears*24
  tmp <- matrix(NA, ncol=length(yearslabel), nrow=24)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel
  pd=tmp

  for(i in 1:length(pd)){
    pd[i]=solarconsum*solarmw[i]
  }

  prsolarmw=pd

#  step 9 = #peak demand after adjusting for offgrid, energy efficiency and solar  (step 4 - step 8)

  for(i in 1:length(pd)){
    pd[i]=pdeeffic[i] - prsolarmw[i]
  }

    return(pd)
  }

