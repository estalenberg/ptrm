#' WARL formulas
#'
#' Weighted average remaining life method for real depreciation
#'
#' Function calculates only one asset class: ac, which is specified by user. The function has to be looped to project the WARL depreciation for all asset classes.
#' Prerequisites that need to be defined in ptrm model, i.e. run above code: ac, yearslabel, fcnetavg.full, rvanilla, remlife, stdlife, oavalue, noyears
#' @param noassets count of asset classes
#' @param yearslabel from 2020 to projected year end
#' @param years list of regulatory years from 1
#' @param noyears count of years
#' @param startyearend first regulatory year (2019 or 2020)
#' @param projyearend final projected year
#' @param fcnetavg.full dataframe of all netcapex for all assets defined by netcapex function
#' @param iabcheck dataframe of raw iab values from ptrm for the first ten years
#' @param rvanilla vector of real vanilla WACC
#' @param remlife vector of remaining life of assets from dnsp assets.df input dataframe
#' @param stdlife vector of standard life of assets from dnsp assets.df input dataframe
#' @param oavalue vector of opening asset value of assets from dnsp assets.df input dataframe
#' @keywords year on year, depreciation, capex, remlife, stdlife, vanilla WACC
#' @export

yoyall_fun <- function(noassets, yearslabel, years, noyears, remlife, oavalue,
                     startyearend,projyearend,stdlife,fcnetavg.full,rvanilla,iabcheck){

  assetclass=1:noassets
  result <- matrix(NA, ncol=length(yearslabel), nrow=length(assetclass))
  result=as.data.frame(result)
  names(result)=yearslabel

  for(j in 1:length(assetclass)){
    result[j,]=iab_fun(years,remlife,oavalue,assetclass[j])}

  iab.all=result #dataframe of initial asset base value for all asset classes in each projection year

  #row=5 #lvs - the sa ptrm counts down to at 5 years 17 and then stays at 17, but the remaining life is 20 years
  #iab.all[row,1:10]
  #iabcheck[row,1:10]

  #substitute the first 10 predictions for the raw values in iabcheck
  for (i in 1:noassets){
    (iab.all[i,1:length(iabcheck[i,])]= iabcheck[i,])
  }
  #substite na for 0
  baseR.na   <- function(x) { x[is.na(x)] <- 0; x }
  iab.all=baseR.na(iab.all)

  #yoy capex----
  #loop to calculate each capex stage
  capexstage= 1:noyears #1:5 #this needs to be from 1 to the number of projected years
  capexyears=(startyearend:projyearend) #will delete 2019 in next step
  result2 <- matrix(NA, ncol=length(capexyears), nrow=length(capexstage)) #matrix for each asset class
  result2=as.data.frame(result2)
  names(result2)=capexyears

  datalist = list()

  #loop for each asset class
  for (i in 1:noassets) {
    capexstage=1:noyears #1:noyears or 1:5 to keep consistent with ptrm - this says how many capex stages to predict, either keep going through the years, or just do 5 years
    assetclass=i
    for(j in 1:length(capexstage)){
      result2[j,]=yoydeprec_fun(capexyears,capexstage[j],assetclass, stdlife,startyearend,fcnetavg.full,rvanilla)}

    capex.res=result2

    #now need to sum all capex stages for each year - i.e. column total
    sumcapex= colSums (capex.res, na.rm = T, dims = 1)
    datalist[[i]] <- sumcapex # add it to your list
  }

  big_data = do.call(rbind, datalist)
  df=as.data.frame(big_data)

  drops=as.character(startyearend) #drop the starting year if it's there
  df=df[,(!names(df)%in% drops)]

  sumcapex= df + iab.all #=this is initial asset base plus sum of capex for each year, for each asset class
  return(sumcapex)
}

