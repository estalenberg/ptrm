#' WARL formulas
#'
#' Weighted average remaining life method for real depreciation
#'
#' Function calculates only one asset class: ac, which is specified by user. The function has to be looped to project the WARL depreciation for all asset classes.
#' Prerequisites that need to be defined in ptrm model, i.e. run above code: ac, yearslabel, fcnetavg.full, rvanilla, remlife, stdlife, oavalue, noyears
#' @param noassets count of asset classes
#' @param yearslabel from 2020 to projected year end
#' @param noyears count of years
#' @param startyearend first regulatory year (2019 or 2020)
#' @param projyearend final projected year
#' @param fcnetavg.full dataframe of all netcapex for all assets defined by netcapex function
#' @param fccust.full dataframe of all customer contribution for all assets
#' @param cif.df dataframe of cumulative inflation
#' @param taxiabcheck dataframe of raw tax iab values from ptrm for the first ten years
#' @param taxremlife vector of tax remaining life of assets from dnsp assets.df input dataframe
#' @param taxstdlife vector of tax standard life of assets from dnsp assets.df input dataframe
#' @param taxoavalue vector of tax opening asset value of assets from dnsp assets.df input dataframe
#' @keywords tax, year on year, depreciation, capex, remlife, stdlife
#' @export

taxyoyall_fun <- function(noassets, yearslabel, noyears, taxremlife, taxoavalue,
                       startyearend,projyearend,taxstdlife,fcnetavg.full,fccust.full,cif.df,taxiabcheck){

  #loop to calculate for each asset class
  assetclass=1:noassets
  taxiab.yrs=1:noyears
  result <- matrix(NA, ncol=length(yearslabel), nrow=length(assetclass))
  result=as.data.frame(result)
  names(result)=yearslabel


  for(j in 1:length(assetclass)){
    result[j,]=taxiab_fun(taxiab.yrs,taxremlife,taxoavalue,assetclass[j])}

  taxiab.all=result #initial asset base value for all asset classes in each projection year

  #the tax declines rapidly using the ausgrid method but only slowly using the sa method
  # need to substitue the sa data in to the tax iab calc (prepared already in iab.df)
  #but energex doesnt have any taxiab data
  if(is.na(taxiabcheck[1,1])){
    taxiab.all=result
  } else {
    for (i in 1:noassets){
      (taxiab.all[i,1:length(taxiabcheck[i,])]= taxiabcheck[i,])
    }
  }

  #substite na for 0
  baseR.na   <- function(x) { x[is.na(x)] <- 0; x }
  taxiab.all=baseR.na(taxiab.all)

  #tax YOY depreciation ----
  #e.g. excel assets H482
  #=IF(A1taxstdlife="n/a","n/a",
  #IF(H$3>(A1taxstdlife+$E482),(('PTRM input'!$G$143+'PTRM input'!$G$109)*$G$7)-SUM($G482:G482),
  #(('PTRM input'!$G$143+'PTRM input'!$G$109)*$G$7)/A1taxstdlife))

  fctaxcapex.df=fcnetavg.full+fccust.full

  #loop to calculate each capex stage in each asset
  capexstage=1:noyears
  capexyears=(startyearend:projyearend) #will delete 2019 in next step
  result2 <- matrix(NA, ncol=length(capexyears), nrow=length(capexstage))
  result2=as.data.frame(result2)
  names(result2)=capexyears

  datalist = list()

  for (i in 1:noassets) {
    capexstage=1:noyears
    assetclass=i
    for(j in 1:length(capexstage)){
      result2[j,]=taxyoy_fun(capexyears,capexstage[j],assetclass, taxstdlife, startyearend, cif.df, fctaxcapex.df)}

    taxcapex.res=result2

    #sum all capex stages for each year - i.e. column total
    taxsumcapex= colSums (taxcapex.res, na.rm = T, dims = 1)
    datalist[[i]] <- taxsumcapex # add it to your list

  }

  #stop()
  big_data = do.call(rbind, datalist)

  df=as.data.frame(big_data)

  drops=as.character(startyearend)  #drop the year 2018-19 if it's there
  df=df[,(!names(df)%in% drops)]

  #then add the iab for each year
  taxsumcapex= df + taxiab.all

  return(taxsumcapex)
}


