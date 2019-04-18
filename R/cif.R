#' CIF function
#'
#' Cumulative inflation risk for each year in model
#'
#' Prerequisites that need to be defined in ptrm model (i.e. run above code):
#' cifyear, infl, startyearend, projyearend

#' @param y cifyear vector of 1:noyears+1 cif is moved across one year
#' @param infl inflation from dnsp other.df dataframe
#' @param startyearend always 2019 or 19
#' @param projyearend dynamic input to define the end year of the modelling
#' @keywords cif, inflation
#' @export
#' @examples
#' projyearend = 97
#' startyearend = 19
#' noyears = projyearend-startyearend
#' infl=0.02425
#' cifyears=1:noyears+1
#'
#'
cif_fun=function(y, infl, startyearend, projyearend)
{
  if(length(y)<2){y=1}else{
    for(i in 2:length(y))
      y[i]=(y[i-1]*(1+infl))}
  label=(startyearend:projyearend)
  df=as.data.frame(matrix(NA,ncol=length(label),nrow=1))
  names(df)=label
  df[1,]=y
  y=df
  y
}


