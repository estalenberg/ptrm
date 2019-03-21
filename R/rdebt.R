#' Rate of debt function
#'
#' Trailing average portfolio return on debt for each year in model
#'
#' rate of debt changes with year starting from the first value (debt start) and reducing by debt reduce
#' rate of debt is capped at the fifth year (2024) and then made into a dataframe with years as column headings.
#'
#' Prerequisites that need to be defined in ptrm model (i.e. run above code):
#' year, debtstart, debtfinal, debtreduce, startyearend, projyearend

#' @param y year vector of 1:noyears
#' @param debtstart defined above from dnsp other.df dataframe
#' @param debtreduce defined above from dnsp other.df dataframe
#' @param debtfinal defined above from dnsp other.df dataframe
#' @param startyearend always 2019 or 19
#' @param projyearend dynamic input to define the end year of the modelling
#' @keywords debt, rate, return on debt
#' @export
#' @examples
#' projyearend = 97
#' startyearend = 19
#' noyears = projyearend-startyearend
#' debtstart=0.057231
#' debtreduce=0.00205
#' debtfinal=0.049038
#' years=1:noyears
#'
#'
rdebt_fun=function(y,debtstart,debtfinal,debtreduce,startyearend,projyearend)
{

  if(length(y)<2){y=debtstart}else{
    y[1]=debtstart
    for(i in 2:length(y))  # pardon the case where x is of length 1 or 0
      if((y[i-1]-debtreduce)<0){y[i]=0}else #if rate of debt is <0 make 0
      {y[i]=(y[i-1]-debtreduce)}
  }
  y=ifelse(y<debtfinal,debtfinal,y)
  label=(startyearend+1):projyearend
  df=as.data.frame(matrix(NA,ncol=length(label),nrow=1))
  names(df)=label
  df[1,]=y
  y=df
  y
}
