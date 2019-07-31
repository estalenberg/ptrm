#' Energy volume functions
#'
#' Formula for calculating the energy volume distribution among customers
#'
#'
#' @param dnsp.in DNSP selected for function loop
#' @param assets.df database of static dnsp inputs for all assets
#' @param other.df database of static dnsp inputs
#' @param iab.df database of static dnsp inputs for iab and tax iab
#' @param priceprof.df dataframe of static dnsp price profile inputs
#' @param projyearend.in dynamic variable of final year
#' @param age.in dynamic variable of age of assets
#' @param retireslim.in dynamic variable of slim assets
#' @param addnew.in dynamic variable of add new assets
#' @param productivity.in dynamic variable of productivity
#' @param rba.in dynamic variable of RBA cash rate
#' @param cars.in dynamic variable for electric vehicle penetration by 2060 as a percent
#' @param cust.in dynamic variable of growth of customers percent
#' @param ogrid.in dynamic variable of offgrid customers by 2060 percent
#' @param solar.in dynamic variable of percent of solar penetration by 2060
#' @param batt.in dynamic variable of percent of battery penetration by 2060
#' @param use.in dynamic variable of growth of energy use per customer
#' @param signal.in dynamic variable of price signal text input
#' @export
#'
loopall_fun=function(assets.df,other.df, iab.df, priceprof.df,projyearend.in, age.in, retireslim.in,addnew.in,
                     productivity.in,dnsp.in, rba.in,use.in, cust.in,cars.in,ogrid.in,solar.in,batt.in,
                     signal.in){


  #call functions ptrm, energyvol and price (not demand) from package
  #demand.in will be here
  df.real=ptrm_fun(assets.df,other.df, iab.df, projyearend.in, age.in, retireslim.in,addnew.in,
                   productivity.in,dnsp.in, rba.in)


  #call energy volumes with electric cars #subsets dnsp inside function
  energyvol.df=allenergyvol_fun(dnsp.in,other.df,projyearend.in,use.in, cust.in,cars.in,ogrid.in,solar.in,batt.in)


  #call price function
  price.df=price_fun(dnsp.in,df.real,energyvol.df,other.df,priceprof.df,projyearend.in,cust.in,signal.in)

  #rev_pcust=subset(price.df,price.df$names=="rev_pcust")
  rev_pGWh=subset(price.df,price.df$names=="rev_pGWh") # which one?


  #mapping variable: #one number
  pchange2160_pGWh=((rev_pGWh[40]-rev_pGWh[1])/rev_pGWh[1])*100


  return(pchange2160_pGWh)
  }

