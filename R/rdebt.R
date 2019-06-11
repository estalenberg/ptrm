#' Rate of debt function
#'
#' Trailing average portfolio return on debt for each year in model
#'
#' rate of debt changes with year starting from the first value (debt start) and reducing by debt reduce
#' rate of debt is capped at the fifth year and then made into a dataframe with years as column headings.
#'
#' Prerequisites that need to be defined in ptrm model (i.e. run above code):
#' year, debtstart, debtfinal, debtreduce, startyearend, projyearend

#' @param other.df DNSP other.df dataframe
#' @param yearslabel label of projection years from 2020
#' @param rba dynamic input of rba cash rate
#' @keywords rba, debt, return on debt
#' @export
#'
#'
rdebt_fun=function(other.df,yearslabel,rba)
{
  debtstart=other.df$'1'[other.df$name=="Trailing Average Portfolio Return on Debt"]
  debtreduce=round((other.df$'1'[other.df$name=="Trailing Average Portfolio Return on Debt"]-
                      other.df$'2'[other.df$name=="Trailing Average Portfolio Return on Debt"]),digits=5)
  debtfinal=other.df$'5'[other.df$name=="Trailing Average Portfolio Return on Debt"]
  todayrba=other.df$all.years[other.df$name=="rba today"]

  tmp <- matrix(NA, ncol=length(yearslabel), nrow=1)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel
  rd=tmp

  rd[1]=debtstart
  for(i in 2:5)
    rd[i]=((rd[i-1]-debtreduce)) #make each year decrease by debt reduct until year 5
  for (i in 6:length(rd))
    rd[i]=ifelse((yearslabel[i]<40),(rd[i-1]+((rba-todayrba)/15)),(rd[5]+(rba-todayrba)))
  rd

}

