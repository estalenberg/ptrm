#' Net tax functions
#'
#' Formulas for calculating the net tax allowance by years
#'
#' @param yearslabel label of years from year two to projected year end
#' @param projyearend final projected year
#' @param startyearend start of projections
#' @param other.df dataframe of PTRM input other values
#' @param tax.df dataframe of tax calculations for 2019 onwards
#' @export
#'

nettax_fun <- function(yearslabel, startyearend,projyearend, other.df, tax.df){

  res <- matrix(0, ncol=length(yearslabel)+1, nrow=1) #one at a time
  res=as.data.frame(res)
  taxyearslabel=startyearend:projyearend
  names(res)=taxyearslabel
  imputationcredits=other.df$all.years[other.df$name=="Value of Imputation Credits (gamma)"]

  #year 2019 df[,1]
  #taxlosscarriedforward (for 2019)
  taxloss=res
  taxloss1=other.df$all.years[other.df$name=="Tax Loss Carried Forward"]
  taxloss[,1]=taxloss1

  #split tax.df
  nomequityreturn=subset(tax.df,tax.df$values=="nomequityreturn")
  nomdebtreturn=subset(tax.df,tax.df$values=="nomdebtreturn")
  returncapitalregdeprec=subset(tax.df,tax.df$values=="returncapitalregdeprec")
  opex=subset(tax.df,tax.df$values=="opex")
  revadjust=subset(tax.df,tax.df$values=="revadjust")
  fccustindex=subset(tax.df,tax.df$values=="fccustindex")
  nontaxincome=subset(tax.df,tax.df$values=="nontaxincome")
  totaltaxexpense=subset(tax.df,tax.df$values=="totaltaxexpense")
  corptaxrate=subset(tax.df,tax.df$values=="corptaxrate")


  sum=res
  sum[2]=((nomequityreturn[2]+nomdebtreturn[2]+returncapitalregdeprec[2]+opex[2]+revadjust[2]+
            fccustindex[2]+nontaxincome[2]-totaltaxexpense[2]+taxloss[1])*
           corptaxrate[2])/(1-(1-imputationcredits)*corptaxrate[2])

  immediatetax=res
  immediatetax[2]=max(0,as.numeric(sum[2]), na.rm=T)

  revenuesubtotal=res
  revenuesubtotal[2] = nomequityreturn[2]+nomdebtreturn[2]+returncapitalregdeprec[2]+
    opex[2]+revadjust[2]+immediatetax[2]+(-immediatetax[2]*imputationcredits)

  revtaxassess=res
  revtaxassess[2]=revenuesubtotal[2]+fccustindex[2]+nontaxincome[2]

  pretaxincome=res
  pretaxincome[2]=revtaxassess[2]-totaltaxexpense[2]

  taxableincome=res
  taxableincome[2]=pretaxincome[2]+taxloss[1]

  taxpayable=res
  taxpayable[2]=as.numeric(ifelse(taxableincome[2]>0,corptaxrate[2]*taxableincome[2],0))

  valueimputationcredits=res
  valueimputationcredits[2]=-immediatetax[2]*imputationcredits

  nettaxallowance=res
  nettaxallowance[2]=immediatetax[2]+valueimputationcredits[2]


  #next years, from y3 (2021 onwards):
  for (i in 3:length(res)){
  sum[i]=((nomequityreturn[i]+nomdebtreturn[i]+returncapitalregdeprec[i]+opex[i]+revadjust[i]+
               fccustindex[i]+nontaxincome[i]-totaltaxexpense[i]+taxloss[(i-1)])*
              corptaxrate[i])/(1-(1-imputationcredits)*corptaxrate[i])

  immediatetax[i]=max(0,as.numeric(sum[i]), na.rm=T)

  revenuesubtotal[i] = nomequityreturn[i]+nomdebtreturn[i]+returncapitalregdeprec[i]+
    opex[i]+revadjust[i]+immediatetax[i]+(-immediatetax[i]*imputationcredits)

  revtaxassess[i]=revenuesubtotal[i]+fccustindex[i]+nontaxincome[i]

  pretaxincome[i]=revtaxassess[i]-totaltaxexpense[i]

  taxableincome[i]=pretaxincome[i]+taxloss[(i-1)]

  taxpayable[i]=as.numeric(ifelse(taxableincome[i]>0,corptaxrate[i]*taxableincome[i],0))

  valueimputationcredits[i]=-immediatetax[i]*imputationcredits

  nettaxallowance[i]=immediatetax[i]+valueimputationcredits[i]}

  immediatetax$value="immediatetax"
  revenuesubtotal$value="revenuesubtotal"
  revtaxassess$value="revtaxassess"
  pretaxincome$value="pretaxincome"
  taxableincome$value="taxableincome"
  taxpayable$value="taxpayable"
  valueimputationcredits$value="valueimputationcredits"
  nettaxallowance$value="nettaxallowance"


  df=rbind.data.frame(immediatetax,revenuesubtotal,revtaxassess,pretaxincome,taxableincome,taxpayable,
                      valueimputationcredits,nettaxallowance)

  return(df)

}

