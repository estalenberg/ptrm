#' Price per GWh and per customer
#'
#' Formula for calculating the relative price for customers
#'
#' @param dnsp.in DNSP selected for function loop
#' @param projyearend.in dynamic variable of final year
#' @param df.real dataframe of real revenue calculated from ptrm_fun
#' @param energyvol.df dataframe of relative energy volumes from allenergyvol_fun
#' @param other.df dataframe of static dnsp inputs
#'
#' @export
#'
price_fun=function(dnsp.in,df.real,energyvol.df,other.df,projyearend.in){
  #cut to dnsp
  d.name=c("Ausgrid","SAPN")
  d.code=1:2
  dnsp.df=cbind.data.frame(d.code,d.name)
  dnsp=as.numeric(dnsp.df$d.code[dnsp.df$d.name==dnsp.in])
  dnsp.label=as.character(dnsp.in)

  other.df=other.df[other.df$dnsp==(as.numeric(dnsp)),]
  other.df=subset(other.df,(!is.na(other.df$dnsp)))

  #time line ----
  projyearend = projyearend.in
  startyearend=other.df$all.years[other.df$name=="year start"]
  noyears=(projyearend-startyearend)
  years=1:noyears
  yearslabel=(startyearend:projyearend)
  yearslabel=yearslabel[2:length(yearslabel)]

  #subset revenue.real
  rev.df=subset(df.real,df.real$type=="Revenue.real")
  revenue.real=rev.df$cost*1000000
  revenue.real=revenue.real[2:length(revenue.real)]

  custnum= cust_fun(yearslabel,other.df)

  rev_pcust=revenue.real/custnum
  energyvol=subset(energyvol.df,energyvol.df$names=="energyvoltot")
  rev_pGWh=revenue.real/energyvol[1:(length(energyvol)-1)]

  energypcustgrowthrate=subset(energyvol.df,energyvol.df$names=="energypcustgrowthrate")
  energypcustgrowthrate=energypcustgrowthrate[1:length(energypcustgrowthrate)-1]

  tmp <- matrix(NA, ncol=length(yearslabel-1), nrow=1)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel
  perchange_pcust=tmp
  perchange_pGWh=tmp

  resnetprice=tmp
  resnetpricetou=tmp
  resannualuse=tmp
  resannualusetou=tmp
  res_ckwh=tmp
  res_ckwhtou=tmp

  busnetprice=tmp
  busnetpricetou=tmp
  busannualuse=tmp
  busannualusetou=tmp
  bus_ckwh=tmp
  bus_ckwhtou=tmp

  for(i in 1)
    perchange_pcust[i]=0
  for(i in 2:length(tmp))
    perchange_pcust[i]=((rev_pcust[i]-rev_pcust[i-1])/rev_pcust[i-1])*100

  for(i in 1)
    perchange_pGWh[i]=0
  for(i in 2:length(tmp))
    perchange_pGWh[i]=((rev_pGWh[i]-rev_pGWh[i-1])/rev_pGWh[i-1])*100

  for(i in 1){
  resnetprice[i]=other.df[which(other.df$name=="res cust network price nontou"),grep("^all.years$",colnames(other.df))]
  resnetpricetou[i]=other.df[which(other.df$name=="res cust network price tou"),grep("^all.years$",colnames(other.df))]
  resannualuse[i]=other.df[which(other.df$name=="res cust annual energy use nontou"),grep("^all.years$",colnames(other.df))]
  resannualusetou[i]=other.df[which(other.df$name=="res cust annual energy use tou"),grep("^all.years$",colnames(other.df))]

  busnetprice[i]=other.df[which(other.df$name=="small business network price nontou"),grep("^all.years$",colnames(other.df))]
  busnetpricetou[i]=other.df[which(other.df$name=="small business network price tou"),grep("^all.years$",colnames(other.df))]
  busannualuse[i]=other.df[which(other.df$name=="small business annual energy use nontou"),grep("^all.years$",colnames(other.df))]
  busannualusetou[i]=other.df[which(other.df$name=="small business annual energy use tou"),grep("^all.years$",colnames(other.df))]
  }

  for (i in 2:length(tmp)){
    resnetprice[i]=resnetprice[i-1]*(1+(perchange_pcust[i]/100))
    resnetpricetou[i]=resnetpricetou[i-1]*(1+(perchange_pcust[i]/100))

    resannualuse[i]=resannualuse[i-1]*(1+energypcustgrowthrate[i])
    resannualusetou[i]=resannualusetou[i-1]*(1+energypcustgrowthrate[i])}

  for (i in 2:length(tmp)){
    busnetprice[i]=busnetprice[i-1]*(1+(perchange_pcust[i]/100))
    busnetpricetou[i]=busnetpricetou[i-1]*(1+(perchange_pcust[i]/100))
    busannualuse[i]=busannualuse[i-1]*(1+energypcustgrowthrate[i])
    busannualusetou[i]=busannualusetou[i-1]*(1+energypcustgrowthrate[i])

  }

  res_ckwh=resnetprice/resannualuse*100
  res_ckwhtou=resnetpricetou/resannualusetou*100
  bus_ckwh=busnetprice/busannualuse*100
  bus_ckwhtou=busnetpricetou/busannualusetou*100

  data=rbind.data.frame(revenue.real,rev_pcust,rev_pGWh,perchange_pcust,perchange_pGWh,
                        resnetprice,resnetpricetou,busnetprice,busnetpricetou,
                        resannualuse,resannualusetou,busannualuse,busannualusetou,
                        res_ckwh,res_ckwhtou,bus_ckwh,bus_ckwhtou)


  names=c("revenue.real","rev_pcust","rev_pGWh","perchange_pcust","perchange_pGWh",
          "resnetprice","resnetpricetou","busnetprice","busnetpricetou",
          "resannualuse","resannualusetou","busannualuse","busannualusetou",
          "res_ckwh","res_ckwhtou","bus_ckwh","bus_ckwhtou")

  data$names=names

  return(data)

  }

