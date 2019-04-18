#' WARL formulas
#'
#' Weighted average remaining life method for real depreciation
#'
#' Function calculates only one asset class: ac, which is specified by user. The function has to be looped to project the WARL depreciation for all asset classes.
#' Prerequisites that need to be defined in ptrm model, i.e. run above code: ac, yearslabel, fcnetavg.full, rvanilla, remlife, stdlife, oavalue, noyears
#' @param ac asset class
#' @param yearslabel from 2020 to projected year end
#' @param noyears count of years
#' @param fcnetavg.full dataframe of all netcapex for all assets defined by netcapex function
#' @param rvanilla vector of real vanilla WACC
#' @param remlife vector of remaining life of assets from dnsp assets.df input dataframe
#' @param stdlife vector of standard life of assets from dnsp assets.df input dataframe
#' @param oavalue vector of opening asset value of assets from dnsp assets.df input dataframe
#' @keywords warl, depreciation, capex, remlife, stdlife, vanilla WACC
#' @export

warl_fun <- function(ac, yearslabel, fcnetavg.full, rvanilla, remlife, stdlife, oavalue,
                     noyears){

  res <- matrix(0, ncol=length(yearslabel), nrow=1) #one at a time
  res=as.data.frame(res)
  names(res)=yearslabel
  res$asset=as.character(ac)
  res$value=NA

  s=stdlife[ac]
  s=ifelse(is.na(s),0,s)
  nc=fcnetavg.full[ac,]
  v=rvanilla

  #year 1
  #remlife
  rl=res
  rl1=remlife[ac]
  rl[,1]=rl1

  #opening asset value=oav
  oav=res
  oav1=oavalue[ac]
  oav[,1]=oav1

  #origcost=oc
  oc=res
  oc[,1]=ifelse(is.na(rl1),oav1,ifelse(rl1==0,0,(oav1*s/rl1)))

  #regdeprec=rd
  rd=res
  rd[,1]=ifelse(is.na(rl1)|rl1==0,0,(ifelse(rl1>1,oav1/rl1,oc[,1]-oav1)))

  rdtest=res
  rdtest[,1]=1

  #y2 and beyond
  for(i in 2:noyears){

    oc[i]=ifelse((rl[(i-1)]<1),
                 (nc[,(i-1)]*(1+v[,(i-1)])^0.5),
                 oc[i-1]+(nc[,(i-1)]*(1+v[,(i-1)])^0.5))

    oav[i]=ifelse(rl[i-1]<1,oc[i],(oc[i]-(oc[i-1]-oav[i-1])-rd[i-1]))

    rl[i]=ifelse(is.na(rl[(i-1)]),NA,ifelse((oc[i]==0),0,(oav[i]/oc[i]*s)))

    rdtest[i]=max(1,which(rl==s))

    rd[i]=ifelse(is.na(rl[i]),0,ifelse((rl[i]==0),0,
                                       (ifelse(rl[i]>1,oav[i]/rl[i],
                                               (ifelse(rdtest[i]>1,(oc[i]-sum(rd[,as.numeric(rdtest[i]):(i-1)], na.rm=T)),
                                                       (oc[i]-(sum(rd[,1:(i-1)],na.rm=T)+(oc[1]-oav[1])))))))))

  }

  oc$value="oc"
  oav$value="oav"
  rl$value="rl"
  rd$value="rd"

  df=rbind.data.frame(oc,oav,rl,rd)

  return(df)
}


