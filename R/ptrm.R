#' PTRM all model
#'
#' Complete model with only the real output
#'
#' Returns a database of projected revenue buildling blocks and capex for app
#'
#' Prerequisites that need to be defined in ptrm model (i.e. run above code) are defined in arguments below.
#'
#' @param assets.df dataframe of all assets for all dnsps
#' @param iab.df dataframe of iab for dnsps with data
#' @param other.df dataframe of all other inputs for all dnsps
#' @param projyearend.in dynamic input of final projected year
#' @param age.in dynamic input of asset age
#' @param retireslim.in dynamic input of retire or slim assets
#' @param addnew.in dynamic input of adding new assets
#' @param productivity.in dynamic input of productivity
#' @param dynamicincdebt.in dynamic input of increase in debt
#' @param dnsp.in dynamic input of selected dnsp
#' @param method dynamic input of method either yoy or warl
#'
#' @keywords ptrm, dynamic model, shiny, app
#' @export
#'
#'
ptrm_fun= function(assets.df,other.df, iab.df,projyearend.in, age.in, retireslim.in,addnew.in,
                   productivity.in,dnsp.in, method, dynamicincdebt.in){

  #create dataframe to link dnsp name with dnsp number
  d.name=c("Ausgrid","SAPN")
  d.code=1:2
  dnsp.df=cbind.data.frame(d.code,d.name)
  dnsp=as.numeric(dnsp.df$d.code[dnsp.df$d.name==dnsp.in])
  dnsp.label=as.character(dnsp.in)


  assets.df=assets.df[assets.df$dnsp==(as.numeric(dnsp)),]
  assets.df=subset(assets.df,(!is.na(assets.df$dnsp)))

  other.df=other.df[other.df$dnsp==(as.numeric(dnsp)),]
  other.df=subset(other.df,(!is.na(other.df$dnsp)))

  #iab dataframe
  iab.df=iab.df[iab.df$dnsp==(as.numeric(dnsp)),]
  iab.df=subset(iab.df,(!is.na(iab.df$dnsp)))

  noassets=as.numeric(nrow(assets.df))
  assetclasslist=1:noassets
  assetcode=assets.df$assetcode


  #time line ----
  projyearend = projyearend.in
  startyearend=other.df$all.years[other.df$name=="year start"]
  noyears=(projyearend-startyearend)
  years=1:noyears
  yearslabel=(startyearend:projyearend)
  yearslabel=yearslabel[2:length(yearslabel)]


  #dynamic inputs----
  age=rep(age.in,noyears)
  retireslim=rep(retireslim.in/100,noyears)
  addnew=rep(addnew.in/100,noyears)
  productivity=rep(productivity.in/100,noyears)
  dynamicincdebt=dynamicincdebt.in/100

  #no scientific notation
  options(scipen=999999)


  #static inputs ----
  #inflation rate
  infl=other.df$all.years[other.df$name=="Inflation Rate"]

  #corporate tax rate
  corptaxrate=other.df$all.years[other.df$name=="Expected Corporate Tax Rate"]

  #PTRM inputs ----
  propdebtfund=(other.df$all.years[other.df$name=="Proportion of Debt Funding"])
  propeqfund=(1-propdebtfund) # annual building blocks # equity = 1-debt (proportion of debt funding)

  oab=sum(assets.df$openingassetvalue[1:noassets], na.rm=T) #opening asset base
  auc=sum(assets.df$assetsunderconstruction[1:noassets], na.rm=T) #assets under construction

  #RABstart = opening RAB -
  RABstart=(oab+auc) #assets F474 real residual RAB at 2018-19

  #rate equity input - does not vary with time
  rateequity=other.df$all.years[other.df$name=="Return on Equity"]


  #return on debt ----
  # trailing average portfolio return on debt function
  debtstart=other.df$'1'[other.df$name=="Trailing Average Portfolio Return on Debt"]
  debtreduce=round((other.df$'1'[other.df$name=="Trailing Average Portfolio Return on Debt"]-
                      other.df$'2'[other.df$name=="Trailing Average Portfolio Return on Debt"]),digits=5)
  debtfinal=other.df$'5'[other.df$name=="Trailing Average Portfolio Return on Debt"]

  rdebt.df=rdebt_fun(other.df,yearslabel,dynamicincdebt)

  #PTRM excel macros ----
  #asset remaining life
  #A1remlife = A1 = PTRM input! asset class 1, rem life = remaining life
  #so A1remlife is remlife[1]
  remlife=assets.df$remaininglife

  #opening asset value
  oavalue = assets.df$openingassetvalue

  #asset standard life
  stdlife = assets.df$standardlife #now convert 0 standard life to na for capex formula (cause it divides by zero and the capex inputs treat na's as zeros)
  stdlife[which(stdlife==0)]=NA

  #real vanilla WACC ----
  #rvanilla01 = real vanilla !WACC = G8*G11+G9*G15
  #rvanilla=propeqfund*raterealequityreturn + propdebtfund*raterealdebtreturn

  #real rate of equity funding
  #WACC rows:
  #G8 = propeqfund #Proportion of Equity Funding
  #G11 Post-tax Real Return on Equity	= raterealequityreturn = (1+G10)/(1+G6)-1
  #G10 = rateequity = Post-tax Nominal Return on Equity (constant)
  #G6 = infl = Inflation rate
  raterealequityreturn=(1+rateequity)/(1+infl)-1 #static value

  #real rate of debt return
  #G9 = propdebtfun = Proportion of Debt Funding
  #G15 =  Real Pre-tax Return on Debt =(1+G14)/(1+G6)-1 = rate real debt return
  #G14 = rdebt.df[1:10] = Nominal Pre-tax Return on Debt 	(Varying)	#nominal means inflation is taken into account
  #=IF(OR(ISBLANK('PTRM input'!K222),'PTRM input'!K222=0),J14,'PTRM input'!K222) #this means that if the input of the nominal pre-tax return on debt is blank or zero, make it the final prediction (in this case 0.0503)
  #G6 = infl = Inflation rate
  raterealdebtreturn=((1+rdebt.df)/(1+infl)-1) #real is where impact of inflation is removed

  #real vanilla WACC vector - WACC row 19
  rvanilla=(propeqfund*raterealequityreturn)+(propdebtfund*raterealdebtreturn)

  #net capex ----
  #replace na for 0 - na results in an error and ptrm treats na for zero in capex
  baseR.na   <- function(x) { x[is.na(x)] <- 0; x }

  assets.orig=assets.df
  assets.df=baseR.na(assets.orig[,(which(colnames(assets.orig)=="fccustcontrib1")):length(assets.orig)]) #from column 10 (start of capex inputs) change na's to zeros
  assets.df=cbind.data.frame(assets.orig[,1:(which(colnames(assets.orig)=="taxstdlife"))],assets.df)

  #net capex from dnsp input
  col1=which(names(assets.df)=="fcnetcapex1")
  end1=which(names(assets.df)=="fcnetcapex5")
  fcnetcapex=(assets.df[col1:end1])

  #customer contributions
  col1=which(names(assets.df)=="fccustcontrib1")
  end1=which(names(assets.df)=="fccustcontrib5")
  fccust=(assets.df[col1:end1])

  #fccust contrib predictions----
  result3 <- matrix(NA, ncol=length(yearslabel), nrow=length(assetclasslist))
  result3=as.data.frame(result3)
  names(result3)=yearslabel
  #make customer contributions equal to final year of inputs (i.e.y5)
  fcyears=(1:length(fccust)) #how many years do we have
  result3[assetclasslist, fcyears]=fccust #make first 5 years equal to inputs
  result3[,(length(fccust)+1):length(result3)]=fccust[length(fccust)] #make the rest equal to the final year (y5)
  fccust.full=result3

  #iab SAPN and Energex -----
  #take out iab for raw data
  col1=which(names(iab.df)=="21")
  iabend=as.numeric((names(iab.df[length(iab.df)])))
  #cut to size of dataframe
  if(iabend>=projyearend){
  end1=which(names(iab.df)==projyearend)
  iabcheck=(iab.df[col1:end1])
  } else
  {#make the rest =0
    tmp <- matrix(0, ncol=length(yearslabel), nrow=noassets)
    tmp=as.data.frame(tmp)
    names(tmp)=yearslabel
    iabcheck=tmp
    col1=which(names(iab.df)=="21")
    end1=length(iab.df)
    iabcheck[1:end1]=(iab.df[col1:end1])
  }



  #netcapex predictions ----
  repratio	= other.df$all.years[other.df$name=="Replacement ratio"]#Replacement ratio
  augexratio	 = other.df$all.years[other.df$name=="Augex ratio"]# Augex ratio
  syscapratio	= other.df$all.years[other.df$name=="Systcap ratio"]# System capital overhead ratio
  modrepcost = other.df$all.years[other.df$name=="Modern rep cost"]# Modern day replacement cost

  startincline	= other.df$all.years[other.df$name=="Start incline"]
  ratedecline	 = other.df$all.years[other.df$name=="Rate decline"]
  decline2040	= other.df$all.years[other.df$name=="Decline after 2040"]#


  fcnetavg=fcnetcapex #df of first 5 years in ptrm
  fccapex6=rowMeans(fcnetavg)

  #if average is less than 0 make zero
  fccapex6=ifelse(fccapex6<0,0,fccapex6)
  fcnetavg$fccapex6=fccapex6
  avg=fccapex6

  #make an empty df for all years and then put in the first 5 years
  tmp <- matrix(NA, ncol=length(yearslabel), nrow=noassets)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel
  blank=tmp
  fcnetyears=(1:length(fcnetavg))
  tmp[, fcnetyears]=fcnetavg
  fcnetavg=tmp

  #I. net capex replacement ----
  ncreplace.df=ncreplace_fun(yearslabel, fcnetavg,noyears,projyearend,noassets, assetcode, repratio,
                             age, modrepcost,startincline,ratedecline,decline2040)

  #II net capex replace or slim
  ncskinny.df=ncskinny_fun(noassets,yearslabel, fcnetcapex, ncreplace.df, retireslim)

  #III net capex augex
  ncaugex.df=ncaugex_fun(yearslabel,fcnetavg,noyears,projyearend,noassets, augexratio, addnew, assetcode)

  #IV net capex sys cap overhead
  ncsyscap.df=ncsyscap_fun(noassets,yearslabel, fcnetavg, syscapratio,assetcode)

  #V capex non-network
  ncnonnetwk.df=ncnonnetwk_fun(noassets,yearslabel, fcnetcapex, assetcode)

  #aggregate
  netcapex.all=ncagg_fun(noassets,yearslabel, productivity, fcnetcapex, ncreplace.df, ncskinny.df, ncaugex.df,
                         ncsyscap.df, ncnonnetwk.df)


  fcnetavg.full=netcapex.all
  totalnetcapex= colSums (fcnetavg.full, na.rm = T,dims=1)
  totalnetcapex=as.data.frame(totalnetcapex)

  #WARL or YOY depreciation remlife formulas ----

  #warl using package
  if(method==1){
    #warl method
    res_all <- NULL
    for (i in 1:noassets){
      ac=i
      tmp=warl_fun(ac, yearslabel, fcnetavg.full, rvanilla, remlife, stdlife, oavalue,
                   noyears)
      res_all <- rbind(res_all, tmp)}

    origcost_all=subset(res_all,res_all$value=="oc")
    oav_all=subset(res_all,res_all$value=="oav")
    remlife_all=subset(res_all,res_all$value=="rl")
    regdeprec_all=subset(res_all,res_all$value=="rd")
    regdeprec_all=regdeprec_all[,1:(length(regdeprec_all)-2)]
    regdeprec_warl=regdeprec_all

  }else{
    #using year on year tracking
    sumcapex=yoyall_fun(noassets, yearslabel, years, noyears, remlife, oavalue,
                        startyearend,projyearend,stdlife,fcnetavg.full,rvanilla,iabcheck)
    regdeprec_yoy=sumcapex

    regdeprec_all=sumcapex

  }


  #depreciation ----
  #Excel Assets row 75, i.e. the sum of the iab + capex across all the asset categories for each year
  deprec= colSums (regdeprec_all, na.rm = T,dims=1)


  #real net capex ----
  #assets row 10=real net capital expenditure =SUM(G11:G40) = real capex of all asset classes for each year
  #G11=fcnetassets*(1+rvanilla01)^0.5 -  per asset class - so g11-g40 are all the 30 asset classes with this formula
  #fcnetcapex.df[assetclass,capexstage]*(1+rvanilla[capexstage])^0.5

  realcapexall=realcapex_fun(noyears,noassets,fcnetavg.full,rvanilla,yearslabel)


  #real RAB (end period)----
  #rRAB= prevyearRAB - deprec + realcapex = real residual RAB (end period) = assets! row 466

  rRABend=rRABend_fun(noyears,RABstart,deprec,realcapexall,startyearend,projyearend)
  #end period +1 because it starts with 2018-19, assets! row 466


  #real residual RAB start period is the RAB from previous year
  rRABstart=as.numeric(c(0,rRABend[1:(length(rRABend)-1)])) #assets row 467
  label=(startyearend:projyearend)
  df=as.data.frame(matrix(NA,ncol=length(label),nrow=1))
  names(df)=label
  df[1,]=rRABstart
  rRABstart=df

  #nominal RAB ----

  #cif ----
  cifyears=(1:(noyears+1)) #cumulative inflation index
  cif.all=cif_fun(cifyears,infl, startyearend, projyearend) #includes y0
  cif.df=cif.all[2:length(cif.all)]

  RABnom=rRABend*cif.all #nominal residual RAB (end period) assets row 474

  #analysis row 17 is revenue building blocks RAB (start period) and is moved up one year
  RABBBstart=as.numeric(c(0,RABnom[1:(length(RABnom)-1)])) #including y0
  RABBBstart=RABBBstart[2:length(RABBBstart)] #starting at year ending y1
  label=((startyearend+1):projyearend)
  df=as.data.frame(matrix(NA,ncol=length(label),nrow=1))
  names(df)=label
  df[1,]=RABBBstart
  RABBBstart=df

  #nominal vanilla WACC return on capital - from year ending y1 onwards
  nomequityreturn = (RABBBstart*propeqfund) * rateequity #Analysis row 25
  nomdebtreturn = (RABBBstart*propdebtfund) * rdebt.df #analysis row 26

  #1. return on capital ----
  #total return on capital = nominal vanilla WACC starts at y1
  returnoncapital = nomequityreturn+nomdebtreturn #analysis row 23, also final revenue summary component 1


  ##reg depreciation  ----
  #analysisG28=Assets!G$473=G472-G471 #nominal regulatory depreciation
  #nom straight line depreciation - inflation opening rab
  #G472=G75*G7 = real straight line deprec*cif.df
  nomdeprec=(deprec*cif.df) #assets row 472 - 2019 value is zero


  #inflation on open RAB ----
  #assets row 471 : G471=F474*G6: nominal RABend (RABnom) from previous year (including y0) *2.5%
  prevRABnom=as.numeric(c(0,RABnom[2:length(RABnom)-1])) #including zero for y0
  RABoinfl=prevRABnom*infl
  RABoinfl=RABoinfl[2:length(RABoinfl)]
  label=((startyearend+1):projyearend)
  df=as.data.frame(matrix(NA,ncol=length(label),nrow=1))
  names(df)=label
  df[1,]=RABoinfl
  RABoinfl=df


  #2. nominal reg depreciation ----
  #revenue summary return of capital
  #nominal regulatory depreciation row 473 also analysis row 28 (return on)
  nomREGdeprec=nomdeprec-RABoinfl #y1


  #inflated nominal residual RAB -----
  #assets H475 Inflated Nominal Residual RAB (start period)=H$7*H467=rRABstart*cif
  inflnomresRAB=rRABstart*cif.all #assets row 475


  #operating expenditure ----
  #Analysis row 30='PTRM input'!G187*cif.df (starting at y1)
  #= (controllable opex + debt raising costs[1:length(controlopex)])*cif.df

  #controllable opex ----
  controlopex= other.df[which(other.df$name=="Controllable Opex"),grep("^1$",colnames(other.df)):grep("^5$",colnames(other.df))] #just first 5 years 2020 to 2024
  #opex debt raising costs
  opexdrc=other.df[which(other.df$name=="Opex debt raising costs"),grep("^1$",colnames(other.df)):grep("^5$",colnames(other.df))] #just first 5 years 2020 to 2024


  #opex ----
  #function for projecting controllable opex across all years
  controlopex.full=opex_fun(yearslabel, controlopex, productivity,other.df)

  #debt raising costs----
  #if using the average of ptrm years
  #opexdrc.full=opexdrc_fun(yearslabel, opexdrc)

  #using ptrm equation
  #=(Drc*Dv*Assets!G475)/Assets!G7

  Drc=other.df$all.years[other.df$name=="Debt Raising Costs"] #Drc = debt raising cost static input PTRM inputs G233
  Dv = propdebtfund #proportion of debt funding =  0.6
  #assets G475: Inflated Nominal Residual RAB (start period)

  fcdebtraisecosts=(Drc*Dv*inflnomresRAB)/cif.all
  fcdebtraisecosts=fcdebtraisecosts[2:length(fcdebtraisecosts)] ##PTRM input tab row 186, #forecast operating and maintenance expend debt raising costs


  #3. opex ----
  #x factor number 3

  opex=(controlopex.full + fcdebtraisecosts[1:length(controlopex.full)])*cif.df[,1:length(controlopex.full)]

  #4. Revenue adjustments ----
  #='PTRM input'!G205*cif.df #analysis row 32

  revadjust.df=subset(other.df,other.df$type=="revadjust")

  #revenue adjustments predictions ----
  #extend to noyears by average column
  start.col=grep("^1$", colnames(revadjust.df))
  end.col=grep("^5$", colnames(revadjust.df))
  name.col=grep("^name$", colnames(revadjust.df))

  revadjustavg=revadjust.df[,c(name.col,start.col:end.col)]

  #make year ending 2025 the average of previous values
  revadjustavg$'6'=rowMeans(revadjustavg[,2:length(revadjustavg)])

  #make 2025 remittal adjustments and shared assets =0
  revadjustavg$'6'[grep("remittal adjust", revadjustavg$name)]=0
  revadjustavg$'6'[grep("shared assets adjust", revadjustavg$name)]=0

  #take out name column of revadjust
  revadjustavg=revadjustavg[,2:length(revadjustavg)]

  result <- matrix(NA, ncol=length(yearslabel), nrow=(nrow(revadjustavg)))
  result=as.data.frame(result)
  names(result)=yearslabel
  revadjustyears=(1:length(revadjustavg))
  result[, revadjustyears]=revadjustavg

  #make all the other values the average
  result[,(length(revadjustavg)+1):length(result)]=revadjustavg[length(revadjustavg)] #make the rest equal to the final reg year (y5)
  revadjust.full=result

  revadjust=colSums (revadjust.full, na.rm = T,dims=1)
  revadjust=revadjust*cif.df[1:length(revadjust)]

  # Net tax allowance -----
  #=Analysis!G34+Analysis!G35 = Tax Payable + (Less) Value of Imputation Credits

  #tax payable calculations----
  #anlaysis excel tab
  #taxpayable = G59=MAX(0,(G25+G26+G28+G30+G32+G40+G41-G50+F56)*G53/(1-(1-$D$35)*G53))

  #g25=nomequityreturn
  #make df
  res <- matrix(NA, nrow=1, ncol=length(yearslabel))
  res=as.data.frame(res)
  names(res)=yearslabel
  df=res
  df[1,]=nomequityreturn
  nomequityreturn=df

  #g26=nomdebtreturn #already calc above

  #g28=returncapitalregdeprec : return of capital (regulatory depreciation)
  returncapitalregdeprec=nomREGdeprec[1:length(nomREGdeprec)] #assets 473 = RAB.df #from y1 onwards
  df=res
  df[1,]=returncapitalregdeprec
  returncapitalregdeprec=df

  #g30=opex #already calc above

  #G32=revadjust revenue adjustments #already calc above

  #g40=customer contributions
  #='PTRM input'!G139*Analysis!G$7 = sum of customer contributions each year * cif.df
  fccustsum= colSums (fccust.full, na.rm = T,dims=1)
  fccustindex=fccustsum*cif.df[1:length(fccustsum)]

  #G41 = Deduct Non-Tax Income Revenue Adjustments - has to be negative
  #=-'PTRM input'!G202*Analysis!G$7
  #=SUMIF($R$191:$R$200,"No",G191:G200)*cif.df - only include in sum if it's a no

  #add in tax income or expense notes from other.df
  taxincome=other.df$taxinc[other.df$type=="revadjust"]
  taxexp=other.df$taxexp[other.df$type=="revadjust"]
  revadjust.full$taxincome=as.numeric(taxincome)
  revadjust.full$taxexp=as.numeric(taxexp)

  #g41=nontaxincome
  nontaxincome=subset(revadjust.full,revadjust.full$taxincome==0)
  nontaxincome=colSums(nontaxincome[,1:(length(nontaxincome)-2)])
  nontaxincome=-(nontaxincome*cif.df[1:length(nontaxincome)])

  #G50=SUM(G46:G49)= sum of opex (see above), Tax Depreciation, Interest, Tax expense revenue adjustments, Total Tax Expenses

  # row 46 is assets row 479 = sum of taxiab + tax depreciation in each year for each asset (like year on year tracking depreciation above)

  #taxiab check -----
  #take out iab 1-10 as a new dataframe
  col1=which(names(assets.df)=="taxiab1")
  end1=which(names(assets.df)=="taxiab10")
  taxiabcheck=(assets.df[col1:end1])

  #
  #tax iab ----
  taxremlife=assets.df$taxremlife
  taxoavalue=assets.df$openingtaxvalue
  taxstdlife=assets.df$taxstdlife

  #then add run tax yoy functions
  taxsumcapex= taxyoyall_fun(noassets, yearslabel, noyears, taxremlife, taxoavalue,
                             startyearend,projyearend,taxstdlife,fcnetavg.full,fccust.full,cif.df,taxiabcheck)

  #tax depreciation ----
  #analysis row 47 and assets row 479 =taxdeprec
  taxdeprec= colSums (taxsumcapex, na.rm = T,dims=1)

  #placeholder adjustment----
  result <- matrix(0, ncol=length(yearslabel), nrow=1)
  result=as.data.frame(result)
  names(result)=yearslabel

  place=other.df[other.df$name=="placeholder adjustment",
                 (which(colnames(other.df)=="1"):which(colnames(other.df)=="10"))]

  result[1:length(place)]=place
  place=result

  taxdeprec=taxdeprec+place

  #interest ----
  #analysis g48 and g26 =G10*G19 = interest = time varying return on debt *RABnom*0.6 (debt prop) - from y1 onwards
  interest= nomdebtreturn #rdebt.df*RABBBstart*0.6

  #tax expense revenue adjustments----
  #g49='PTRM input'!G203*cif.df = (SUMIF($S$191:$S$200,"Yes",G191:G200))*cif.df

  #g49=taxexprevadj
  taxexp.df=subset(revadjust.full,revadjust.full$taxexp==1)
  taxexprevadj=colSums(taxexp.df[,1:(length(taxexp.df)-2)])
  taxexprevadj=(taxexprevadj*cif.df[1:length(taxexprevadj)])

  #g50=totaltaxexpense
  totaltaxexpense=opex+taxdeprec+interest+taxexprevadj

  #value of imputation credits----
  #d35=imputationcredits (static value)
  imputationcredits=other.df$all.years[other.df$name=="Value of Imputation Credits (gamma)"]

  #corporate tax rate ----
  #g53=corptaxrate
  res <- matrix(NA, nrow=1, ncol=length(yearslabel))
  res=as.data.frame(res)
  names(res)=yearslabel
  df=res
  df[1,]=other.df$all.years[other.df$name=="Expected Corporate Tax Rate"]
  corptaxrate=df

  values=c("nomequityreturn","nomdebtreturn","returncapitalregdeprec","opex","revadjust",
           "fccustindex","nontaxincome","totaltaxexpense","corptaxrate")

  #join all into a dataframe to import into net tax allowance function
  tax.df=rbind.data.frame(nomequityreturn,nomdebtreturn,returncapitalregdeprec,opex,revadjust,
                          fccustindex,nontaxincome,totaltaxexpense,corptaxrate)


  tax.df[,as.character(startyearend)]=0
  #reorder
  tax.df = tax.df[c(length(tax.df),1:(length(tax.df)-1))]

  tax.df$values=values

  #tax loss carried forward ----
  nettax.df=nettax_fun(yearslabel, startyearend, projyearend, other.df, tax.df)

  #5. net tax allowance----
  #revenue summary row 11
  nettaxallowance=subset(nettax.df,nettax.df$value=="nettaxallowance")

  nettaxallowance=nettaxallowance[2:(length(nettaxallowance)-1)]

  #tax definitions ----
  ##excel reference from analysis tab
  #all in: nettax.df$value and tax.df$value
  #row 25=nomequityreturn
  #row 26=nomdebtreturn
  #row 28=returncapitalregdeprec
  #row 30=opex
  #row 32=revadjust
  #row 34=immediatetax
  #row 35=valueimputationcredits
  #row 37=revenuesubtotal
  #row 40=fccustindex
  #row 41=nontaxincome
  #row 43=revtaxassess
  #row 46=opex (repeated in ptrm)
  #row 47=taxdeprec
  #row 48=interest
  #row 49=taxexp
  #row 50=totaltaxexpense
  #row 53=corptaxrate
  #row 54=taxableincome
  #row 55=pretaxincome
  #row 56=taxlosscarriedforward
  #row 57=taxpayable
  #row 59=immediatetax
  #d35=imputationcredits


  # annual revenue requirement ----
  #nominal
  A=returnoncapital
  B=nomREGdeprec #return OF capital (depreciation)
  C=opex
  D=revadjust
  E=nettaxallowance

  annualrevenue=A+B+C+D+E

  #real
  Areal=returnoncapital/cif.df
  Breal=nomREGdeprec/cif.df #return OF capital (depreciation)
  Creal=opex/cif.df
  Dreal=revadjust/cif.df
  Ereal=nettaxallowance/cif.df

  annualrevenue.real=annualrevenue/cif.df

  year=(startyearend+1):projyearend
  year=year+2000
  names(df)=c("Rev.real","A.real","B.real","C.real","D.real","E.real","Year")


  #Areal=returnoncapital/cif.df
  #Breal=nomREGdeprec/cif.df #return OF capital (depreciation)
  #Creal=opex/cif.df
  #Dreal=revadjust/cif.df
  #Ereal=nettaxallowance/cif.df

  retdf=t(Areal)
  retdf=cbind.data.frame(year,retdf,"Return on capital (real)")
  names(retdf)=c("year","cost","type")

  depdf=t(Breal)
  depdf=cbind.data.frame(year, depdf,"Regulatory depreciation (real)")
  names(depdf)=c("year","cost","type")

  opdf=t(Creal)
  opdf=cbind.data.frame(year,opdf,"Opex (real)")
  names(opdf)=c("year","cost","type")

  revadf=t(Dreal)
  revadf=cbind.data.frame(year,revadf,"Adjusted revenue (real)")
  names(revadf)=c("year","cost","type")

  taxdf=t(Ereal)
  taxdf=cbind.data.frame(year,taxdf,"Tax (real)")
  names(taxdf)=c("year","cost","type")

  anrevdf=t(annualrevenue.real)
  anrevdf=cbind.data.frame(year, anrevdf,"Revenue (real)")
  names(anrevdf)=c("year","cost","type")


  #real capex, real RAB and real vanilla WACC
  realcapex=as.numeric(realcapexall)
  realrab=as.numeric(rRABend[2:length(rRABend)])
  realratereturn=round((as.numeric(rvanilla))*100, digits=2) #%


  df.real=rbind.data.frame(retdf,depdf,opdf,revadf,taxdf,anrevdf)
  df.real=cbind.data.frame(df.real,realcapex,realrab,realratereturn)

  df.real$dnsp=dnsp
  df.real$dnsp.label=as.character(dnsp.in)


  #df=rbind.data.frame(annualrevenue.real,Areal,Breal,Creal,Dreal,Ereal)
  #df=as.data.frame(t(df))
  #year=(startyearend+1):projyearend
  #df$year=year
  #names(df)=c("Rev.real","A.real","B.real","C.real","D.real","E.real","Year")

  #need to add year 2020 if not there, make equal to first year
  if (min(df.real$year)>2020){
    y1=subset(df.real,df.real$year==2021)
    y1$year=2020
    df.real=rbind.data.frame(y1,df.real)
  } else
  {df.real}

  #END return ----
  return(df.real)
}





