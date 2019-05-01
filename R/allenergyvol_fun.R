#' Energy volume functions
#'
#' Formula for calculating the energy volume distribution among customers
#'
#'
#' @param dnsp.in DNSP selected for function loop
#' @param other.df database of static dnsp inputs
#' @param cars.in dynamic variable for electric vehicle penetration by 2060 as a percent
#' @param projyearend.in dynamic variable of final year
#' @export
#'
allenergyvol_fun=function(dnsp.in,other.df,cars.in,projyearend.in){
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


# customer number functions
  custnum=cust_fun(yearslabel,other.df)
  newcust=newcust_fun(yearslabel,custnum)

  energyvol= other.df[which(other.df$name=="energy volumes"),grep("^1$",colnames(other.df)):grep("^5$",colnames(other.df))]
  pcustgrowth=other.df[which(other.df$name=="energy p cust growth rate"),grep("^all.years$",colnames(other.df))]

  #electric vehicle functions
  cars=cars_fun(yearslabel,other.df) #total number of cars
  carpenet=carpenet_fun(yearslabel,cars.in) #electric car penetration
  electriccars=cars*carpenet #total number of electric cars
  carenergyvol=carenergyvol_fun(yearslabel,other.df,electriccars) #absolute energy vol of electric cars
  carevolchange=carevolchange_fun(yearslabel,carenergyvol) #change in energy volume of electric cars



  tmp <- matrix(NA, ncol=length(yearslabel), nrow=1)
  tmp=as.data.frame(tmp)
  names(tmp)=yearslabel
  energyvolexist=tmp
  energyvolpcust=tmp
  energyvolnewcust=tmp
  energyvoltot=tmp
  energygrowthrate=tmp
  energypcustgrowthrate=tmp

  #energy vol total
  for(i in 1:5)
    energyvoltot[i]=energyvol[i]

  #energy vol exisiting customers
  for(i in 1:5)
    energyvolexist[i]=0
  for(i in 6)
    energyvolexist[i]=(energyvoltot[i-1]*(1+pcustgrowth))

  #energy vol per customer
  for(i in 1:5)
    energyvolpcust[i]=0
  for(i in 6)
    energyvolpcust[i]=energyvolexist[i]/custnum[i-1]

  #energy vol for new customers
  for(i in 1:5)
    energyvolnewcust[i]=0
  for(i in 6)
    energyvolnewcust[i]=newcust[i]*energyvolpcust[i]

  #total energy volumes
  for(i in 6)
    energyvoltot[i]=energyvolexist[i]+energyvolnewcust[i]+carevolchange[i]

  #loop through from 7:length
  for (i in 7:length(tmp)){

    energyvolexist[i]=(energyvoltot[i-1]*(1+pcustgrowth))

    energyvolpcust[i]=energyvolexist[i]/custnum[i-1]

    energyvolnewcust[i]=newcust[i]*energyvolpcust[i]

    energyvoltot[i]=energyvolexist[i]+energyvolnewcust[i]+carevolchange[i]
    }

  #energy growth rate
  for(i in 1)
    energygrowthrate[i]=0
  for(i in 2:length(tmp))
    energygrowthrate[i]= (energyvoltot[i]-energyvoltot[i-1])/energyvoltot[i-1]

  #energy per customer
  energypcust=energyvoltot/custnum

  #energy per customer growth rate
  for(i in 1)
    energypcustgrowthrate[i]=0
  for(i in 2:length(tmp))
    energypcustgrowthrate[i]=(energypcust[i]-energypcust[i-1])/energypcust[i-1]

data=rbind.data.frame(custnum,
                      energyvoltot,
                      energyvolexist,
                      newcust,
                      energyvolpcust,
                      energyvolnewcust,
                      carevolchange,
                      energygrowthrate,
                      energypcust,
                      energypcustgrowthrate,
                      cars,
                      carpenet,electriccars,carenergyvol)

names=c("custnum",
        "energyvoltot",
        "energyvolexist",
        "newcust",
        "energyvolpcust",
        "energyvolnewcust",
        "carevolchange",
        "energygrowthrate",
        "energypcust",
        "energypcustgrowthrate",
        "cars",
        "carpenet","electriccars","carenergyvol")

data$names=names


return(data)
  }

