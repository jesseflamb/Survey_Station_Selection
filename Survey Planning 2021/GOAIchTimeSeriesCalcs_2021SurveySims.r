# Test effects of different survey sampling strategies on historical time-series

# modified file from GOAIchTimeSeriesCalcs_Incl2020_NewPolygon.r

library(rgdal);library(maptools);library(mapdata);library(deldir)
library(spatstat)
library(rgeos)
library(raster)
library(SDraw)
library(RANN)

mywd<-getwd()

setwd("~/Projects/GOA ichthyoplankton time series/Data_For2018Update")
hauls.b<-read.csv("TSCruises_60Bon_Grid_HAUL RECORDS.csv")
samples.b<-read.csv("TSCruises_60Bon_Grid_SPECIMEN RECORDS.csv")
colnames(samples.b)[1]<-sub("ï..","", colnames(samples.b)[1])
colnames(hauls.b)[1]<-sub("ï..","", colnames(hauls.b)[1])

hauls.t<-read.csv("TSCruises_TUCK_GridGridPost_HAUL RECORDS.csv")
samples.t<-read.csv("TSCruises_TUCK_GridGridPost_SPECIMEN RECORDS.csv")
colnames(samples.t)[1]<-sub("ï..","", colnames(samples.t)[1])
colnames(hauls.t)[1]<-sub("ï..","", colnames(hauls.t)[1])

poly17<-read.csv("plyDY17_05area.txt")
abc<-merge(hauls.b,poly17[,c("HAUL_ID","polyarea")],all.x=T)
abc$POLYGONAL_AREA[abc$YEAR==2017]<-abc$polyarea[abc$YEAR==2017]
hauls.b<-abc[,colnames(abc)!="polyarea"]

#Use only Primary Net == Y for 60BON (PRimary Net == NPQ for Tucker)
hauls.b<-hauls.b[hauls.b$PRIMARY_NET == "Y",] #3677 Records
# CHeck hauls for Tucker: are all hauls represented in specimen table?
all(unique(hauls.t$HAUL_ID) %in% unique(samples.t$HAUL_ID))

# Merge 60Bon and Tucker
hauls<-rbind(hauls.b,hauls.t)
samples<-rbind(samples.b,samples.t)

# Now add DY1905
setwd("~/Projects/GOA ichthyoplankton time series/Data_For2020Update")
hauls19<-read.csv("plyDY19_05area.csv") #this file direct from Kimberly
samples19<-read.csv("DY19-05_60BON_CATCH.csv")
# station 120 is repeated - need to delete duplicated catches
samples19<-samples19[-c(653:656),]

samples19$YEAR<-2019
colnames(samples19)[colnames(samples19)=="SPECIES_NAME_ICHBASE"]<-"SPECIES_NAME"
#confirm all column names in hauls and samples are in 2019 data files:
colnames(samples) %in% colnames(samples19)
colnames(hauls) %in% colnames(hauls19)

samples19<-samples19[,colnames(samples)]
hauls19<-hauls19[,colnames(hauls)]

hauls<-rbind(hauls,hauls19)
samples<-rbind(samples, samples19)


#Subset, then merge and transform into catch w zero.

temp1<-sub(",","",hauls$GMT_DATE_TIME)
hauls$GMTDATETIME<-strptime(temp1,"%m/%d/%Y  %H:%M:%S",tz="UTC")
#DY1905 GMT_DATE_TIME is in different format.
temp2<-sub(".000000000","",temp1)
hauls$GMTDATETIME[is.na(hauls$GMTDATETIME)==T]<-strptime(temp2[is.na(hauls$GMTDATETIME)==T],"%d-%b-%y  %I.%M.%S %p",tz="UTC")

hauls$HOUR<-as.POSIXlt(hauls$GMTDATETIME,tz="GMT")$hour
hauls$YDAY<-as.POSIXlt(hauls$GMTDATETIME,tz="GMT")$yday+1


alls<-merge(hauls,samples[,c("HAUL_ID","SPECIES_NAME","LARVALCATCHPER1000M3","LARVALCATCHPER10M2")],by="HAUL_ID",all.x=T) #a handful of hauls are missing from samples bc none of 12 spp were caught. one entry for each haul. Fill in with Gcha, then expand.grid works on next step. Catch recorded as NA, then sub zero
alls$SPECIES_NAME[is.na(alls$SPECIES_NAME)==T]<-"Gadus chalcogrammus"

specieslist<-c(as.character(unique(alls$SPECIES_NAME)))
tt<-expand.grid(unique(alls$HAUL_ID),specieslist)
colnames(tt)<-c("HAUL_ID","SPECIES_NAME")
all0<-merge(alls[,c("HAUL_ID","SPECIES_NAME","LARVALCATCHPER10M2","LARVALCATCHPER1000M3")],tt,by=c("HAUL_ID","SPECIES_NAME"),all.y=TRUE)
all0$LARVALCATCHPER1000M3[is.na(all0$LARVALCATCHPER1000M3)==T]<-0
all0$LARVALCATCHPER10M2[is.na(all0$LARVALCATCHPER10M2)==T]<-0
all0<-merge(all0,hauls,by="HAUL_ID")

tapply(all0$LARVALCATCHPER10M2,all0$SPECIES_NAME,mean)

#Note that for 2019, some species will have catches of zero at each station ONLY BECAUSE not verified yet.
#Remove these later for plotting and timeseries.


#Remove extra files from working environment
rm(hauls.b,hauls.t,samples.b,samples.t,tt)

mydat<-all0


plot(mydat$YDAY~mydat$YEAR) #NOte that in 2017, stations 45-51 were early (days 134-135), and sampled out of order geographically. At southern edge. Do not include? Exclude by using yday 137-158.
#NOte that in 2017, stations 45-51 were early (days 134-135), and sampled out of order geographically. At southern edge. Do not include? Exclude by using yday 137-158.
#Sampling in 2019 was almost all early. Subset dates after areas.

#mydat<-mydat[mydat$YDAY %in% 137:158,] 

#write.csv(mydat,"GOALarvalCatchwZeros_d137to158_May2019.csv",row.names=F)


#Extract only stations within criteria for time-series.
#First, read in spatial polygon, convert to Lat/lon projection.
#shp.mp <- readShapePoly("~/Projects/GOA 2013 2015/GOAdata/MapData/regionlatemay")
#proj4string(shp.mp)<-CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")  ##Equal area Albers"
#shp.mp.LL<-spTransform(shp.mp,CRS("+proj=longlat"))

shp.mp <- ss<-readOGR(dsn="~/Projects/GOA ichthyoplankton time series/NewLateLarvalPolygon",layer="Survey_poly")
shp.mp@data<-data.frame(1) #so that over() function works later
shp.mp.LL<-spTransform(shp.mp,CRS("+proj=longlat"))

#x11()
map('worldHires',xlim=c(-170,-140),ylim=c(52,62),fill=T,col="gray",border=F)
points(mydat$LON,mydat$LAT,pch=".",cex=2)
map(shp.mp.LL,add=T,col="blue",lwd=2)
map.axes()

#Create spatial points object from mydat, and use over() function to identify stations that are within "late may" time series polygon.
LLs<-SpatialPoints(mydat[,c("LON","LAT")],CRS("+proj=longlat"))
test<-over(LLs,shp.mp.LL) #NA if not in polygon, 0 if in polygon ##USE this function on abundance
mydat$Core<-test
mydat$Core[mydat$Core==1]<-T
mydat$Core[is.na(mydat$Core)==T]<-F

points(LLs[mydat$Core==T,],col="red")
points(LLs[mydat$Core==F,],col="green")

coredat<-mydat[mydat$Core==T,] #stored as 0,1 but seems OK

#Check that dates are within core sampling dates (May 17 - June 7) yday 137 - 158 (includes all of 2DY11 and 5Dy15)

plot(coredat$YDAY~coredat$YEAR)
abline(h=158);abline(h=138) #basically can use 138-158 for all years except 2019, which is 131-138, a week early.


#Go back and check for polygonal areas for all core stations:
table(coredat$CRUISE,is.na(coredat$POLYGONAL_AREA))
coredat[is.na(coredat$POLYGONAL_AREA)==T,]
table(coredat$HAUL_PERFORMANCE,is.na(coredat$POLYGONAL_AREA))
#Poly Areas missing for some questionable hauls. Kathy/Susan say these were removed on purpose. Also need to check computation for years with multiple cruises.

#For now, remove hauls with no poly areas:
coredat<-coredat[is.na(coredat$POLYGONAL_AREA)==F,]

#Sort coredat so that years/cruises are in order:
coredat<-coredat[order(coredat$GMTDATETIME),]

#############################################
#############################################
# Add FOCI_GRID to coredat data so I can subset it for scenarios
# use nn2 function from RANN library - much faster than distance calcs and seems to give correct IDs.
# This gives the FOCI_GRID code for the nearest grid station by lat/lon


testdist<-nn2(grid[,c("longitude","latitude")],coredat[,c("LON","LAT")],k=1)

coredat$FOCI_GRID<-grid$FOCI_GRID[testdist$nn.idx]

#############################################
#  Start sampling scenarios. Polygonal areas will be incorrect, will need to update for each
#

setwd(mywd)
### Read in scenarios
Scen1<-read.csv("Scenario1.csv")
Scen2<-read.csv("Scenario2.csv")
Scen3<-read.csv("Scenario3.csv")
Scen4<-read.csv("Scenario4.csv")
Scen4p<-read.csv("Scenario4plus.csv")

## detailed alaska map data
AK.map <- map("world2Hires", "USA:Alaska", fill=TRUE, col="transparent", plot=FALSE)
IDs <- sapply(strsplit(AK.map$names, ":"), function(x) x[1])
AK.map.sp <- map2SpatialPolygons(AK.map, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
AK.map.sp.aea<-spTransform(AK.map.sp,CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

#Get just one record for each haul

corehauls<-coredat[coredat$SPECIES_NAME=="Gadus chalcogrammus",c("HAUL_ID","CRUISE","FOCI_GRID","LAT","LON","YEAR","POLYGONAL_AREA")] 

corehauls$POLY_AREA_Base_15<-NA
corehauls$POLY_AREA_Scen1_15<-NA
corehauls$POLY_AREA_Scen2_15<-NA
corehauls$POLY_AREA_Scen3_15<-NA
corehauls$POLY_AREA_Scen4_15<-NA
corehauls$POLY_AREA_Scen4p_15<-NA

#######################
# Choose scenario
MyScen<-Scen4p
MyScenName<-"Scen4p"

#MyScenName<-"Base"

ScenInds<-which(corehauls$FOCI_GRID %in% MyScen$FOCI_GRID)
ScenHauls<-corehauls[ScenInds,]
#ScenHauls<-corehauls  ## Use this for Base scenario only


dat.ll<-ScenHauls
coordinates(dat.ll)<-c("LON","LAT")
dat.ll<-SpatialPoints(dat.ll,CRS("+proj=longlat"))
dat.aea<-spTransform(dat.ll,CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

### Loop through years to calculate polygonal areas for each
###

pdf(paste("SamplingScenarios_Polygons_",MyScenName,"_15nmBuffer.pdf"),width=8.5,height=11)
par(mfrow=c(3,4),mar=c(1,1,1,1))
allyrs<-unique(coredat$YEAR)
for(yr in allyrs){

  yrinds<-which(ScenHauls$YEAR == yr)
  
  buffdist<-13890*2 #13890 m is 7.5 nm. -- how to determine?!
  buff<-buffer(dat.aea[yrinds],buffdist,dissolve=TRUE) 

  buff.noland<-erase(buff,AK.map.sp.aea)

  plot(buff) #spatial polygons
  plot(dat.aea[yrinds],add=T,pch=16,cex=0.5,col="red2") #spatial points
  polys<-voronoi.polygons(dat.aea[yrinds],buff.noland) #Calculate polygon areas.

  plot(AK.map.sp.aea,col="gray",lty=0,add=T)
  plot(polys,add=T)
  legend("bottomright",legend=c(yr,paste(length(yrinds),"Stns")),bty="n")
  box()
  ##***** NEED TO MANUALLY CHANGE COLUMN NAME FOR SCENARIO BELOW ********
  corehauls$POLY_AREA_Scen4p_15[ScenInds][yrinds]<-polys$area/1000000 #to convert to km2
#  corehauls$POLY_AREA_Base_15[yrinds]<-polys$area/1000000 #to convert to km2
  
}
dev.off()


#write.csv(corehauls[,c(1:7,13)],"corehauls_wPolyAreasforScenario4p.csv",row.names=F)
corehaulsss<-read.csv("corehauls_wPolyAreasforScenarios.csv")
corehauls2<-merge(corehaulsss,corehauls[,c(1:7,13)])
#### Merge polygonal area info for each haul with catch dataframe (multispecies)

coredat2<-merge(coredat,corehauls2,all.x=T)


##################################
#Now calculate index of abundance for each species, weighted by polygonal area.
#################

ScenList<-list(Scen1,Scen2,Scen3,Scen4,Scen4p)
MyScens<-0:5

for(i in MyScens){
  if(i==0) {
    coredat_scen<-coredat2
  } else {
    coredat_scen<-coredat2[coredat2$FOCI_GRID %in% ScenList[[i]]$FOCI_GRID,]
  }
coredat_scen$POLY_AREA_TOUSE<-coredat_scen[,(i+28)]

#Mean CPUE is the sum of the CPUA_i * Area_i, divided by total area.

#Calculate total "polygonal areas" for each cruise (and species - to get correct #) #consider whether this should by by year instead.
TotalAreas<-tapply(coredat_scen$POLY_AREA_TOUSE,list(coredat_scen$YEAR,coredat_scen$SPECIES_NAME),sum)

#Create new column with CPUA*Area for each species/station
coredat_scen$AREA_CATCH<-coredat_scen$LARVALCATCHPER10M2 * coredat_scen$POLY_AREA_TOUSE

#Take sum of Area_Catch for each species and cruise and divide by TotalArea for each cruise
SumCatch<-tapply(coredat_scen$AREA_CATCH,list(coredat_scen$YEAR,coredat_scen$SPECIES_NAME),sum)
MeanCatch<-SumCatch/(TotalAreas) #area-weighted mean ## 2013 Matches Kathy's files!

AveCatch<-tapply(coredat_scen$LARVALCATCHPER10M2,list(coredat_scen$YEAR,coredat_scen$SPECIES_NAME),mean) #unweighted mean

#Calculate variance: shortcut is to use canned variance function, multiply by the number of stations, and divide by the square of the sum of all polygonal areas. Var_corrected = Var*(n/(TotalArea^2))

VarCA<-tapply(coredat_scen$AREA_CATCH,list(coredat_scen$YEAR,coredat_scen$SPECIES_NAME),var)
nStns<-tapply(coredat_scen$LARVALCATCHPER10M2,list(coredat_scen$YEAR,coredat_scen$SPECIES_NAME),length)
A2<-tapply(coredat_scen$POLY_AREA_TOUSE,list(coredat_scen$YEAR,coredat_scen$SPECIES_NAME),function(x) sum(x)^2)
VarCatch<-VarCA*nStns/A2
se <- function(x) sqrt(var(x)/length(x))
SESimp<-tapply(coredat_scen$LARVALCATCHPER10M2,list(coredat_scen$YEAR,coredat_scen$SPECIES_NAME),se)
SE_Catch<-sqrt(VarCatch)
#Kathy reports "SEMEAN", which is the sqrt of the variance.



###
SE_stacked<-stack(as.data.frame(SE_Catch))
colnames(SE_stacked)<-c("CPUE_SE","Species")
Mean_stacked<-stack(as.data.frame(MeanCatch))
colnames(Mean_stacked)<-c("MeanCPUE","Species")
Years<-as.numeric(rownames(MeanCatch))
Mean_stacked$Year<-rep(Years,12)
Mean_stacked$CPUE_SE<-SE_stacked$CPUE_SE
ts<-Mean_stacked

#Add species common names
species<-read.csv("SpeciesNamesCodes.csv")

ts<-merge(ts,species[,c("Species","Common_Name2")])

write.csv(ts,paste0("TimeSeries_Scenario_",i,".csv"),row.names=F)
}


##########################
# End for simulations
###########################



#pdf("GOA_Ich_TimeSeries_NewPolygonalAraeas.pdf",height=10, width=8)
#x11(width=8,height=10)
par(mfrow=c(6,2),mar=c(3,3,1,1),oma=c(0,3,1,0))
spp<-unique(ts$Common_Name2)
sppos<-c("topleft","topright","top")[c(1,2,3,1,1,3,1,1,1,1,1,1)] #position of species name
for(i in 1:length(spp)){
  mysp<-spp[i]
  toplot<-ts[ts$Common_Name2==mysp,]
  Years<-1981:2019
  toplot<-merge(as.data.frame(Years),toplot,by.x="Years",by.y="Year",all.x=T)
  plot(MeanCPUE~Years, pch=21,data=toplot,ylab="",ylim=c(0,max(toplot$MeanCPUE+toplot$CPUE_SE,na.rm=T)),cex.axis=1.2,cex.main=1.5,cex=1.5,col=c(rep(1,38),"red"),bg=1) 
  abline(h=mean(toplot$MeanCPUE,na.rm=T),lty=2,col="gray")
  arrows(toplot$Years,toplot$MeanCPUE-toplot$CPUE_SE,toplot$Year,toplot$MeanCPUE+toplot$CPUE_SE, code=3, length=0, angle = 90,lwd=1.5)
  lines(MeanCPUE~Years,data=toplot)
  mtext(side=3,mysp,font=2)
 # points(MeanCPUE~Years, pch=16,data=toplot,cex=1.5) 
  
  #legend(sppos[i],legend=mysp, bty="n",cex=1.5)
}
mtext(side=2,outer=T,expression(Mean~abundance~(no.~per~10~m^2)))
dev.off()


#######
#Survey summary statistics for Table:
#######

nStns.tab<-tapply(coredat$STATION_NAME,coredat$CRUISE,function(x) length(unique(x))) #not in order
Year.tab<-tapply(coredat$YEAR,coredat$CRUISE,mean,na.rm=T)
StartDate.tab<-tapply(coredat$YDAY,coredat$CRUISE,min,na.rm=T)
EndDate.tab<-tapply(coredat$YDAY,coredat$CRUISE,max,na.rm=T)
MedDate.tab<-tapply(coredat$YDAY,coredat$CRUISE,median,na.rm=T)

mytable<-data.frame(cbind(Year.tab,StartDate.tab,EndDate.tab,MedDate.tab,nStns.tab))
mytable<-mytable[order(mytable$Year.tab),]
mytable$DateShift<-mytable$MedDate.tab-148

