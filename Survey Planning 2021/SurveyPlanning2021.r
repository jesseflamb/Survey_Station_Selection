# Analysis of survey scenarios for 2021 spring larval survey planning

library(maps);library(mapdata);library(geosphere);library(lattice)


grid<-read.csv("EcoFOCI_GOAIERP_spreadsheet.csv")
grid$FOCI_GRID<-toupper(grid$xy)
scen<-read.csv("StationRunTime_wo21_01.csv")
LRscen<-read.csv("StationScenarios.csv")
hauls19<-read.csv("~/Projects/GOA ichthyoplankton time series/Data_For2020Update/plyDY19_05area.csv") #this file direct from Kimberly
#hauls19 doesn't have FOX stations - ?

dat19<-read.csv("~/Projects/GOA ichthyoplankton time series/Data_For2019Update/DY19_05_StationData_LatsLonsFixed.csv")
dat19<-dat19[dat19$NET==1,]
dat19$FOCI_GRID[dat19$FOCI_GRID=="GT135`"]<-"GT135"
dat19$FOCI_GRID[dat19$FOCI_GRID=="FOX060"]<-"FOX60"

age0<-read.csv("Sites_2021_age0.csv")
CalVET<-read.csv("CalVETS_Stns.csv")
Neuston<-read.csv("Neuston_Stns.csv")

# Don't use CheckerB2 - takes longer than CheckerB for same stations
LRscen<-LRscen[,1:4]


#fix Homer arrival Longitude (from positive)
scen$DecLong[scen$DecLong>0]<- -151.5483

map("worldHires",xlim=c(-165,-145),ylim=c(53.9,61),fill=T,col="gray",border=F)
points(grid$longitude,grid$latitude,pch=".")
points(scen$DecLong,scen$DecLat,type="o",col=2)
map.axes()


inds1<-match(LRscen[,1],dat19$STATION_NAME)
inds2<-match(LRscen[,2],dat19$STATION_NAME)
inds3<-match(LRscen[,3],dat19$STATION_NAME)
inds4<-match(LRscen[,4],dat19$STATION_NAME)
#inds5<-match(LRscen[,5],dat19$STATION_NAME)

dists1<-distGeo(matrix(cbind(dat19$LON[inds1],dat19$LAT[inds1]),byrow=FALSE,ncol=2,nrow=length(dat19$LON[inds1])))
dists2<-distGeo(matrix(cbind(dat19$LON[inds2],dat19$LAT[inds2]),byrow=FALSE,ncol=2,nrow=length(dat19$LON[inds2])))
dists3<-distGeo(matrix(cbind(dat19$LON[inds3],dat19$LAT[inds3]),byrow=FALSE,ncol=2,nrow=length(dat19$LON[inds3])))
dists4<-distGeo(matrix(cbind(dat19$LON[inds4],dat19$LAT[inds4]),byrow=FALSE,ncol=2,nrow=length(dat19$LON[inds4])))
#dists5<-distGeo(matrix(cbind(dat19$LON[inds5],dat19$LAT[inds5]),byrow=FALSE,ncol=2,nrow=length(dat19$LON[inds5])))

SummaryTable<-data.frame("Scenario"=colnames(LRscen),"NStations"=c(69,65,69,63,63),
                         "TotalDistance_NM"=c(sum(dists1,na.rm=T)/(1000*1.852),sum(dists2,na.rm=T)/(1000*1.852),
                                              sum(dists3,na.rm=T)/(1000*1.852),
                                              sum(dists4,na.rm=T)/(1000*1.852),sum(dists5,na.rm=T)/(1000*1.852)))




pdf("SamplingScenarios.pdf",width=11,height=8.5)

par(mfrow=c(2,2),mar=c(2,2,1,1))
for(i in 1:length(colnames(LRscen))){
  
map("worldHires",xlim=c(-165,-145),ylim=c(53.9,61),fill=T,col="gray",border=F)

  myinds<-match(LRscen[,i],dat19$STATION_NAME)
  points(dat19$LON[myinds],dat19$LAT[myinds],type="o",cex=0.7,col="darkblue")
 legend("bottomright",legend=c(i,colnames(LRscen)[i], paste(round(SummaryTable[i,3]),"nm"), paste(SummaryTable[i,2],"stations")) 
                               ,bty="n")
 points(dat19$LON,dat19$LAT,pch=".")
 
#points(dat19$LON[inds2],dat19$LAT[inds2],type="o")
#points(dat19$LON[inds3],dat19$LAT[inds3],type="o")
#points(dat19$LON[inds4],dat19$LAT[inds4],type="o")
#points(dat19$LON[inds5],dat19$LAT[inds5],type="o")
  map.axes()
}

dev.off()

scen4plus<-read.csv("Scenario4plus.csv")

#############
pdf("CompareScen4andAge0grid_wNeuston_wCalVet_extraStns.pdf",width=11,height=8.5)

  map("worldHires",xlim=c(-165,-145),ylim=c(53.9,61),fill=T,col="gray",border=F)
  
  myinds<-match(LRscen[,4],dat19$STATION_NAME)
#  points(dat19$LON[myinds],dat19$LAT[myinds],type="o",cex=0.7,col="darkblue",pch=16)
  points(dat19$LON,dat19$LAT,pch=16, cex=0.2)
  points(age0$longitude,age0$latitude,col="gray30")
   
  points(CalVET$LON,CalVET$LAT,pch=1, col="red3",cex=0.6)
  points(Neuston$LON,Neuston$LAT,pch=1,col="turquoise",cex=0.6)
 
  legend("bottomright",pch=c(16,1,16,1,1), col=c("darkblue","gray30",1,"red3","turquoise"),
         lty=c(1,NA,NA,NA,NA),pt.cex=c(0.7,1,0.2,1,1),
         legend=c("Scenario 4", "Age-0 Grid","DY19-05","CalVET Grid","Neuston Grid"), bty="n")
  
  points(scen4plus$FOCI_GRID_LON,scen4plus$FOCI_GRID_LAT,pch=16,col="darkblue",cex=0.7,type="o")
   #points(dat19$LON[inds2],dat19$LAT[inds2],type="o")
  #points(dat19$LON[inds3],dat19$LAT[inds3],type="o")
  #points(dat19$LON[inds4],dat19$LAT[inds4],type="o")
  #points(dat19$LON[inds5],dat19$LAT[inds5],type="o")
#  text(dat19$LON,dat19$LAT,pch=16, cex=0.5,label=dat19$STATION_NAME)
  
  map.axes()

dev.off()

############

pdf("FOCI_GRID.pdf",width=20,height=16)
map("worldHires",xlim=c(-165,-145),ylim=c(53.9,61),fill=T,col="gray",border=F)
text(grid$longitude,grid$latitude,labels=grid$xy,cex=0.6)
map.axes()
dev.off()


pdf("FOCI_GRID_wScenario4plus.pdf",width=20,height=16)
map("worldHires",xlim=c(-165,-145),ylim=c(53.9,61),fill=T,col="gray",border=F)
text(grid$longitude,grid$latitude,labels=grid$xy,cex=0.6)
points(scen4plus$FOCI_GRID_LON,scen4plus$FOCI_GRID_LAT,pch=16,col="darkblue",cex=0.7,type="o")

map.axes()
dev.off()


Scen1df<-data.frame("DY19Station"=dat19$STATION_NAME[inds1],"DY19LON"=dat19$LON[inds1],"DY19LAT"=dat19$LAT[inds1],"FOCI_GRID"=dat19$FOCI_GRID[inds1])
gridinds1<-match(Scen1df$FOCI_GRID,grid$FOCI_GRID)
Scen1df$FOCI_GRID_LON<-grid$longitude[gridinds1]
Scen1df$FOCI_GRID_LAT<-grid$latitude[gridinds1]


Scen2df<-data.frame("DY19Station"=dat19$STATION_NAME[inds2],"DY19LON"=dat19$LON[inds2],"DY19LAT"=dat19$LAT[inds2],"FOCI_GRID"=dat19$FOCI_GRID[inds2])
gridinds2<-match(Scen2df$FOCI_GRID,grid$FOCI_GRID)
Scen2df$FOCI_GRID_LON<-grid$longitude[gridinds2]
Scen2df$FOCI_GRID_LAT<-grid$latitude[gridinds2]


Scen3df<-data.frame("DY19Station"=dat19$STATION_NAME[inds3],"DY19LON"=dat19$LON[inds3],"DY19LAT"=dat19$LAT[inds3],"FOCI_GRID"=dat19$FOCI_GRID[inds3])
gridinds3<-match(Scen3df$FOCI_GRID,grid$FOCI_GRID)
Scen3df$FOCI_GRID_LON<-grid$longitude[gridinds3]
Scen3df$FOCI_GRID_LAT<-grid$latitude[gridinds3]


Scen4df<-data.frame("DY19Station"=dat19$STATION_NAME[inds4],"DY19LON"=dat19$LON[inds4],"DY19LAT"=dat19$LAT[inds4],"FOCI_GRID"=dat19$FOCI_GRID[inds4])
gridinds4<-match(Scen4df$FOCI_GRID,grid$FOCI_GRID)
Scen4df$FOCI_GRID_LON<-grid$longitude[gridinds4]
Scen4df$FOCI_GRID_LAT<-grid$latitude[gridinds4]

write.csv(Scen1df,"Scenario1.csv",row.names=F)
write.csv(Scen2df,"Scenario2.csv",row.names=F)
write.csv(Scen3df,"Scenario3.csv",row.names=F)
write.csv(Scen4df,"Scenario4.csv",row.names=F)


#Create an updated scenario4 with some additional stations on edges - do manually in excel.
#Scenario4plus.csv

#######################
# Calculate what time-series would look like for different scenarios historically
# This done in GOAIchTimeSeriesCalcs_2021SurveySimulations.r
# Note that subsetting historical surveys does not look exactly like the scenarios
# Survey grids varied a lot historically - check out maps of polygonal areas for different scenarios

## Read in time-series associated with each scenario
#scenario 0 is the base (all stations) but with my recalculated polygonal areas

TS0<-read.csv("TimeSeries_Scenario_0.csv") 
TS1<-read.csv("TimeSeries_Scenario_1.csv")
TS2<-read.csv("TimeSeries_Scenario_2.csv")
TS3<-read.csv("TimeSeries_Scenario_3.csv")
TS4<-read.csv("TimeSeries_Scenario_4.csv")
TS4p<-read.csv("TimeSeries_Scenario_5.csv")

plot(TS0$MeanCPUE,TS1$MeanCPUE)

#Look at years since 2000 since surveys were more often different design before that.
TS0<-TS0[TS0$Year > 2000,]
TS1<-TS1[TS1$Year > 2000,]
TS2<-TS2[TS2$Year > 2000,]
TS3<-TS3[TS3$Year > 2000,]
TS4<-TS4[TS4$Year > 2000,]
TS4p<-TS4p[TS4p$Year > 2000,]


TSlist<-list(TS0,TS1,TS2,TS3,TS4,TS4p)




#######################

spp<-unique(TS0$Common_Name2)

pdf("TimeSeries_ScenarioComparison_with4p.pdf",width=8,height=8)
for(i in 1:length(spp)){
  par(mfrow=c(3,2),mar=c(3,3,3,1))
  mysp<-spp[i]
    inds<-which(TS0$Common_Name2 %in% mysp)
    for(scen in c(1:5,0)){
      MyTS<-TSlist[[scen+1]]
      toplot<-MyTS[MyTS$Common_Name2==mysp,]
      Years<-1981:2019
      toplot<-merge(as.data.frame(Years),toplot,by.x="Years",by.y="Year",all.x=T)
      plot(MeanCPUE~Years, pch=21,data=toplot,ylab="",ylim=c(0,max(toplot$MeanCPUE+toplot$CPUE_SE,na.rm=T)),cex.axis=1.2,cex.main=1.5,cex=1.5,bg=1) 
      abline(h=mean(toplot$MeanCPUE,na.rm=T),lty=2,col="gray")
      arrows(toplot$Years,toplot$MeanCPUE-toplot$CPUE_SE,toplot$Year,toplot$MeanCPUE+toplot$CPUE_SE, code=3, length=0, angle = 90,lwd=1.5)
      lines(MeanCPUE~Years,data=toplot)
      mtext(side=3,mysp,font=2)
      legend("topleft",legend=paste("Scenario",scen),bty="n")
    
  }
  mtext(side=2,outer=T,expression(Mean~abundance~(no.~per~10~m^2)))
  
}
dev.off()


####
# Need to calculate error metrics - bias, MSE, Mean absolute error, etc. 
###

library(RColorBrewer)

# FIrst, how correlated are the time-series?

ScenCors<-matrix(NA,length(spp),5)
ScenLCors<-matrix(NA,length(spp),5)

for (i in 1:length(spp)){
  mysp<-spp[i]
  spinds<-which(TS1$Common_Name2==mysp)
  baseSp<-TSlist[[1]][spinds,"MeanCPUE"]
  Cor1<-cor(baseSp,TSlist[[2]][spinds,"MeanCPUE"])
  Cor2<-cor(baseSp,TSlist[[3]][spinds,"MeanCPUE"])
  Cor3<-cor(baseSp,TSlist[[4]][spinds,"MeanCPUE"])
  Cor4<-cor(baseSp,TSlist[[5]][spinds,"MeanCPUE"])
  Cor5<-cor(baseSp,TSlist[[6]][spinds,"MeanCPUE"])
  
  lCor1<-cor(log(baseSp+0.1),log(TSlist[[2]][spinds,"MeanCPUE"]+0.1))
  lCor2<-cor(log(baseSp+0.1),log(TSlist[[3]][spinds,"MeanCPUE"]+0.1))
  lCor3<-cor(log(baseSp+0.1),log(TSlist[[4]][spinds,"MeanCPUE"]+0.1))
  lCor4<-cor(log(baseSp+0.1),log(TSlist[[5]][spinds,"MeanCPUE"]+0.1))
  lCor5<-cor(log(baseSp+0.1),log(TSlist[[6]][spinds,"MeanCPUE"]+0.1))
  
  ScenCors[i,]<-c(Cor1,Cor2,Cor3,Cor4,Cor5)
  ScenLCors[i,]<-c(lCor1,lCor2,lCor3,lCor4,lCor5)
  
  
}

Corranks<-apply(ScenCors*-1,1,rank)
LCorranks<-apply(ScenLCors*-1,1,rank)


pdf("Scenario_TimeseriesCorrelations_since2001_w4plus.pdf",width=10,height=6)
par(mfcol=c(2,2))
print(levelplot(ScenCors,as.table=T,col.regions=colorRampPalette(brewer.pal(9,"GnBu")),
          ylab="Scenario",xlab="Species",main="Time-series Correlation"), split=c(1, 1, 2, 2))
print(levelplot(ScenLCors,as.table=T,col.regions=colorRampPalette(brewer.pal(9,"GnBu")),
          ylab="Scenario",xlab="Species",main="Time-series Correlation (log)"),split=c(1, 2, 2, 2),newpage=F)


print(levelplot(t(Corranks),as.table=T,col.regions=colorRampPalette(brewer.pal(9,"Blues")), cuts=4,
          ylab="Scenario",xlab="Species",main="Time-series Correlation  Ranks (1 is best)"), split=c(2, 1, 2, 2),newpage=F)
print(levelplot(t(LCorranks),as.table=T,col.regions=colorRampPalette(brewer.pal(9,"Blues")),cuts=4,
          ylab="Scenario",xlab="Species",main="Time-series Correlation (log)  Ranks (1 is best)"), split=c(2, 2, 2, 2),newpage=F)
dev.off()


# Second, what's the mean absolute error, real and log-scale?

ScenMAE<-matrix(NA,length(spp),5)
ScenLMAE<-matrix(NA,length(spp),5)

for (i in 1:length(spp)){
  mysp<-spp[i]
  spinds<-which(TS1$Common_Name2==mysp)
  baseSp<-TSlist[[1]][spinds,"MeanCPUE"]
  MAE1<-mean(abs(baseSp-TSlist[[2]][spinds,"MeanCPUE"]))
  MAE2<-mean(abs(baseSp-TSlist[[3]][spinds,"MeanCPUE"]))
  MAE3<-mean(abs(baseSp-TSlist[[4]][spinds,"MeanCPUE"]))
  MAE4<-mean(abs(baseSp-TSlist[[5]][spinds,"MeanCPUE"]))
  MAE5<-mean(abs(baseSp-TSlist[[6]][spinds,"MeanCPUE"]))
  
  lMAE1<-mean(abs(log(baseSp+0.1)-log(TSlist[[2]][spinds,"MeanCPUE"]+0.1)))
  lMAE2<-mean(abs(log(baseSp+0.1)-log(TSlist[[3]][spinds,"MeanCPUE"]+0.1)))
  lMAE3<-mean(abs(log(baseSp+0.1)-log(TSlist[[4]][spinds,"MeanCPUE"]+0.1)))
  lMAE4<-mean(abs(log(baseSp+0.1)-log(TSlist[[5]][spinds,"MeanCPUE"]+0.1)))
  lMAE5<-mean(abs(log(baseSp+0.1)-log(TSlist[[6]][spinds,"MeanCPUE"]+0.1)))
  
  ScenMAE[i,]<-c(MAE1,MAE2,MAE3,MAE4,MAE5)
  ScenLMAE[i,]<-c(lMAE1,lMAE2,lMAE3,lMAE4,lMAE5)
}

MAEranks<-apply(ScenMAE,1,rank)
LMAEranks<-apply(ScenLMAE,1,rank)


pdf("Scenario_TimeseriesError_since2001-with4p.pdf",width=10,height=6)
par(mfcol=c(2,2))

print(levelplot(ScenMAE,as.table=T,col.regions=colorRampPalette(brewer.pal(9,"Reds")),
          ylab="Scenario",xlab="Species",main="Mean Absolute Error (lower is better)"), split=c(1, 1, 2, 2))

print(levelplot(ScenLMAE,as.table=T,col.regions=colorRampPalette(brewer.pal(9,"Reds")),
          ylab="Scenario",xlab="Species",main="Mean Absolute Log Error (lower is better)"),split=c(1, 2, 2, 2),newpage=F)

print(levelplot(t(MAEranks),col.regions=colorRampPalette(brewer.pal(9,"Reds")),cuts=4,
          ylab="Scenario",xlab="Species",main="Mean Absolute Error Rank (1 is best)"), split=c(2, 1, 2, 2),newpage=F)

print(levelplot(t(LMAEranks),as.table=T,col.regions=colorRampPalette(brewer.pal(9,"Reds")),cuts=4,
          ylab="Scenario",xlab="Species",main="Mean Absolute Log Error Rank (1 is best)"), split=c(2, 2, 2, 2),newpage=F)

dev.off()
