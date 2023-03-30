# Plot historical spring larval surveys for comparison
# Currently plotting 60BON
# -LR 
require(RODBC); require(here);require(tidyverse)
require(lubridate); require(ggmap);require(rnaturalearthdata)

user <- readline("Input Username: ")
pswd <- readline("Input Password: ")

AFSCconnect <- odbcConnect("AFSC", uid=user,  pwd=pswd)

sqlQuery(AFSCconnect,"DROP TABLE HAUL;")
sqlQuery(AFSCconnect,"CREATE TABLE HAUL AS SELECT * FROM ECODAAT.HAUL;")

BonHaulQuery <-
  paste0(
    "SELECT 
      COMMENTS_HAUL, CRUISE, DAY, GEAR_NAME, GMT_DATE_TIME, HAUL_ID, HAUL_NAME,       
      HAUL_PERFORMANCE, LAT, LON, MAX_GEAR_DEPTH, MESH, MONTH, NET,           
      POLYGONAL_AREA, PRIMARY_NET, PURPOSE,  STATION_NAME, YEAR    
   FROM HAUL 
   WHERE GEAR_NAME LIKE '60BON' AND
    PRIMARY_NET IN('Y') AND
    HAUL_PERFORMANCE IN('GOOD','QUEST') AND
    CRUISE IN('3SH81', '4MF81', '2DA82', '1CH83', '2PO85', '3MF87', '4MF90', '4MF91', '4MF92', '5MF93', '6MF94', '8MF95', '8MF96', '8MF97',
    '5MF98', '2WE99', '5MF99', '6MF00', '3MF01', '4MF02', '5MF03', '5MF04', '6MF05', '4MF06', '5MF07', '4DY08', '4DY09', '3DY10',
     '2DY11', 'DY13-06', 'DY15-05', 'DY17-05', 'DY19-05','WO21-01')"
  ) #     PURPOSE IN('GRID') AND


hauls.b<-sqlQuery(AFSCconnect,BonHaulQuery) # 4677

hauls.b <- hauls.b %>% 
  mutate(GMT_DATE_TIME = ymd_hms(GMT_DATE_TIME)) %>%
  mutate(YDAY = yday(GMT_DATE_TIME)) %>%
   mutate(CRUISE = as.factor(CRUISE)) %>%
  mutate(CRUISE=reorder(CRUISE,YEAR)) %>%
  arrange(GMT_DATE_TIME) 
  
world <- ne_countries(scale = "medium",country = c("United States of America"), returnclass = "sf")

lons = c(-167, -145) #-140 = large map
lats = c(53, 61) # 52 = large map

#world <- map_data("world2")

g<-ggplot() +
  geom_sf(data = world) +
  coord_sf(xlim = lons, ylim = lats, expand = FALSE) +
  geom_point(data=hauls.b, mapping = aes(LON,LAT,color=YDAY),size=0.5) +
  geom_path(data=hauls.b, mapping = aes(LON,LAT,color=YDAY)) +
  facet_wrap(~CRUISE) +
  scale_color_viridis_c() +
  theme_bw()
# +
#  
ggsave("Figures/HistoricalSurveyMaps.png")

#Not sure how to get facet_wrap to plot across multiple pages
#
#
makemaps<-function(dat=hauls.b, years = 1981:2021 ) { ggplot() +
  geom_sf(data = world) +
  coord_sf(xlim = lons, ylim = lats, expand = FALSE) +
  geom_point(data=dat[dat$YEAR %in% years,], mapping = aes(LON,LAT,color=YDAY),size=0.5) +
  geom_path(data=dat[dat$YEAR %in% years,], mapping = aes(LON,LAT,color=YDAY)) +
  facet_wrap(~CRUISE) +
  scale_color_viridis_c() +
  theme_bw()
}

g1<-makemaps(hauls.b, 1981:1995)
g2<-makemaps(hauls.b, 1996:2006)
g3<-makemaps(hauls.b, 2007:2021)
#g4<-makemaps(hauls.b, 1999:2021)

g1
ggsave("Figures/HistoricalSurveyMaps_1981_1995.png",width=11,height=8.5)
g2
ggsave("Figures/HistoricalSurveyMaps_1996_2006.png",width=11,height=8.5)
g3
ggsave("Figures/HistoricalSurveyMaps_2007_2021.png",width=11,height=8.5)
