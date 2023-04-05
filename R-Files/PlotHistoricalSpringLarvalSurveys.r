# Plot historical spring larval surveys for comparison
# 
# -LR 
require(RODBC); require(here);require(tidyverse)
require(lubridate); require(ggmap);require(rnaturalearthdata);require(rnaturalearth)

user <- readline("Input Username: ")
pswd <- readline("Input Password: ")

AFSCconnect <- odbcConnect("AFSC", uid=user,  pwd=pswd)

sqlQuery(AFSCconnect,"DROP TABLE HAUL;")
sqlQuery(AFSCconnect,"CREATE TABLE HAUL AS SELECT * FROM ECODAAT.HAUL;")

HaulQuery <-
  paste0(
    "SELECT 
      COMMENTS_HAUL, CRUISE, DAY, GEAR_NAME, GMT_DATE_TIME, HAUL_ID, HAUL_NAME,       
      HAUL_PERFORMANCE, LAT, LON, MAX_GEAR_DEPTH, MESH, MONTH, NET,           
      POLYGONAL_AREA, PRIMARY_NET, PURPOSE,  STATION_NAME, YEAR    
   FROM HAUL 
   WHERE GEAR_NAME IN('60BON','CALVET','20BON','CTD','CTDB') AND
    CRUISE IN('3SH81', '4MF81', '2DA82', '1CH83', '2PO85', '3MF87', '4MF90', '4MF91', '4MF92', '5MF93', '6MF94', '8MF95', '8MF96', '8MF97',
    '5MF98', '2WE99', '5MF99', '6MF00', '3MF01', '4MF02', '5MF03', '5MF04', '6MF05', '4MF06', '5MF07', '4DY08', '4DY09', '3DY10',
     '2DY11', 'DY13-06', 'DY15-05', 'DY17-05', 'DY19-05','WO21-01')"
  ) #     PURPOSE IN('GRID') AND


hauls<-sqlQuery(AFSCconnect,HaulQuery) # 4677 60BOn stations

hauls <- hauls %>% 
  mutate(GMT_DATE_TIME = ymd_hms(GMT_DATE_TIME)) %>%
  mutate(YDAY = yday(GMT_DATE_TIME)) %>%
   mutate(CRUISE = as.factor(CRUISE)) %>%
  mutate(CRUISE=reorder(CRUISE,YEAR)) %>%
  arrange(GMT_DATE_TIME) 

hauls.60b <- hauls %>%
  filter(GEAR_NAME == "60BON") %>%
  filter(PRIMARY_NET == "Y")

hauls.20b <- hauls %>%
  filter(GEAR_NAME == "20BON")
  
hauls.cal <- hauls %>%
  filter(GEAR_NAME == "CALVET")

hauls.ctd <- hauls %>%
  filter(GEAR_NAME %in% c("CTD","CTDB"))

world <- ne_countries(scale = "medium",country = c("United States of America"), returnclass = "sf")

lons = c(-167, -145) #-140 = large map
lats = c(53, 61) # 52 = large map

#world <- map_data("world2")

g<-ggplot() +
  geom_sf(data = world) +
  coord_sf(xlim = lons, ylim = lats, expand = FALSE) +
  geom_point(data=hauls.ctd, mapping = aes(LON,LAT,color=YDAY),size=0.5) +
#  geom_path(data=hauls.60b, mapping = aes(LON,LAT,color=YDAY)) +
  facet_wrap(~CRUISE) +
  scale_color_viridis_c() +
  theme_bw()
# +
#  
ggsave("HistoricalSurveyMaps_20BON.png",path = here("Figures"),width=11,height=8.5)
ggsave("HistoricalSurveyMaps_CALVET.png",path = here("Figures"),width=11,height=8.5)
ggsave("HistoricalSurveyMaps_CTD.png",path = here("Figures"),width=11,height=8.5)

#Not sure how to get facet_wrap to plot across multiple pages
#
#
makemaps<-function(dat=hauls.60b, years = 1981:2021 ) { ggplot() +
  geom_sf(data = world) +
  coord_sf(xlim = lons, ylim = lats, expand = FALSE) +
  geom_point(data=dat[dat$YEAR %in% years,], mapping = aes(LON,LAT,color=YDAY),size=0.5) +
  geom_path(data=dat[dat$YEAR %in% years,], mapping = aes(LON,LAT,color=YDAY)) +
  facet_wrap(~CRUISE) +
  scale_color_viridis_c() +
  theme_bw()
}

g1<-makemaps(hauls.60b, 1981:1995)
g2<-makemaps(hauls.60b, 1996:2006)
g3<-makemaps(hauls.60b, 2007:2021)

g1
ggsave("HistoricalSurveyMaps_60BON_1981_1995.png",path = here("Figures"),width=11,height=8.5)
g2
ggsave("HistoricalSurveyMaps_60BON_1996_2006.png",path = here("Figures"),width=11,height=8.5)
g3
ggsave("HistoricalSurveyMaps_60BON_2007_2021.png",path = here("Figures"),width=11,height=8.5)




c1<-makemaps(hauls.cal, 1981:1995)
c2<-makemaps(hauls.cal, 1996:2006)
c3<-makemaps(hauls.cal, 2007:2021)

z1
ggsave("HistoricalSurveyMaps_CAL_1981_1995.png",path = here("Figures"),width=11,height=8.5)
z2
ggsave("HistoricalSurveyMaps_CAL_1996_2006.png",path = here("Figures"),width=11,height=8.5)
z3
ggsave("HistoricalSurveyMaps_CAL_2007_2021.png",path = here("Figures"),width=11,height=8.5)

