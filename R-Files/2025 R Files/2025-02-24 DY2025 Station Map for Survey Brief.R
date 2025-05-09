#Install Packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyr",  "dplyr","lubridate","hms", "ggplot2", "ggmap", "lattice", "stringr", "data.table",  
              "tibble","readxl", "sf","maps", "mapdata", "mapplots", "mapview", "marmap", "Cairo", "ncdf4",
              "oce", "here","reshape2", "viridis", "export", "rnaturalearth", 
              "rnaturalearthdata", "forcats","sf", "geosphere")
ipak(packages) 

# Create a base Layer map of the core sampled areas, as per JL and LR meeting on 2023-02-16
# Create Base Layer Maps will all numbered stations labeled, with a more focused region (not whole WGOA)
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
# get bathymetry data
b = getNOAA.bathy(lon1 = -166, lon2 = -140, lat1 = 52, lat2 = 62, 
                  resolution = 15)
## Querying NOAA database ...
## This may take seconds to minutes, depending on grid size
## Building bathy matrix ...
# convert bathymetry to data frame
bf = fortify.bathy(b)
# get regional polygons
reg = map_data("world2Hires")
reg = subset(reg, region %in% c('USSR', 'USA'))
# convert lat longs
reg$long = (360 - reg$long)*-1
# set map limits, whole region
#Normal Map
lons = c(-167, -149) #-140 = large map
lats = c(53, 60.25) # 52 = large map

#Focused Map
#lons = c(-153, -149) 
#lats = c(58, 60.25) 


### Station Data
#WGOAdata <- read.csv(here("Data","WGOA station_list_dougherty_2019_ProjInstrutions.csv"))
#WGOAdata$Longitude..W. <- WGOAdata$Longitude..W.*-1
# Remove stations SW of Shumagins for 2025 Start
#WGOAdata_03 <- WGOAdata[-c(8:25,183:197),] 
#WGOAdata_03 <- WGOAdata_03 %>% mutate(Station = row_number() -1)


# Create Waypoint Data for Dyson and Survey Station Selection base coordinates
#WGOA_wp <- WGOAdata_03 %>% rename (LAT = Latitude.N., LON = Longitude..W.) %>% 
#  mutate(Gear.Sampled = "") %>% 
#  mutate(Gear.Sampled = ifelse(Grid.ID %in% c("DH","Kodiak"), "Port", Gear.Sampled)) %>% 
#  mutate(Gear.Sampled = ifelse(grepl("FOX", Grid.ID), "LINE8", Gear.Sampled)) %>% 
#  mutate(Gear.Sampled = ifelse(Grid.ID %in% c("gv161","gx161","gz161",
#                                              "hb161","hd161","hf161"), "BONGO,CTD", Gear.Sampled )) %>% 
#  mutate(Gear.Sampled= ifelse(Gear.Sampled == "", "BONGO", Gear.Sampled))
  
#write.csv(WGOA_wp, file = here("Data","For Dyson","2025-03-10 DY25-06 Project Instructions Waypoints.csv"))

# All Rangling for 2025 Station Data file below done above, just loading file now
WGOAdata <- read.csv(here("Data","For Dyson","2025-03-10 DY25-06 Project Instructions Waypoints.csv"))
WGOA_wp <- WGOAdata
#### Project Instructions: Large Station Maps for Survey ####

#### Large Overall Map ####

lons = c(-165, -149) #-140 = large map
lats = c(53.5, 59.5) # 52 = large map

WGOALarge <- ggplot()+
  # add 50m contour
  geom_contour(data = bf, aes(x=x, y=y, z=z),breaks=c(-50),linewidth=c(0.5),colour="light grey")+
  # add 100m contour
  geom_contour(data = bf, aes(x=x, y=y, z=z),breaks=c(-100),linewidth=c(0.5),colour="darkgrey")+
  # add 200m contour
  geom_contour(data = bf, aes(x=x, y=y, z=z),breaks=c(-200),linewidth=c(0.2),colour="black")+
  geom_sf(data = world) + coord_sf(xlim = lons, ylim = lats, expand = FALSE)+
  #Plot WGOA points
  geom_point(data = WGOA_wp, mapping = aes(LON, LAT), #, shape = Gear.Sampled
             size = 1, show.legend = FALSE)+ 
  #Delete "shape..." for just stations
  geom_text(data = WGOA_wp, mapping = aes(LON, LAT, label = Station ), 
            nudge_y = -0.08, size = 3)+ 
  scale_shape_discrete()+
  labs(title = NULL, xlab = NULL, ylab = NULL)+
  theme_bw() + 
  theme(plot.title = element_blank(),
  axis.title = element_blank(),  
  axis.text.x=element_text(size=15, color = "black"), 
  axis.text.y = element_text(size=15, color = "black"),
  legend.position = "none") +
  annotate("text", x=-163,y=57.4912, label = "Bering Sea", size = 8)+
  annotate("text", x=-153.495,y=57.4912,size = 8, label = "Kodiak Is.")+
  annotate("text", x=-157.5,y=59.7, label = "Alaska", 
           size = 15, fontface = "bold")
 
WGOALarge
ggsave("2025-04-03 DY25-05 Station Map for Spr Larval Project Instructions.png", 
       path = here("Docs", "2025 Docs"), height = 8.5, width = 11, units = "in")



###### Smaller Section Maps
lons = c(-155.5, -149.5) #-140 = large map
lats = c(56, 59.5) # 52 = large map

WGOASmall <- ggplot()+
  # add 50m contour
  geom_contour(data = bf, aes(x=x, y=y, z=z),breaks=c(-50),linewidth=c(0.5),colour="light grey")+
  # add 100m contour
  geom_contour(data = bf, aes(x=x, y=y, z=z),breaks=c(-100),linewidth=c(0.5),colour="darkgrey")+
  # add 200m contour
  geom_contour(data = bf, aes(x=x, y=y, z=z),breaks=c(-200),linewidth=c(0.2),colour="black")+
  geom_sf(data = world) + coord_sf(xlim = lons, ylim = lats, expand = FALSE)+
  #Plot WGOA points
  geom_point(data = WGOA_wp, mapping = aes(LON, LAT), #, shape = Gear.Sampled
             size = 1, show.legend = FALSE)+ 
  #Delete "shape..." for just stations
  geom_text(data = WGOA_wp, mapping = aes(LON, LAT, label = Station ), 
            nudge_y = -0.03, size = 4)+ 
  scale_shape_discrete()+
  labs(title = "DY25-05 Late Survey", xlab = NULL, ylab = NULL)+
  theme_bw() + 
  theme(plot.title = element_text(size=13, color = "black"),
        axis.title = element_blank(),  
        axis.text.x=element_text(size=15, color = "black"), 
        axis.text.y = element_text(size=15, color = "black"),
        legend.position = "none") +
  annotate("text", x=-163,y=57.4912, label = "Bering Sea", size = 8)+
  annotate("text", x=-153.495,y=57.4912,size = 8, label = "Kodiak Is.")+
  annotate("text", x=-157.5,y=59.7, label = "Alaska", 
           size = 15, fontface = "bold")

WGOASmall
ggsave("2025-04-29 DY25-05 Station Map Late Survey.png", 
       path = here("Figures"), height = 8.5, width = 11, units = "in")



#### CTD Stations  ####
#### Large Overall Map ####

WGOA_CTD <- WGOA_wp %>% filter(Gear.Sampled == "BONGO,CTD" | Gear.Sampled == "LINE8")
#write.csv(WGOA_CTD, file = "DY23-07 Current CTD locations.csv")

lons = c(-164.5, -149) #-140 = large map
lats = c(53.5, 59.5) # 52 = large map

WGOA_CTDplot <- ggplot()+
  # add 50m contour
  geom_contour(data = bf, aes(x=x, y=y, z=z),breaks=c(-50),linewidth=c(0.5),colour="light grey")+
  # add 100m contour
  geom_contour(data = bf, aes(x=x, y=y, z=z),breaks=c(-100),linewidth=c(0.5),colour="darkgrey")+
  # add 200m contour
  geom_contour(data = bf, aes(x=x, y=y, z=z),breaks=c(-200),linewidth=c(0.2),colour="black")+
  
  geom_sf(data = world) + coord_sf(xlim = lons, ylim = lats, expand = FALSE)+
  #Plot WGOA points
  geom_point(data = WGOA_CTD, mapping = aes(LON, LAT), #, shape = Gear.Sampled
             size = 1, show.legend = FALSE)+ 
  #Delete "shape..." for just stations
  geom_text(data = WGOA_CTD, mapping = aes(LON, LAT, label = Grid.ID ), 
            nudge_y = -0.08, size = 3)+ 
  scale_shape_discrete()+
  labs(title = "DY25-05: Current CTD Stations")+
  theme_bw()+ xlab("Longitude")+ ylab("Latitude")+
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 18))+
  theme(axis.title = element_text(size = 20))+  
  theme(axis.text.x=element_text(size=15, color = "black"), axis.text.y = element_text(size=15, color = "black"))+
  theme(legend.position = "none")+
  annotate("text", x=-153.495,y=57.4912,size = 6, label = "Kodiak Is.")

WGOA_CTDplot
ggsave("DY25-05 CTD data and plot for Reference.png",path = here("Docs"), height = 8.5, width = 11, units = "in")


