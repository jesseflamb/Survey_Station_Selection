

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyr",  "dplyr", "ggplot2", "ggmap", "lattice", "stringr", 
              "tibble","readxl", "sf","maps", "mapdata", "mapplots", "mapview", "marmap", "Cairo", "ncdf4",
              "oce", "here","reshape2", "viridis", "export", "rnaturalearth", 
              "rnaturalearthdata")
ipak(packages) 


world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

#### WGOA MASTER STATION LIST MAPS

# GOA-IERP Master List, w/ Line 8 Added (not on orginial list)
WGOAMaster <- read.csv(here("Data","EcoFOCI_GOAIERP_GEARspreadsheet.csv")) #GOA-IERP Stations
WGOAMaster <- WGOAMaster %>% arrange(y) %>% mutate(St_Ord = 1:length(GRID.STA))
WGOAMaster$LON <- WGOAMaster$LON*-1 # make LON negative (just once!!!)


# get bathymetry data
b = getNOAA.bathy(lon1 = -165.5, lon2 = -144.5, lat1 = 51.5, lat2 = 62, 
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
lons = c(-165.5, -144.5) 
lats = c(51.5, 62) 

######################################
# make plot
GOARegion_Map_New <- ggplot()+
  # add 50m contour
  geom_contour(data = bf, 
               aes(x=x, y=y, z=z),
               breaks=c(-50),
               linewidth=c(0.5),
               colour="light grey")+
  # add 100m contour
  geom_contour(data = bf, 
               aes(x=x, y=y, z=z),
               breaks=c(-100),
               linewidth=c(0.5),
               colour="darkgrey")+
  # add 200m contour
  geom_contour(data = bf, 
               aes(x=x, y=y, z=z),
               breaks=c(-200),
               linewidth=c(0.5),
               colour="black")+
  # add coastline
  geom_sf(data = world)+
  coord_sf(xlim = lons, ylim = lats, expand = FALSE)+
  
  #Plot WGOA points
  geom_point(data = WGOAMaster, mapping = aes(LON, LAT), size = 1)+
  geom_text(data = WGOAMaster, mapping = aes(LON, LAT, label = St_Ord ), 
            nudge_y = -0.05, size = 2.5)+ 
  # formatting
  scale_shape_discrete()+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(axis.text.x=element_text(size=10, color = "black"), axis.text.y = element_text(size=10, color = "black"))

GOARegion_Map_New

ggsave("GOA_IERP_AllSta_&_LINE8.png",path = here("Docs"), height = 8.5, width = 11, units = "in")



### Create .gpx in table

WGOAgpx <- WGOAMaster %>% 