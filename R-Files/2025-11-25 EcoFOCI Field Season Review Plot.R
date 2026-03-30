### Make Pretty Map for EcoFOCI Year End review

#Install Packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyr",  "dplyr","lubridate","hms", "ggplot2", "ggmap", "lattice",
              "stringr", "data.table","tibble","readxl", "sf","maps", "mapdata", 
              "mapplots", "mapview", "marmap", "Cairo", "ncdf4","oce", "here",
              "reshape2", "viridis", "export", "rnaturalearth", "kableExtra",
              "rnaturalearthdata", "forcats","sf", "geosphere")
ipak(packages)

# Create Base Layer Maps will all numbered stations labeled, with a more focused region (not whole WGOA)
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

## Load and Wrangle Final Station Data & Methot Trawl Locations

Final_Sta <- read.csv(here("Data","2025 Station Data", "2025-11-25 DY25-05 Final_Sta.csv")) %>% 
  dplyr::select(Station,LAT,LON, Gear.Sampled) %>% 
  mutate(Gear.Sampled = str_replace(Gear.Sampled,"BONGO,CTD","CTD")) %>% 
  mutate(Gear.Sampled = str_replace(Gear.Sampled,"LINE8", "LINE 8"))

Methot_Sta <- read.csv(here("Data","2025 Station Data","Methot_Abundance_Summary_DY2505.csv")) %>% 
  dplyr::select(Station,LAT,LON) %>% mutate(Gear.Sampled = "METHOT")

Final_Sta <- bind_rows(Final_Sta,Methot_Sta) %>% 
  dplyr::distinct()

## Create Map Plot

#Final_Sta$LON <- Final_Sta$LON*-1
# get bathymetry data

Gear.shapes <- c(16,17,15,8)
Gear.color <- c(
  "black", # orange
  "#56B4E9", # sky blue
  "#009E73", # bluish green
  "purple"  # yellow
)


b = getNOAA.bathy(lon1 = min(Final_Sta$LON)+-1, lon2 = max(Final_Sta$LON)+1, 
                  lat1 = min(Final_Sta$LAT)+-1, lat2 = max(Final_Sta$LAT)+1, 
                  resolution = 15)
# convert bathymetry to data frame
bf = fortify.bathy(b)
# set map limits, whole region
lons = c(min(Final_Sta$LON)+-1, max(Final_Sta$LON)+1) #-140 = large map
lats = c(min(Final_Sta$LAT)+-1,max(Final_Sta$LAT)+1) # 52 = large map

# 5. Plot filtered station map with survey track
StaMapF <-ggplot()+geom_contour(data = bf, aes(x=x, y=y, z=z),breaks=c(-50),linewidth=c(0.5),colour="light grey") + # add 50m contour
  geom_contour(data = bf, aes(x=x, y=y, z=z), breaks=c(-100), linewidth=c(0.5), colour="darkgrey")+ # add 100m contour
  geom_contour(data = bf, aes(x=x, y=y, z=z),breaks=c(-200),linewidth=c(0.3), colour="black")+ # add 200m contour
  geom_sf(data = world)+coord_sf(xlim = lons, ylim = lats, expand = FALSE)+ # add coastline
  #Plot points & station track
  geom_point(data = Final_Sta, mapping = aes(LON, LAT, shape = Gear.Sampled, colour = Gear.Sampled), size = 3)+ 
  scale_shape_manual(values = Gear.shapes)+ scale_color_manual(values = Gear.color) +
  theme_bw()+ 
  xlab("Longitude") + 
  ylab("Latitude")+ 
  theme(plot.title = element_blank(),
        legend.title = element_blank(),
         axis.text.x=element_text(size=rel(1.5),hjust = 1, vjust = 1, angle = 45, 
                                  face = "bold"), 
              axis.title.x = element_text(size=rel(1.5), face = "bold"),
               axis.title.y = element_text(size=rel(1.5),face = "bold"),
               axis.text.y = element_text(size=rel(1.5), face = "bold"))

StaMapF

ggsave(here("Figures","DY25-05 Stations Completed Pretty.png"), StaMapF, dpi = 300, 
       width = 12, height = 10)

