#### Calculate Station Time by Gear for WGOA stations

# Survey Station Selection Scratch

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyr",  "dplyr", "ggplot2", "ggmap", "lattice", "stringr", 
              "tibble","readxl", "sf","maps", "mapdata", "mapplots", "mapview", "marmap", "Cairo", "ncdf4",
              "oce", "here","reshape2", "viridis", "export", "rnaturalearth", 
              "rnaturalearthdata", "data.table", "lubridate", "geosphere")
ipak(packages) 


#### Wrangle Mean Depth from Zooplankton Data


# GOA-IERP Master List, w/ Line 8 Added (not on orginial list)
GridZData <- read.csv(here("Data","2025-04-01 All WGOA Gridsta Z Data.csv")) %>% rename("Grid.ID" = "FOCI_GRID") 
WGOA_GridList <- read.csv(here("Data","WGOA station_list_dougherty_2019_ProjInstrutions.csv")) 
WGOA_GridList$Grid.ID <- toupper(WGOA_GridList$Grid.ID)
WGOA_GridVec <- WGOA_GridList$Grid.ID

# Filter GridZData by Known Recent WGOA Grid Stations, find mean depth per Grid Station
WGOA_GridZ <- GridZData %>% filter(Grid.ID %in% WGOA_GridVec) %>% 
  na.omit() %>% dplyr::select(Grid.ID,BOTTOM_DEPTH) %>% group_by(Grid.ID) %>% 
  summarise(Mean_Z = mean(BOTTOM_DEPTH))
#write.csv(WGOA_GridZ, file = here("Data","WGOA_GridSta_Bottom_Z.csv"))

# Add Grid Station Mean Depth to Original WGOA Grid Station location file
WGOA_Gridwp_Z <- left_join(WGOA_GridList, WGOA_GridZ, by = "Grid.ID")
#write.csv(WGOA_Gridwp_Z,file = here("Data","2025-04-01 WGOA Station List Dougherty 2019_Z ProjInstructions.csv"))

# Add Depths to 2025 Dyson Project instructions Draft WP list
WGOA_wp <- read.csv(here("Data","For Dyson","2025-03-10 DY25-06 Project Instructions Waypoints.csv"))
WGOA_wp$Grid.ID <- toupper(WGOA_wp$Grid.ID)
WGOA_wpz <- left_join(WGOA_wp,WGOA_GridZ, by = "Grid.ID")
WGOA_wpz <- WGOA_wpz[,-1]
WGOA_wpz <- WGOA_wpz %>% mutate(Mean_Z = ifelse(Grid.ID == "HJ173",60,Mean_Z))

### Calculate Estimated Tow time per operation based on Depth
## Bongo 
# Use Pythagorean theorem to calculate tow time (c = sqrt(a^2 + b^2))
# Assume ship moving 2 knots = 61.73m per minute
# Assume 42.5m down, 20m up, Use Mean Z for calculation
# 200m max, for 200m:
bon_max_T = (((sqrt(200^2 + 200^2))/42.5 + (sqrt(200^2 + 200^2))/20)+6)/60

WGOA_wpz <- WGOA_wpz %>%
  # Create BONGO_Time
  mutate(BONGO_Time = ifelse(Gear.Sampled %in% c("BONGO", "BONGO,CTD") & (Mean_Z <= 200),
                             (((sqrt(Mean_Z^2 + Mean_Z^2)) / 42.5 + (sqrt(Mean_Z^2 + Mean_Z^2)) / 20) + 6) / 60, 0)) %>%
  mutate(BONGO_Time = ifelse(Gear.Sampled %in% c("BONGO", "BONGO,CTD") & (Mean_Z > 200), bon_max_T, BONGO_Time)) %>%
  
  # Create CTD_Time
  mutate(CTD_Time = ifelse(Gear.Sampled == "BONGO,CTD", 
                           case_when(
                             Mean_Z <= 200 ~ (Mean_Z / 30) / 60,
                             Mean_Z <= 300 ~ ((200 / 30) + ((Mean_Z - 200) / 45)) / 60,
                             Mean_Z > 300 ~ NA_real_ # Protocol restriction: Do not calculate beyond 300m
                           ) + ((Mean_Z / 40) + 4) / 60, 0)) %>%
  
  # Create LINE8_Time
  mutate(LINE8_Time = ifelse(Gear.Sampled == "LINE8",
                             (
                               ((sqrt(Mean_Z^2 + Mean_Z^2) / 42.5 + (sqrt(Mean_Z^2 + Mean_Z^2)) / 20) + 6) / 60
                             ) + 
                               (
                                 case_when(
                                   Mean_Z <= 200 ~ (Mean_Z / 30) / 60,
                                   Mean_Z <= 300 ~ ((200 / 30) + ((Mean_Z - 200) / 45)) / 60
                                 ) +
                                   ((Mean_Z / 40) + 4) / 60
                               ) + .4167, 0)) %>%
  
  # Sum across columns
  mutate(Total_Gear_Time = rowSums(across(c(BONGO_Time, CTD_Time, LINE8_Time))))

#write.csv(WGOA_wpz,file = here("Data","2025 Station Data","2025-04-01 WGOA DY25-05 GearTime & WPZ.csv"))