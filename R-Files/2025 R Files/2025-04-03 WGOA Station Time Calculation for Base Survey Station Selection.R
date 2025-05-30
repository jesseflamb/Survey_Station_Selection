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

# Filter GridZData by Known Recent WGOA Grid Stations, find mean depth per Grid Station (10 m off bottom)
WGOA_GridZ <- GridZData %>% filter(Grid.ID %in% WGOA_GridVec) %>% 
  na.omit() %>% dplyr::select(Grid.ID,BOTTOM_DEPTH) %>% group_by(Grid.ID) %>% 
  summarise(Mean_Z = (mean(BOTTOM_DEPTH)) - 10)
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
# Calculate vertical decent rate based on 45 degree towing angle (calculate angle radians)
# Then calculate vertical speed, based on wire speeds at 45 degree angle

# Calculate Angle Radians
angle_degrees = 42.5
AngRad <- angle_degrees*(pi / 180)

# Define Vertical Descent & Assent Rates 
BON_Dn_WS <- 40
BON_Up_WS <- 20
CTD_lt200_WS <- 30
CTD_gt200_WS <- 30

# Calc Vertical Speed by angle and Wire speed
BON_Dn_VS <- BON_Dn_WS * sin(AngRad)
BON_Up_VS <- BON_Up_WS * sin(AngRad)

bon_max_T = ((300/BON_Dn_VS) + (300/BON_Up_VS)+ 20) / 60

WGOA_wpz <- WGOA_wpz %>% 
  # Create BONGO_Time
  mutate(BONGO_Time = ifelse(Gear.Sampled %in% c("BONGO", "BONGO,CTD") & (Mean_Z <= 300),
                             (((Mean_Z / BON_Dn_VS) + (Mean_Z / BON_Up_VS)) + 20)/60, 
                             0)) %>% 
  mutate(BONGO_Time = ifelse(Gear.Sampled %in% c("BONGO", "BONGO,CTD") & (Mean_Z > 300), 
                             bon_max_T, BONGO_Time)) %>%
  
  # Create CTD_Time
  
  mutate(CTD_Time = ifelse(Gear.Sampled == "BONGO,CTD", 
                           case_when(
                             Mean_Z <= 200 ~ ((Mean_Z / CTD_lt200_WS)+9) / 60,
                             Mean_Z <= 300 ~ (((200 / CTD_lt200_WS) + ((Mean_Z - 200) / CTD_gt200_WS))+12)/ 60,
                             Mean_Z > 300 ~ NA_real_ # Protocol restriction: Do not calculate beyond 300m
                           ) + ( 10 / 60), 0)) %>%
  
  # Create LINE8_Time, the "0.4167" at end is adding 25 minutes to each station for deck work
  mutate(LINE8_Time = ifelse(Gear.Sampled == "LINE8",
                             (((Mean_Z / BON_Dn_VS) + (Mean_Z / BON_Up_VS) + 10) / 60) + 
                               case_when(
                                 Mean_Z <= 200 ~ (Mean_Z / CTD_lt200_WS) / 60,
                                 Mean_Z <= 300 ~ ((200 / CTD_lt200_WS) + ((Mean_Z - 200) / CTD_gt200_WS)) / 60 + (10 / 60) + 0.4167
                               ), 
                             0)) %>%
  
  # Sum across columns
  mutate(Total_Gear_Time = rowSums(across(c(BONGO_Time, CTD_Time, LINE8_Time)))) %>% 
  mutate(Total_Gear_Minutes = Total_Gear_Time*60)

write.csv(WGOA_wpz,file = here("Data","2025 Station Data","2025-04-01 WGOA DY25-05 GearTime & WPZ.csv"))

# Create Reference List of Sta number, GridID, LAT, LON for print
WGOA_StaRef <- WGOA_wpz %>% select(Station, Grid.ID,LAT,LON,Gear.Sampled,Mean_Z)
write.csv(WGOA_StaRef,file = here("Data","2025 Station Data","2025-05-13 WGOA DY25-05 Station Reference for Print.csv"))
