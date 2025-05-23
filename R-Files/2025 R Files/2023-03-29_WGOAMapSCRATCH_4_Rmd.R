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


##### REwork Outlook and Day to Day code to fit Operations & station depth

### Create Station & Runtime Dataset ############

WGOAdata <- WGOA_wpz

###All Stations: based on "Station Column" of "WGOA station_list_dougherty_2019_ProjInstrutions.csv", currently
# in "Data", with mean Grid station depth added
AllSta<- c(0:238) 

#Stations Left to Sample

# Completed Stations
CompleteScenL <- length(c()) #Fill in with completed StaTDL stations

# Staions Remaining 
StaTDL<-c(0:238)

### Cutting stations for time
# NE_Kodiak <- c(161:180)

#B_Drop_Vec <- c(NE_Kodiak)
#Drop Stations by subset using updated above vector, keep drops by date
#WGOAdata <- WGOAdata %>% subset(!(Station %in% STA.0516))

# Save CSV of above for Day to Day
#write.csv(WGOAdata, here("Data","DY23-07_Day2Day.csv"))

# Reverse Order Stations for Reference (DO NOT DELETE)
# 
#            c(1:9,24:28,59:53,84:77, 92:85, 100:93, 107:101,113:108,119:114, 
#           125:120, 131:126, 136:132,141:137, 142:142,152:148, 156:153, 159:157,165:160,
#           166:179,180,213:208,216:220,228:225,234:238,271)


ScenName <- "Stations Completed"
Scenario<-StaTDL # Always Change to Updated Stations Remaining
FinalSta<-data.frame("Station"=Scenario,"Stn_Order"= (CompleteScenL+1):(CompleteScenL+length(Scenario)))

# 1. Station Selection Through Scenario (above) 

Sta <- WGOAdata %>% left_join(.,FinalSta) %>%
  filter(Station %in% FinalSta$Station) %>%  
  arrange(Stn_Order)

# 2. Choose Departure Date
DH_SIP <- ymd_hm("2025-05-17 12:00") 
depart <- DH_SIP + dhours(9) # pre-survey calculations, takes ~8 hours to get to STA 1 from DH

# Use below during survey
depart <- lubridate::now(tzone = "America/Anchorage") + dhours(16.5) # Alaska time, for at sea calculations

# 3. Create data.frame that mirrors "Final Station Runtime.xlsx"=

SRTdata <- Sta %>% 
 mutate(Station_Time = lubridate::dhours(Total_Gear_Time)) %>% 
  mutate(Ship_Speed = 11) %>%  # Ship speed is Variable
  mutate(lat_rad = LAT*pi/180) %>% mutate(long_rad = LON*pi/180) %>% 
  mutate(long_dif = long_rad - shift(long_rad)) %>% 
  #>>>>>>> Stashed changes
  mutate(Dist_NM = ifelse(Station == 0,0,0) ) %>% 
  mutate(Dist_NM = geosphere::distGeo(cbind(LON,LAT),cbind(shift(LON),shift(LAT)))/(1000*1.852)) %>% 
  # distGEO does what code below does in base r
  #  mutate(Dist_NM = acos((sin(lag(lat_rad))*sin(lat_rad))+(cos(lag(lat_rad))*cos(lat_rad)*cos(long_dif)))/(pi/180)*60) %>%  
  mutate(Steam_T = Dist_NM/Ship_Speed) %>% 
  mutate(Steam_T = lubridate::dhours(Steam_T))

#Write .csv of new Station Order for Oscar Dyson crew 
ODsta <- SRTdata %>% select(Grid.ID,LAT,LON,Gear.Sampled,Stn_Order) 
#write.csv(ODsta, here("Data","For Dyson","DY23-07 Survey Station Order.csv"), row.names = FALSE) #Error if file open

# 4. rearrange data to calculate time to travel to and complete station operations
shipslog <- SRTdata %>% select(Station, Grid.ID, Station_Time, Steam_T, Stn_Order) %>% # SRTdata will be replaced by filtered stations above
  unite("Station",Station:Grid.ID, sep = "_", remove = FALSE) %>% 
  select(Station,Steam_T,Station_Time,Stn_Order) %>% 
  gather(key="Ops", Time_Dur, -Station,-Stn_Order) %>% arrange(Station) %>% 
  separate(Station, c("Station","Grid.ID"), sep = "_") %>% 
  transform(Station = as.numeric(Station)) %>% arrange(Stn_Order) 
shipslog <- na.omit(shipslog)

# 5. Create a table of Station Completion Times 
ShipsLog <- shipslog %>% group_by(Stn_Order, Station, Grid.ID) %>% 
  reframe(Time_Dur = lubridate::dseconds(sum(Time_Dur)))  %>% 
  mutate(Sta_Time =  depart) %>% mutate(cTime_Dur = cumsum(Time_Dur)) %>%
  mutate(Sta_Time = cTime_Dur + Sta_Time) %>% select(Stn_Order, Grid.ID,Sta_Time) %>% 
  rename("Station"="Stn_Order","Grid Station"="Grid.ID","Est.Station Time"="Sta_Time")

#6. Create a table to link Gear.Sampled for daily stations in ShipsLog to know Operations at each station
Sta_GearSamp <- SRTdata %>% select(Station,Grid.ID,LAT,LON,Gear.Sampled) %>% 
  filter(Station %in% FinalSta$Station) %>% select(Grid.ID,LAT,LON,Gear.Sampled) %>% 
  rename("Grid Station"="Grid.ID")

ShipsLog <- left_join(ShipsLog,Sta_GearSamp, by = "Grid Station") %>% 
  select(Station,`Grid Station`,LAT,LON,Gear.Sampled,`Est.Station Time`)

# Output Station Table, include: Ships Log
kbl(ShipsLog, caption = "Next Stations", booktabs = T) %>% 
  kableExtra::kable_styling(bootstrap_options = "bordered", latex_options = c("striped", "HOLD_position"))


