
### Example of how to do station run time, use to model our run time
library(lubridate)
#> 
#> Attaching package: 'lubridate'
#> The following objects are masked from 'package:base':
#> 
#>     date, intersect, setdiff, union
depart <- ymd_hm("2023-03-01 07:30")
set.seed(42)
legs <- sample(1000:2000,14*3,replace = TRUE)*3600
shipslog  <- depart + legs 
shipslog <- shipslog[order(shipslog)]
hour_diff <- diff(shipslog)/3600
attributes(hour_diff) <- NULL
shipslog <- data.frame(log = shipslog)
shipslog$duration <- c(NA,hour_diff)
shipslog$status <- ifelse(shipslog$duration < 20,"On station","Cruising")
shipslog[1,2] <- "Depature"
shipslog[42,2] <- "Return"
shipslog

### head(shipslog)
#                  log duration     status
#1 2023-04-12 22:30:00 Depature       <NA>
#2 2023-04-12 22:30:00        0 On station
#3 2023-04-13 23:30:00       25   Cruising
#4 2023-04-15 00:30:00       25   Cruising
#5 2023-04-17 06:30:00       54   Cruising
#6 2023-04-18 00:30:00       18 On station


### Code Above, using our data

shipslog <- SRTdata %>% select(Station, Grid.ID, Station_Time, Steam_T) %>% 
  unite("Station",Station:Grid.ID, sep = "_", remove = FALSE) %>% 
  select(Station,Steam_T,Station_Time) %>% 
  gather(key="Ops", Time_Dur, -Station) %>% arrange(Station) %>% 
  separate(Station, c("Station","Grid.ID"), sep = "_") %>% 
  transform(Station = as.numeric(Station)) %>% arrange(Station) 

shipslog <- na.omit(shipslog)


depart <- ymd_hm("2023-05-14 12:00")

ShipsLog <- shipslog %>% group_by(Station, Grid.ID) %>% 
  reframe(Time_Dur = lubridate::dseconds(sum(Time_Dur)))  %>% 
  mutate(Sta_Time =  depart) %>% mutate(cTime_Dur = cumsum(Time_Dur)) %>%
  mutate(Sta_Time = cTime_Dur + Sta_Time) %>% select(Station, Grid.ID,Sta_Time)
                                          



depart <- ymd_hm("2023-05-14 12:00")
shipslog <- depart
