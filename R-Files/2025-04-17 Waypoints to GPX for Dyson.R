### Waypoints to gpx files

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyr", "here" , "dplyr","lubridate","hms", "ggplot2", "ggmap", "lattice", 
              "stringr", "data.table","tibble","readxl","sf")
ipak(packages)

WPcsv <- read.csv(here("Data","For Dyson", "2025-03-06 DY25-06 Project Instructions Waypoints.csv")) %>% 
  select(Grid.ID,Latitude,Longitude)
WPgpx <- sf::st_as_sf(WPcsv, coords = c("Latitude","Longitude"), crs = 4326)

sf::st_write(WPgpx, "DY25-05_WP.gpx", driver = "GPX")