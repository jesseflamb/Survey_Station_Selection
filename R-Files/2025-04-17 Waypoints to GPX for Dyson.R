### Waypoints to gpx files
library(sf)
library(here)
library(tidyr)

WPcsv <- read.csv(here("Data","For Dyson", "2025-03-06 DY25-06 Project Instructions Waypoints.csv")) %>% 
  dplyr::select(Grid.ID,Latitude,Longitude)
WPgpx <- sf::st_as_sf(WPcsv, coords = c( "Longitude", "Latitude"), crs = 4326)

sf::st_write(WPgpx, "DY25-05_WP.gpx", driver = "GPX", dataset_options = c("GPX_USE_EXTENSIONS=YES"))