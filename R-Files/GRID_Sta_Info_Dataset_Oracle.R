
#### Script to Query ALL up-to-date Zooplankton Data in EcoDAAT

library(RODBC)
library(tidyverse)
library(readr)
library(here)


## RUN THE CODE BELOW FIRST, enter username and password, THEN run code below###
{user <- readline("Input Username: " )
pswd <- readline("Input Password: " )}

### NOW RUN CODE BELOW #####



AFSC_Connect <- odbcConnect("AFSC", uid=user,  pwd=pswd)

sqlQuery(AFSC_Connect,"DROP TABLE SPECIMEN_MAIN_GEOM;")

sqlQuery(AFSC_Connect,"CREATE TABLE SPECIMEN_MAIN_GEOM AS SELECT * FROM ECODAAT.SPECIMEN_MAIN_GEOM;")

zoopdata <- sqlQuery(AFSC_Connect, "SELECT
BOTTOM_DEPTH ,CRUISE, DAY, DIS_PERVOLM2, DIS_PERVOLM3, EST_NUM_PERM2, EST_NUM_PERM3, FOCI_ID,FOCI_GRID, FOCI_SAMPLE_ID, GEAR_NAME, 
GEOGRAPHIC_AREA, GMT_DATE_TIME_TXT, HAUL_ID, HAUL_NAME, HAUL_PERFORMANCE, LAT, LON, MAX_GEAR_DEPTH, MESH,
MIN_GEAR_DEPTH, MONTH, NET, SAMPLE_DEPTH, SEX, SEX_NAME, SIZE_NAME, SPECIMEN_FORM, STAGE, STAGE_NAME, STATION_NAME,
TAXON_NAME, TAXON_SIZE, VOLUME_FILTERED, YEAR, ZOOP_COPEPOD_NAUPLII, ZOOP_EUPHAUSIID_EGG
FROM SPECIMEN_MAIN_GEOM WHERE ORIG_DB LIKE 'BOB';", stringsAsFactors=FALSE)

write.csv(zoopdata, here("Data", "2023_10_12-All_Zooplankton.csv"))

#Close database connection

odbcClose(AFSC_Connect)
