# Snuffelfiets project (SF)
# Master ADS-Pauline Kiss

#-------------------------------------------------------------------------------

# SF data preparation 

#-------------------------------------------------------------------------------

# Load libraries 

library(tidyverse)
library(lubridate)
library(openair)
library(geosphere)
library(data.table)

# Load paths

path_in = "data/sf_data/raw/"
path_out = "output/data/"

# Load SF data: 5 weeks in January 2020

sf <- 
  list.files(path = path_in,
             pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~fread(.))

sf <- sf[order(sf$recording_time),]

# Save as csv 

write.csv(sf,paste0(path_out,"sf_JAN.csv"), row.names = FALSE)

# Note

# In order to select only the measurements in the province Utrecht, I exported 
# sf_JAN.csv to GIS as well as a map of the province: 
# B1_Provinciegrenzen_van_NederlandPolygon.shp found at the website:
# https://www.geoportaaloverijssel.nl/. After selecting the province
# Utrecht, I did the intersection of the two maps with the tool 'Join attributes 
# by location' and I saved it as utrecht_sf.csv. 


# Import output of GIS

gis <- read.csv("output/data/utrecht_sf.csv", 
                sep = ",",
                header = TRUE) 

gis <- gis[order(gis$recording_),]

# Clean the data: modify formats and names

options(digits.secs=3)

gis <- gis %>%
  dplyr::mutate(
    date = lubridate::ymd_hms(recording_, tz = "Etc/GMT-1")) %>%
  dplyr::mutate(
    Humidity = as.numeric(humidity),
    PM2.5 = as.numeric(pm2_5),
    Pressure = as.numeric(pressure)) %>%
  dplyr::select(-humidity,
                -pm2_5,
                -pressure,
                -recording_,
                -pm10, 
                -pm1_0, 
                -voc, 
                -voltage, 
                -error_code,
                -version_ma,
                -version_mi,
                -acc_max,
                -no2,
                -CBS_CODE,
                -PROV_NAAM,
                -OBJECTID) %>%
  dplyr::rename(air_quality_observed_id = air_qualit, 
                temperature = temperatur,
                trip_sequence = trip_seque)

# Apply selection criteria (suggested by the RIVM)

# disable scientific notation in R
options(scipen = 999) 
sf_c <- gis %>%
  # take only January 2020
  filter(between(date, ymd("2020-01-01"), ymd("2020-02-01"))) %>% 
  # replace null concentrations with 1
  mutate(PM2.5 = replace(PM2.5, PM2.5==0, 1)) %>% 
  # remove extreme concentrations for PM2.5
  filter(PM2.5<150) %>% 
  arrange(sensor, date) %>%
  group_by(sensor) %>% 
  mutate(DateShift = lag(date)) %>%
  # new column time difference between two rows within the same sensor
  mutate(delta_time = as.numeric(date - DateShift)) %>% 
  # new column with distance in meters 
  mutate(distance = distHaversine(cbind(lon, lat),cbind(lag(lon), lag(lat)))) %>%
  # new column with speed in meters/seconds
  mutate(speed = distance / delta_time) %>%
  # replace NA by 0 so that those rows are not deleted in the filter() later
  replace_na(list(delta_time = 0, distance = 0, speed = 0)) %>% 
  # convert speed to km/h
  mutate(speedkm = speed * 3.6) %>% 
  # keep speeds below 45 km/h and above 7.5km/h
  filter(speedkm < 45 & speedkm >= 7.5) %>% 
  # remove columns
  dplyr::select(-DateShift,
                -delta_time,
                -distance,
                -speed,
                -speedkm) %>%
  ungroup()

# Save cleaned data as csv file 

write.csv(sf_c,paste0(path_out,"sf_01.csv"), row.names = FALSE)


