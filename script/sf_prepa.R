## Snuffelfiets data 2020 and 2021: preprocessing 

## Load packages 

library(tidyverse)
library(lubridate)
library(openair)
library(geosphere)
library(data.table)

## Load data

sf <- 
  list.files(path = "data/sf_data/",
             pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~fread(.)) # row bind data together 

sf <- sf[order(sf$recording_time),] # order the data by time

## Cleaning and change formats

options(digits.secs=3)
sf <- sf %>%
  dplyr::mutate(
    date = lubridate::ymd_hms(recording_time, tz = "Etc/GMT-1")) %>%
  dplyr::mutate(
    Humidity = as.numeric(humidity),
    PM2.5 = as.numeric(pm2_5),
    Pressure = as.numeric(pressure)) %>%
  dplyr::select(-humidity,
                -pm2_5,
                -pressure,
                -recording_time,
                -pm10, 
                -pm1_0, 
                -voc, 
                -voltage, 
                -error_code,
                -version_major,
                -version_minor,
                -acc_max,
                -no2) 

## Selection process

# Define coordinates province Utrecht: latitude and longitude
# https://en.wikipedia.org/wiki/Module:Location_map/data/Netherlands_Utrecht
# Remove all measurements with avg. speed >45 km/h (=12.5 m/s)

la0   <- 51.8776 
la1   <- 52.37224
lon0  <- 4.6939
lon1  <- 5.7463

options(scipen = 999) # disable scientific notation in R
sf_c <- sf %>%
  filter(PM2.5>0.5 & PM2.5<150) %>% # remove extreme concentrations for pm2.5 (below 0.5 and above 150?g/m3)
  filter(lat>=la0 & lat<=la1) %>% # keep measures outside Utrecht
  filter(lon>=lon0 & lon<=lon1) %>%
  arrange(sensor,date) %>%
  group_by(sensor) %>% 
  mutate(DateShift = lag(date)) %>%
  mutate(delta_time = as.numeric(date - DateShift)) %>% # time difference between two rows within the same sensor
  mutate(distance = distHaversine(cbind(lon,lat),cbind(lag(lon),lag(lat)))) %>% # new column with distance in meters 
  mutate(speed = distance/delta_time) %>% # speed in meters/seconds
  replace_na(list(delta_time = 0, distance = 0, speed = 0)) %>% # replace NA by 0 so that those rows are not deleted in the filtering later
  mutate(speedkm = speed * 3.6) %>% # convert to km/h
  filter(speedkm < 45) %>% # keep speeds below 45km /h or 12.5 m/s 
  filter(lead(speedkm) >= 5) # keep rows if following row >= 5km/h 

## Save data to txt file 

path_out = "output/data/"
write.csv(sf_c,paste0(path_out,"sf_c1.csv"), row.names = FALSE)


