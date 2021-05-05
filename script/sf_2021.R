## Snuffelfiets data 2020 and 2021

## Load packages 

library(tidyverse)
library(lubridate)
library(openair)
library(geosphere)
library(data.table)

## Load data

# 2020
# I didn't manage to download all data from 2020, only the second part of the year

setwd('C:/Users/Popi/Documents/applied data study/thesis_project/sf_datasets/snuffelfiets_data/2020')

sf_20 <- 
  list.files(pattern = "*.csv") %>% 
  map_df(~fread(.)) # row bind data together 

sf_20 <- sf_20[order(sf_20$recording_time),] # order the data by time

# 2021

setwd('C:/Users/Popi/Documents/applied data study/thesis_project/sf_datasets/snuffelfiets_data/2021')

sf_21 <- 
  list.files(pattern = "*.csv") %>% 
  map_df(~fread(.)) # row bind data together 

sf_21 <- sf_21[order(sf_21$recording_time),] # order the data by time

## Cleaning and change formats

# 2020

options(digits.secs=3)
sf_20 <- sf_20 %>%
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
# 2021

options(digits.secs=3)
sf_21 <- sf_21 %>%
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

# 2020

options(scipen = 999) # disable scientific notation in R
sf_20_u <- sf_20 %>%
  filter(PM2.5>0.5 & PM2.5<150) %>% # remove extreme concentrations for pm2.5 (below 0.5 and above 150µg/m3)
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
  filter(speedkm < 45) %>% # keep speeds below 45km /h or 12.5 m/s (removes 134 rows)
  filter(lead(speedkm) >= 5) # keep rows if following row >= 5km/h (removes 36440 rows)


# 2021

options(scipen = 999) # disable scientific notation in R
sf_21_u <- sf_21 %>%
  filter(PM2.5>0.5 & PM2.5<150) %>% # remove extreme concentrations for pm2.5 (below 0.5 and above 150µg/m3)
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
  filter(speedkm < 45) %>% # keep speeds below 45km /h or 12.5 m/s (removes 8 rows)
  filter(lead(speedkm) >= 5) # keep rows if following row >= 5km/h


## Plot

# 2020

plot20_sf <- openair::timeVariation(mydata=sf_20_u,
                       pollutant = 'Pm2.5')

# 2021 

plot21_sf <- openair::timeVariation(mydata=sf_21_u,
                       pollutant = 'Pm2.5')

openair::timeVariation(mydata=sf_21_u,
                       pollutant = 'Pm2.5',
                       statistic = "median",
                       col = "firebrick") # shows how the data are distributed 

plot21_sf <- openair::timeVariation(mydata=sf_21_u,
                                    pollutant = 'Pm2.5', 
                                    type = 'weekend')

# compare the plots 
# need to choose a subset: day.hour, hour, day, month

print(plot20_sf, split = c(1, 1, 2, 1), subset = 'day')
print(plot21_sf, split = c(2, 1, 2, 1), subset = 'day', newpage = FALSE)

# other plots 

plot20_sf1 <- openair::timePlot(sf_20_u, pollutant = 'PM2.5', y.relation = "free")
plot21_sf1 <- openair::timePlot(sf_21_u, pollutant = 'PM2.5', y.relation = "free", ylim = c(0,150))


# other plots 

trendLevel(sf_20_u, pollutant = "PM2.5", 
           border = "white", 
           cols = "jet")
