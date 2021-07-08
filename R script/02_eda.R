# Snuffelfiets project (SF)
# Master ADS-Pauline Kiss

#-------------------------------------------------------------------------------

# Exploration data analysis

#-------------------------------------------------------------------------------

# Load libraries 

library(openair)
library(ggplot2)
library(timelineR)
library(dplyr)
library(Hmisc)
library(gridExtra)

# Load SF and arrange formats 

sf <- read.csv("output/data/sf_01.csv", 
               sep = ",",
               header = TRUE) 

sf <- sf %>%
  dplyr::mutate(
    humidity = as.numeric(Humidity),
    Pm2.5 = as.numeric(PM2.5),
    pressure = as.numeric(Pressure),
    Date = lubridate::ymd_hms(date, tz = "Etc/GMT-1")) %>%
  dplyr::select(-Humidity,
                -PM2.5,
                -Pressure,
                -date) %>%
  dplyr::rename(PM2.5 = Pm2.5,
                date  = Date)

#-------------------------------------------------------------------------------

# Explore the data 

str(sf) 
Hmisc::describe(sf)
dlookr::diagnose_outlier(sf)

# Frequencies 

sf$date_formatd <- format(sf$date, '%d/%m')
# Get the number of observations per day
freq.day <- sf %>%
  group_by(date_formatd) %>%
  summarise(frequency = n(),
            mean = mean(PM2.5)) %>% 
  arrange(desc(frequency, mean))
# Get the number of observations per sensor
freq.sensor <- sf %>%
  group_by(sensor) %>%
  summarise(frequency = n(),
            mean = mean(PM2.5))%>% 
  arrange(desc(frequency, mean))

#-------------------------------------------------------------------------------

# Plots

# Get the distribution of the PM2.5
sf_hist <- ggplot(data = sf) +
  geom_histogram(mapping = aes(PM2.5), binwidth = 1) +
  theme_minimal() 

sf_density <- ggplot(data = sf, aes(x = PM2.5)) +
  geom_density(fill = "lightblue") +
  xlim(0,100) +
  theme_minimal() 

grid.arrange(sf_hist,sf_density, ncol=2)

# Get time serie plot 
ggplot(data = sf) +
  geom_line(mapping = aes(x = date, y = PM2.5)) 

# Get overview of daily average concentration over the whole month!
openair::calendarPlot(sf, 
                      pollutant = 'PM2.5', 
                      main = 'PM2.5 concentration in Snuffelfiets data (µg/m3)',
                      w.shift = 2,
                      cols = "heat")

openair::timeVariation(mydata=sf,
                       pollutant = 'PM2.5') 

# Get the concentration over time
openair::timePlot(sf, pollutant = 'PM2.5', 
                  y.relation = "free",
                  main = 'PM2.5 concentration in Snuffelfiets data (µg/m3)',
                  cols = 'blue',
                  ref.y = list(h = c(100,25), lty = 2)) 




