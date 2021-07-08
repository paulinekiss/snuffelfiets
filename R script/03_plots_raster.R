# Snuffelfiets project (SF)
# Master ADS-Pauline Kiss
# Code SESAM from Alicia Gressent (https://github.com/AliciaGressent/SESAM)

#-------------------------------------------------------------------------------

# Raster plots: 07/01

#-------------------------------------------------------------------------------

# Load libraries

library(dplyr)
library(tidyr)
library(data.table)
library(purrr)
library(raster)
library(sf)
library(Matrix)
library(ggplot2)


# Load paths

path_out <- 'output/data/'
dirout <- 'output/plots/'

#-------------------------------------------------------------------------------

# Load RIO data for 07/01  (Hourly estimates over 2400 positions)

files <- list.files("data/uu_rio/UU/jan/", pattern = "20200107", full.names = TRUE)
# Select hours
files <- files[7:20] 

rio_7 <- files %>% 
  # Combine separate files into one dataframe
  map(read.csv, stringsAsFactors=FALSE) %>% 
  bind_rows() %>%
  # Split columns
  separate(col = X.Y.CONC, into = c("X", "Y", "PM2.5"), sep = "\t") %>%
  # Add columns to identify uniquely each timeslot
  dplyr::mutate(nr = rep(1:2400, 24),
                hour = rep(1:24, each = 2400))


# Aggregate data per position on 07/01

rio_meanj_7 <- rio_7 %>%
  mutate(PM2.5_ = as.numeric(PM2.5)) %>%
  dplyr::select(-PM2.5) %>%
  rename(PM2.5 = PM2.5_) %>%
  group_by(X,Y) %>%
  summarise(mean_2.5 = mean(PM2.5)) %>%
  ungroup()

# Rasterize daily averaged RIO estimates

ras <- rasterFromXYZ(rio_meanj_7, 
                     res = c(1000,1000), 
                     crs = st_crs(28992), 
                     digits = 5)

# Plot

# Create colours 
if (pol == 'PM2.5'){zl <- c(5,15)}
ras[ which(ras@data@values < zl[1]) ] <- zl[1]
ras[ which(ras@data@values > zl[2]) ] <- zl[2]

d = rbind(c(38,48,132),c(61,99,174),c(114,201,195),c(220,225,30),c(240,78,34),c(133,22,24))
palette = colorRampPalette(rgb(d[,1], d[,2], d[,3], maxColorValue = 255))(128)
mycol = palette

png(paste0(dirout, "Raster_rio_7.png"), width = 900, height = 900)
par(mar = c(4,4,3,4)) 
plot(ras, col = mycol, interpolate = FALSE, maxpixels = 5000000,
     cex.lab = 1.6,
     cex.axis = 1.8,
     legend.width = 1.5, legend.shrink = 0.75,
     legend.args = list(text = "   (µg/m3)", cex = 1.8, line = 1, font = 1))
dev.off()

#-------------------------------------------------------------------------------

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

# Select SF data for 07/01

sf$day <- format(sf$date, format = "%y-%m-%d")

sf_7 <- sf %>%
  filter(day == ("20-01-07")) %>%
  dplyr::select(-air_quality_observed_id,
                -trip_sequence,
                -temperature,
                -humidity,
                -pressure,
                -day) %>%
  dplyr::mutate(hour = format(date, format = '%H')) %>%
  arrange(hour) 

# Prepare for rasterization

sf_7 <- sf_7 %>%
  # convert to sf object
  st_as_sf(coords = c('lon','lat'),crs = 4326, remove=FALSE) %>%
  # convert lon lat to geometry column with new crs
  st_transform(crs = 28992) 

# Rasterize daily averaged SF data

sf_mean7 <- rasterize(sf_7, ras, field = 'PM2.5', mean)

# Plot

# Create colours 
if (pol == 'PM2.5'){zl <- c(0,50)}
sf_mean7[which(sf_mean7@data@values < zl[1]) ] <- zl[1]
sf_mean7[which(sf_mean7@data@values > zl[2]) ] <- zl[2]

d = rbind(c(38,48,132), c(61,99,174), c(114,201,195), c(220,225,30), 
        c(240,78,34), c(133,22,24))
palette = colorRampPalette(rgb(d[,1], d[,2], d[,3], maxColorValue = 255))(128)
mycol = palette

png(paste0(dirout, "Raster_sf_7.png"), width = 900, height = 900)
par(mar = c(4,4,3,4)) 
plot(sf_mean7, col = mycol, interpolate=FALSE, maxpixels=5000000,
     cex.lab = 1.4,
     cex.axis = 1.6,
     legend.width = 1.4, legend.shrink = 0.75,
     legend.args = list(text = "   (µg/m3)", cex = 1.4, line = 1, font = 1))
dev.off()