# Snuffelfiets project (SF)
# Master ADS-Pauline Kiss
# Code SESAM from Alicia Gressent (https://github.com/AliciaGressent/SESAM)

#-------------------------------------------------------------------------------

# Hourly comparison plot

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

path_in <- "output/data/rio_7/"
path_out <- 'output/data/'
dirout <- 'output/plots/'

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
  # Select hours
  dplyr::mutate(hour = format(date, format = '%H')) %>%
  arrange(hour) %>%
  filter(hour >= '06' & hour <= '19')

hour_num <- as.numeric(sf_7$hour)
sf_7$hour_num <- hour_num

#-------------------------------------------------------------------------------

# Start loop over all hours

# Initialize 
pol <- 'PM2.5'
CRS_RDNEW <- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs" #spatialreference.org
CRS_WGS84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" 
estim_date   <- "2020-01-07"
estim_date2  <- "20200107" 
unit="h" # unit of the comparison (time resolution h=hour)
estim_YYYY   <- substr(estim_date,1,4)
estim_MM     <- substr(estim_date,6,7)
estim_DD     <- substr(estim_date,9,10)
# Select times 
estim_HH_start_list <- c("06","07","08","09","10","11","12","13","14",
                         "15","16","17","18","19")
estim_HH_end_list   <- c("07","08","09","10","11","12","13","14","15",
                         "16","17","18","19","20")
# Create empty matrixes 
all_stat_pol_model <- matrix(,nrow = length(estim_HH_start_list), ncol = 3)
all_stat_pol_data  <- matrix(,nrow = length(estim_HH_start_list), ncol = 3)
# Create objects 
all_obs_modelf <- c()
nbr_points_all <- c()

for (time in 1:length(estim_HH_start_list)){ 
  estim_HH_start = estim_HH_start_list[time]
  estim_HH_end = estim_HH_end_list[time]
  estim_period = paste0(estim_MM,estim_YYYY, "_", estim_HH_start, "h", 
                        estim_HH_end, "h")
  print(paste0("TIME is ", estim_HH_start, ":00:00"))
  num_HH_start = as.numeric(estim_HH_start)
  num_HH_end = as.numeric(estim_HH_end)
  if (num_HH_end <= 9){
    hhour = paste0("0", num_HH_end)
  }else{
      hhour = toString(num_HH_end)
      }
  
  # Load RIO estimates 
  
  file <- paste0("20200107", num_HH_start, "_pm25.txt")
  data <- read.table(paste(path_in, file, sep = ''), header = T, sep = '\t',
                     stringsAsFactors = F, dec = '.')

  spmodel <- data
  names(spmodel)[names(spmodel) == "X"] <- "lon"
  names(spmodel)[names(spmodel) == "Y"] <- "lat"
  # Create two new numerical variables
  spmodel$Long <- spmodel$lon
  spmodel$Lat  <- spmodel$lat
  spmodel$Long <- as.numeric(spmodel$Long)
  spmodel$Lat  <- as.numeric(spmodel$Lat)
  # Convert to spatial object
  coordinates(spmodel) = ~Long + Lat
  proj4string(spmodel) = CRS(CRS_RDNEW)
  # Convert to WGS84
  spmodel <- spTransform(spmodel, CRS(CRS_WGS84)) 
  # Clean and redefine spatial data object
  spmodel <- subset(spmodel, select = -c(lat,lon))
  tmp = spmodel@coords; dlon = tmp[,1]; dlat = tmp[,2]
  spmodel$lon <- dlon
  spmodel$lat <- dlat
  class(spmodel); summary(spmodel)
  names(spmodel)[names(spmodel) == "CONC"] <- "pol"
  # Create dataframe
  data_model <- as.data.frame(spmodel)
  names(data_model)[names(data_model) == "Long"] <- "lon"
  names(data_model)[names(data_model) == "Lat"] <- "lat"
  data_model <- data_model[,c("lat","lon","pol")] 
  
  
  # Convert sensor data
  sf_7_h <- subset(sf_7, hour_num >= num_HH_start & hour_num < num_HH_end)
  sf_7_agg <- aggregate(PM2.5 ~lat+lon,data=sf_7_h,mean,na.action=na.pass)
  
  spdata_ms <- sf_7_agg 
  spdata_ms$Long <- spdata_ms$lon
  spdata_ms$Lat <- spdata_ms$lat
  # Convert to spatial object
  coordinates(spdata_ms) = ~Long+Lat
  proj4string(spdata_ms) = CRS(CRS_WGS84)
  # Convert to WGS84
  spdata_ms <- spTransform(spdata_ms, CRS(CRS_WGS84)) 
  tmp = spdata_ms@coords; dlon = tmp[,1]; dlat = tmp[,2]
  spdata_ms$lon <- dlon
  spdata_ms$lat <- dlat
  class(spdata_ms); summary(spdata_ms)
  names(spdata_ms)[names(spdata_ms) == "PM2.5"] <- "pol"
  data_ms <- as.data.frame(spdata_ms)
  data_ms <- subset(data_ms, select = -c(Long,Lat))
  
  # Look for the closest model grid point to the SF data 
  model_tmp <- rep(0, length(data_ms[,1]))
  for (ll in 1:length(spdata_ms)){
    spdata_tmp <- spdata_ms[ll,]
    # Calculate distance between the data point and the model grid points
    dist_vector <- spDistsN1(spmodel,spdata_tmp) 
    # Convert km to m
    dist_vector <- dist_vector * 1e3 
    dist_min <- min(dist_vector)
    dist_min <- min(dist_vector)
    # Find point with the shortest distance
    idx <- which.min(dist_vector) 
    tmp <- spmodel[idx,]
    tmp2 <-as.data.frame(tmp)
    pol_tmp2 <- tmp2$pol
    model_tmp[ll] <- pol_tmp2
  }
  
  # Get correlation coefficients hour per hour
  corr <- round(cor(spdata_ms$pol,model_tmp),2)
  print(paste0("Correlation is ", corr))
  
  
  # Combine data for comparison
  obs_model <- cbind(data_ms$lon, data_ms$lat, data_ms$pol, model_tmp)
  model_tmp <- subset(model_tmp, (!is.na(obs_model[,4])))
  data_ms   <- subset(data_ms, (!is.na(obs_model[,4])))
  obs_model <- subset(obs_model, (!is.na(obs_model[,4])))
  
  LAT <- as.vector(t(rbind(data_ms$lat, data_ms$lat)))
  LON <- as.vector(t(rbind(data_ms$lon, data_ms$lon)))
  obs_model2 <- as.vector(t(rbind(data_ms$pol, model_tmp)))
  VAR <- as.vector(t(rbind(rep("Snuffelfiets data", length(data_ms[,1])), 
                           rep("Model", length(data_ms[,1])))))
  num_pts        <- seq(from = 1, to = length(data_ms[,1]))
  PTS            <- as.vector(t(rbind(num_pts, num_pts)))
  obs_modelf     <- as.data.frame(cbind(PTS, LAT, LON, obs_model2))
  obs_modelf$VAR <- as.factor(VAR)
  all_obs_modelf <- rbind(all_obs_modelf, obs_modelf)
  
  # Calculate average of all points and 95% confidence interval
  n <- length(data_ms[,1])
  moy_pol_model   <- mean(model_tmp)
  moy_pol_data    <- mean(data_ms$pol)
  sd_pol_model    <- sd(model_tmp)
  sd_pol_data     <- sd(data_ms$pol)
  error_model     <- qt(0.975, df = n-1) * sd_pol_model / sqrt(n)
  error_data      <- qt(0.975, df = n-1) * sd_pol_model / sqrt(n)
  upper_pol_model <- moy_pol_model + error_model
  upper_pol_data  <- moy_pol_data + error_data
  lower_pol_model <- moy_pol_model - error_model
  lower_pol_data  <- moy_pol_data - error_data
  
  # Put calculated values in the tables
  all_stat_pol_model[time,1] <- moy_pol_model
  all_stat_pol_data[time,1]  <- moy_pol_data
  all_stat_pol_model[time,2] <- upper_pol_model
  all_stat_pol_data[time,2]  <- upper_pol_data
  all_stat_pol_model[time,3] <- lower_pol_model
  all_stat_pol_data[time,3]  <- lower_pol_data
  
} # end of hourly loop

#-------------------------------------------------------------------------------

# Create dataframe 
all_stat_pol <- as.data.frame(rbind(all_stat_pol_model, all_stat_pol_data))

names(all_stat_pol)[names(all_stat_pol) == "V1"] <- "Mean"
names(all_stat_pol)[names(all_stat_pol) == "V2"] <- "Upper"
names(all_stat_pol)[names(all_stat_pol) == "V3"] <- "Lower"

VAR1 <- rep("Model", length(all_stat_pol_model[,1]))
VAR2 <- rep("Snuffelfiets data", length(all_stat_pol_model[,1]))
VAR <- as.vector(t(rbind(VAR1,VAR2)))
all_stat_pol$VAR <- as.factor(VAR)
ntime <- as.numeric(estim_HH_start_list)
TIME = as.vector(t(rbind(ntime,ntime)))
all_stat_pol$TIME <- TIME

#-------------------------------------------------------------------------------

# Plot

# Create colours 
cbPalette1 <- c("springgreen2","firebrick1")
cbPalette2 <- c("springgreen4","firebrick4")

png(paste0(dirout,"hoursplot.png"), width = 1000, height = 500)
p1 <- ggplot(all_stat_pol,aes(x = TIME, y = Mean))+
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = VAR), alpha = 0.2) + 
  geom_line(aes(color = VAR), size = 0.6)+
  scale_x_continuous(breaks = pretty(all_stat_pol$TIME, n = 12)) +
  scale_fill_manual(values = cbPalette1)+
  scale_colour_manual(values = cbPalette2)+
  xlab("Hour") + ylab(bquote(.(pol) ~ (mu*g/m^3)))+
  theme_bw()+
  theme_minimal()+
  theme(plot.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title = element_blank(),
        legend.spacing.x = unit(0.3, 'cm'),
        legend.position = "top")
p1
dev.off()
