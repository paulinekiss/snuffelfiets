# Snuffelfiets project (SF)
# Master ADS-Pauline Kiss
# Code SESAM from Alicia Gressent (https://github.com/AliciaGressent/SESAM)

#-------------------------------------------------------------------------------

# Data fusion 

#-------------------------------------------------------------------------------

# Load libraries

library(RGeostats) 
library(maptools);library(RColorBrewer)
library(fields);library(raster)
library(rgdal);library(gstat)
library(XML);library(akima)
library(rgeos);library(sp)
library(spacetime);library(chron)
library(plotrix); library(Rcpp)
library(RcppArmadillo); library(MASS)
library(foreach)
library(doParallel)
library(dplyr)
library(purrr)
library(tidyr)

# Function----------------------------------------------------------------------

# Function to calculate the Variance of Measurement Errors (VME) from sensor
# data
vme2df <- function(obs_data, U) {
  vme_pol_all <- c()
  # Iteration through all raster grid cells 
  for (l in 1:length(unique(obs_data$id_cell))) {
    cell <- unique(obs_data$id_cell)[l]
    pol_conc <- obs_data[which(obs_data$id_cell == cell), 3] 
    N <- length(pol_conc)
    # VME calculated for grid cells with more than one obs 
    if (length(pol_conc) > 1) {
      var1 = (sd(pol_conc) / sqrt(N))**2 
      var2 = (U ** 2 / N) * sum(sapply(pol_conc, `[`) ** 2)
      vme_pol_sens = var1 + var2
    } else {
      vme_pol_sens = -9999 
    } 
    vme_pol_all[l] <- vme_pol_sens
  }
  vme_pol_sens <- vme_pol_all
  return(vme_pol_sens)
}

# Initialization----------------------------------------------------------------

# Paths
path_in  <- 'output/data/rio_7'
pathout <- 'output/data/'
dirout  <- 'output/plots/file_plot/'


# Variable names
city <- 'Utrecht'
pol <- 'PM2.5'
CRS_RDNEW <- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 
+k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m 
+no_defs" # Reference: spatialreference.org
CRS_WGS84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" 
nbr_pts_all <- c()   # Number of data points for kriging
MS <- "MS"

# Time 
estim_date <- "2020-01-07"
estim_date2 <- "20200107" 
estim_YYYY <- substr(estim_date, 1, 4)
estim_MM <- substr(estim_date, 6, 7)
estim_DD <- substr(estim_date, 9, 10)
estim_HH_start_list <- c("06","07","08","09","10","11","12","13","14","15","16",
                         "17","18","19")
estim_HH_end_list   <- c("07","08","09","10","11","12","13","14","15","16","17",
                         "18","19","20")

# Grid parameters
xmin  <- 120500
ymin  <- 435500
res   <- 1000 # Grid Resolution in meters
ncols <- 60 
nrows <- 40 
U_ms  <- 0.25 # Uncertainty mobile sensor

# Color palette for mapping
d <- rbind(c(38,48,132), c(61,99,174), c(114,201,195), c(220,225,30), 
           c(240,78,34), c(133,22,24))
palette <- colorRampPalette(rgb(d[,1], d[,2], d[,3], maxColorValue = 255))(128)
mycol <- palette


# Grid--------------------------------------------------------------------------

# Create grid (grid_tmp)
X <- c() 
for (i in 1:ncols) { 
  if (i == 1) {
    X[i] = xmin
  }else{
    X[i] = X[i - 1] + res}
}
Y <- c() 
for (i in 1:nrows) {
  if (i == 1) {
    Y[i] = ymin
  }else{ 
    Y[i] = Y[i - 1] + res}
}
grid1 = expand.grid(X, Y) 
# Create pseudo data
data1 <- rep(0, length(grid1))
grid1$data1 <- data1
# Convert grid1 to spatial object
coordinates(grid1) = ~Var1 + Var2 
proj4string(grid1) = CRS_RDNEW 
grid_coords = grid1@coords 
# Convert to dataframe
grid_l <- data.frame(lon = seq(1:length(grid_coords[, 1])),
                     lat = seq(1:length(grid_coords[, 1])),
                     Drift = seq(1:length(grid_coords[, 1])))
grid_l$lon   <- grid_coords[,1]
grid_l$lat   <- grid_coords[,2]
grid_l$Drift <- grid_l$Drift * 1 
grid_l$Long  <- grid_l$lon; grid_l$Lat <- grid_l$lat
# Convert grid_tmp to spatial object
coordinates(grid_l) <- ~Long + Lat
proj4string(grid_l) <- CRS_RDNEW


# Create grid with higher resolution that will be used for the VME assignment
ext <- raster::extent(120000, 180000, 435000, 475000) 
grid_s <- st_bbox(ext) %>%
  st_make_grid(cellsize = 100, what = 'polygons') %>%
  st_set_crs(28992)
# Add cell identifier 
grid_s <- grid_s %>%
  st_sf() %>% 
  mutate(id_cell = seq_len(nrow(.)))


# Sensor data-------------------------------------------------------------------

# Load sensor data 
sf <- read.csv("output/data/sf_01.csv", 
               sep = ",",
               header = TRUE) 

sf <- sf %>%
  dplyr::mutate(Pm2.5 = as.numeric(PM2.5),
                Date = lubridate::ymd_hms(date, tz = "Etc/GMT-1")) %>%
  dplyr::select(-Humidity,
                -PM2.5,
                -Pressure,
                -date,
                -air_quality_observed_id,
                -trip_sequence,
                -temperature) %>%
  dplyr::rename(PM2.5 = Pm2.5,
                date  = Date) %>%
  arrange(date) 

data_preproc <- sf
names(data_preproc)[names(data_preproc) == "PM2.5"] <- "pol"

# Loop over time (hourly estimates)---------------------------------------------

for (time in 1:length(estim_HH_start_list)) {
  
  
  ## Time parameters ##
  
  estim_HH_start <- estim_HH_start_list[time]
  estim_HH_end   <- estim_HH_end_list[time]
  estim_period   <- paste0(estim_DD, estim_MM,estim_YYYY, "_", estim_HH_start, 
                           "h", estim_HH_end, "h")
  print(paste0("TIME is ", estim_date, " ", estim_HH_start, ":00:00"))
  num_HH_start <- as.numeric(estim_HH_start)
  num_HH_end   <- as.numeric(estim_HH_end)
  if (num_HH_end <= 9) { 
    hhour = paste0("0",num_HH_end) 
  } else { 
    hhour = toString(num_HH_end)
  }
  date_start <- as.POSIXct(paste0(estim_date, " ", estim_HH_start, ":00:00"), 
                           origin = "1970-01-01", tz = "Europe/Amsterdam")
  date_end   <- as.POSIXct(paste0(estim_date, " ", estim_HH_end, ":00:00"), 
                           origin = "1970-01-01", tz = "Europe/Amsterdam")
  
  
  
  ## Select RIO data ##
  
  # Get RIO data for each hour (n=14)
  file <- paste0("20200107", num_HH_start, "_pm25.txt") 
  drift_h <- read.table(paste(indir2, file, sep = '/'),
                        header = T, sep = '\t', 
                        stringsAsFactors = T, 
                        dec = '.')
  names(drift_h)[names(drift_h) == "X"] <- "lon"
  names(drift_h)[names(drift_h) == "Y"] <- "lat"
  # Need to sort RIO by ascending order in order to match with grid_l
  drift_h <- drift_h %>%
    arrange(lon, lat)
  # Use grid_l to make raster 
  ras_drift <- raster(list(x = sort(unique(coordinates(grid_l)[,1])), 
                           y = sort(unique(coordinates(grid_l)[,2])), 
                           z = matrix(drift_h$CONC, 
                                      nrow = length(sort(unique(coordinates(
                                        grid_l)[,1]))), 
                                      byrow = T))) 
  # Convert to spatial object (wgs84)
  spmodel <- drift_h
  spmodel$Long <- spmodel$lon; spmodel$Lat <- spmodel$lat
  coordinates(spmodel) = ~Long + Lat
  proj4string(spmodel) = CRS_RDNEW
  spmodel <- spTransform(spmodel, CRS_WGS84)
  spmodel <- subset(spmodel, select = -c(lat, lon))
  tmp = spmodel@coords; dlon = tmp[,1]; dlat = tmp[,2]
  spmodel$lon <- dlon
  spmodel$lat <- dlat
  class(spmodel); summary(spmodel)
  names(spmodel)[names(spmodel) == "CONC"] <- "pol"
  
  
  
  ## Select sensor data and get the Variance of Measurement Errors (VME) ##
  
  # Select sensor data per hour
  data_sub = subset(data_preproc, date >= date_start & date < date_end)
  # Convert to spatial object
  data_sf <- data_sub %>%
    st_as_sf(coords = c('lon','lat'), crs = 4326, remove=FALSE) %>%
    st_transform(crs = 28992) %>%
    dplyr::select (-sensor, 
                   -date)
  # Join data with grid_s to get the sensor observations per grid cell 
  data_join <- data_sf %>% 
    sf::st_join(grid_s, left = T)
  data_join <- data_join %>%
    arrange(id_cell)
  # Export the ids
  unique <- unique(data_join$id_cell)
  # Remove the geometry to convert to dataframe to use function
  data_join <- st_set_geometry(data_join, NULL)
  # Run the function 
  vme_pol_msens_all <- vme2df(data_join, U_ms)
  # Replace null values with very low values to avoid NAN in the colorscale
  vme_pol_msens_all <- replace(vme_pol_msens_all, vme_pol_msens_all == 0, 1e-30) 
  # Replace -9999 values by the max uncertainty
  uncert_max <- max(vme_pol_msens_all)
  vme_pol_msens_all <- replace(vme_pol_msens_all, vme_pol_msens_all == -9999,
                               uncert_max * 2)
  vme_data <- as.data.frame(vme_pol_msens_all)
  # Add id_cell to merge and get the VME for each sensor obs per grid cell
  vme_data$id_cell <- unique
  data_ms <- merge(data_join, vme_data, by = 'id_cell')
  data_ms$vme <- data_ms$vme_pol_msens_all 
  # Convert to spatial object 
  data_ms$Long <- data_ms$lon
  data_ms$Lat <- data_ms$lat
  coordinates(data_ms) <- ~Long + Lat
  proj4string(data_ms) <- CRS_WGS84
  data_ms <- spTransform(data_ms, CRS_RDNEW)
  data_ms <- subset(data_ms, select = -c(lat, lon, vme_pol_msens_all))
  tmp <- data_ms@coords; dlon <- tmp[,1]; dlat <- tmp[,2]
  data_ms$lon <- dlon; data_ms$lat <- dlat
  class(data_ms); summary(data_ms)
  data_wgs84 <- spTransform(data_ms, CRS_WGS84)
  
  # Number of data points for kriging
  nbr_points = length(data_ms)
  nbr_pts_all[time] = nbr_points
  print(paste0("NBR POINTS FOR KRIGING = ", nbr_points))
  
  
  
  ## Plot concentrations and VME ##
  
  # Color and scale for drift
  if (pol == "PM2.5") {
    zl <- c(1, 50)
  } 
  ras_drift[which(ras_drift@data@values < zl[1])] <- zl[1]
  ras_drift[which(ras_drift@data@values > zl[2])] <- zl[2]
  ncol = 100
  brks <- seq(zl[1], zl[2], length.out = ncol+1)
  brkslab <- format(brks, scientific = FALSE, digits = 0)
  indbrks <-  seq(1, length(brks), by = 15)
  mycol_ncol <- colorRampPalette(mycol)(ncol)
  
  # Color and scale for sensor data
  data_plt <- data_ms
  data_plt@data$pol[which(data_plt@data$pol < zl[1])] <- zl[1]+0.1
  data_plt@data$pol[which(data_plt@data$pol > zl[2])] <- zl[2]-0.1
  cut_pol <- cut(data_plt@data$pol, brks)
  col_pol <- mycol_ncol[cut_pol]
  
  # Color and scale for VME
  data_plt$vme <- data_plt$vme
  vme_lim <- c(0,1000)
  ncol2 <- 9
  brks2 <- seq(vme_lim[1], vme_lim[2], length.out = ncol2+1)
  brkslab2 <- format(brks2, scientific = FALSE, digits = 0)
  indbrks2 <- seq(1, length(brks2), by = 20)
  mycol2 <- brewer.pal(9, "YlOrRd")
  mycol_ncol2 <- colorRampPalette(mycol2)(ncol2)
  data_plt@data$vme[which(data_plt@data$vme < vme_lim[1])] <- vme_lim[1]
  data_plt@data$vme[which(data_plt@data$vme > vme_lim[2])] <- vme_lim[2]
  cut_vme <- cut(data_plt@data$vme, brks2)
  col_vme <- mycol_ncol2[cut_vme]
  
  # Plot concentrations drift and sensor data
  png(paste(dirout, city, "_drift_", pol, "_", estim_period, "_", MS,
            "_CONC.png", sep=""), width = 900, height = 900)
  plot(ras_drift,
       col = mycol_ncol, zlim = zl, breaks=brks, interpolate = FALSE,
       maxpixels = 500000000, main = paste0(substr(estim_period, 10, 15)),
       cex.main = 2, xlab = "Longitude", ylab = "Latitude", cex.lab = 1.5,
       cex.axis = 1.5, legend.width = 1.2, legend.shrink = 0.75,
       legend.args = list(text = expression(paste("(", mu, "g/", m^3, ")")),
                          cex = 1.5,line = 1,font = 1),
       axis.args = list(at = brks[indbrks], labels = brkslab[indbrks],
                        cex.axis = 1.5))
  plot(data_plt, pch = 21, col = col_pol, bg = col_pol, add = TRUE, cex = 2)
  dev.off()
  
  
  
  # Plot concentrations and VME
  png(paste(dirout, city,"_drift_", pol, "_", estim_period, "_", MS, "_VME.png",
            sep = ""), width = 900, height = 900)
  plot(ras_drift,
       col = mycol_ncol, zlim = zl, breaks = brks, interpolate = FALSE,
       maxpixels = 500000000, main = paste(substr(estim_period, 10, 15)),
       cex.main = 2, xlab = "Longitude", ylab = "Latitude", cex.lab = 1.5,
       cex.axis = 1.5, legend.width = 1.2, legend.shrink = 0.75,
       legend.args = list(text = expression(paste("(", mu, "g/", m^3, ")")),
                          cex = 1.5,line = 1,font = 1),
       axis.args = list(at = brks[indbrks], labels = brkslab[indbrks],
                        cex.axis = 1.5))
  plot(data_plt, pch = 21, col = col_vme, bg = col_vme, cex = 2, add = TRUE)
  image.plot(legend.only = TRUE, add = TRUE, horizontal = TRUE, zlim = vme_lim,
             breaks = brks2, col = mycol_ncol2, legend.shrink =.9,
             legend.width = 1., legend.mar = 8,
             legend.args = list(text = expression(paste("VME (", mu, "g/", m^3,
                                                        ")",)^2), cex = 1.5,
                                line = 1, font = 1, col = 'black'),
             axis.args = list(at = brks2, labels = format(brks2, digits = 2),
                              cex.axis = 1.2, col.axis = "black", col = "black"))
  dev.off()
  
  
  
  ## Correlation between drift and sensor data ##
  
  data_ms2 <- as.data.frame(data_ms) # Adds the coordinates
  
  # Get the PM2.5 concentrations of the nearest model point (spmodel) to sensor
  # obs (data_wgs84)
  model_pol <- rep(0, length(data_ms2[,1]))
  for (ll in 1:length(data_wgs84)) {
    data_wgs84_tmp <- data_wgs84[ll,]
    dist_vector <- spDistsN1(spmodel, data_wgs84_tmp, longlat = TRUE)
    dist_vector <- dist_vector * 1e3 # km to m
    dist_min <- min(dist_vector)
    idx <- which(dist_vector <= dist_min)
    tmp <- spmodel[idx,]
    tmp2 <- as.data.frame(tmp)
    pol_tmp2 <- mean(tmp2$pol) # In case of several points at same distance
    model_pol[ll] <- pol_tmp2
  }
  
  # Arrange data and get correlations
  data_ms$Drift <- model_pol
  data_ms2$MODEL_POL <- model_pol
  data_ms2 <- subset(data_ms2, select = -c(Lat,Long))
  # Change the order so that it matches with the grid
  drift_h <- drift_h %>%
    arrange(lat)
  # Convert to spatial object
  driftd <- drift_h
  coordinates(driftd) <- ~lon + lat
  proj4string(driftd) <- CRS_RDNEW
  # Add model concentrations to grid
  grid_l$Drift <-  driftd$CONC
  correlation <- round(cor(data_ms$pol, data_ms$Drift), 2)
  
  # Plot of the correlation between drift and sensor data
  png(paste0(dirout, "Correlation_", estim_date2, "_",
             estim_HH_end, "h.png"), width = 600, height = 600, type = "cairo",
      bg = "white")
  par(mar = c(7, 9, 3, 9))
  plot(data_ms$pol, data_ms$Drift, pch = 16, col = "black", 
       main = paste(substr(estim_period, 10, 15)),cex.main = 1.5, 
       xlab = bquote(SF ~ .(pol) ~ (mu*g/m^3)), 
       ylab = bquote(Drift ~ .(pol) ~ (mu*g/m^3)), cex.lab = 1.5, 
       cex.axis = 1.5, ylim = c(0,50))
  reg = lm(pol~Drift, data_ms)
  abline(reg, col = "red", lwd = 2)
  abline(0, 1, lwd = 1, lty = 2, col = "blue")
  text(10, 30, paste("R=", correlation, sep=""), cex = 2, col = 'red')
  dev.off()
  
  
  
  ## Kriging (based on Rgeostats package) ##
  
  print("START KRIGING")
  
  # Create database for kriging
  data.u.neigh <- neigh.create(ndim = 2, type = 0) # ndim: spatial dimension and type=0: unique neighborhood
  data.db <- db.create(data_ms[,c("lon", "lat", "pol", "vme", "Drift")],
                       flag.grid = F, ndim = 2, autoname = F)
  grid.db <- db.create(grid_l[,c("lon", "lat", "Drift")],
                       flag.grid = F, autoname = F)
  # Need to identify the variables in a db_class language
  data.db <- db.locate(data.db, 'Drift', "f", 1) # f is default variable for calling function external drift
  data.db <- db.locate(data.db, 'vme', "v", 1) # v is default variable for calling measurement error variance
  grid.db <- db.locate(grid.db, 'Drift', "f", 1)
  unique.neigh <- neigh.create(type = 0, ndim = 2)
  
  # Calculate the residuals between data and drift (to get the variogram)
  data_ms$resvcdrift <- reg_VC(pol~Drift, data_ms)
  data.u.neigh <- neigh.create(ndim = 2, type = 0)
  data2.db <- db.create(data_ms[,c("lon", "lat", "pol", "vme", "Drift",
                                   "resvcdrift")],
                        flag.grid = F, ndim = 2, autoname = F)
  data2.db <- db.locate(data2.db, "resvcdrift", "z")
  
  # Calculate the variogram map based on the residuals
  # Get experimental variogram
  vario_drift <- vario.calc(data2.db)
  # Theoretical variogram fitted on the empirical variogram
  vario_drift_model <- model.auto(vario_drift, draw = F,
                                  c('Nugget Effect', 'Spherical',
                                    'Exponential', 'Gaussian', 'Linear',
                                    'Cubic'),
                                  wmode = 2, auth.aniso = TRUE,
                                  xlab = 'distance', ylab = "variogram")
  # Plot variograms
  png(paste0(dirout, "Variogram_", estim_date2, "_", estim_HH_end, "h.png"),
      width = 700, height = 600, type = "cairo", bg = "white")
  par(mar = c(7, 9, 3, 9))
  plot(vario_drift, npairdw = TRUE, npairpt = TRUE, 
       main = paste(substr(estim_period, 10, 15)), lwd = 3,
       cex.main = 1.5, xlab = "Distance h (m)", ylab = "γ(h)", cex.lab = 1.5,
       cex.axis = 1.5)
  plot(vario_drift_model, add = T, col = "seagreen", lwd = 3)
  dev.off()
  
  # Prepare database and kriging
  data2.db <- db.locate(data2.db, "pol", "z")
  data2.db <- db.locate(data2.db, "Drift", "f", 1)
  data2.db <- db.locate(data2.db, "vme", "v")
  krige_ED <- kriging(data2.db, grid.db, model = vario_drift_model,
                      uc = c("1", "f1"), neigh = unique.neigh)
  
  # Allocate results to the grid and save
  grid_l$Pred_EDK  <- db.extract(krige_ED, 'Kriging.pol.estim')
  grid_l$StDev_EDK <- db.extract(krige_ED, 'Kriging.pol.stdev')
  Pred_EDK  <- db.locate(krige_ED, 8, NA)
  StDev_EDK <- db.locate(krige_ED, 9, NA)
  write.table(grid_l, file = paste(pathout, "EDK_grid_", city, "_", pol, "_",
                                   estim_period, "_", MS, '.csv', sep=''),
              row.names = F, col.names = T, sep=',')
  
  
  
  ## Prediction and mapping error ##
  
  # Create a raster and define color scale for the prediction
  ras_Pred <- raster(list(x = sort(unique(coordinates(grid_l)[,1])),
                          y = sort(unique(coordinates(grid_l)[,2])),
                          z = matrix(grid_l$Pred_EDK, nrow = length(sort(unique(
                            coordinates(grid_l)[,1]))), byrow = F)))
  
  if (pol == "PM2.5"){
    zl <- c(0,50)
  }
  ras_Pred[which(ras_Pred@data@values < zl[1])] <- zl[1]
  ras_Pred[which(ras_Pred@data@values > zl[2])] <- zl[2]
  ncol <- 50
  brks <- seq(zl[1], zl[2], length.out = ncol+1)
  brkslab <- format(brks, scientific = FALSE, digits = 0)
  indbrks <-  seq(1, length(brks), by = 5)
  mycol_ncol2 <- colorRampPalette(mycol)(ncol)
  
  # Plot of the predictions from the kriging
  png(paste0(dirout, "Pred_", city, "_", pol, "_", estim_period, "_", MS, ".png"),
      width = 900, height = 900, type = "cairo", bg = "white")
  plot(ras_Pred, col = mycol_ncol, scale = 1, zlim = zl,
       xlab = 'Longitude', ylab = 'Latitude', cex.lab = 1.5, cex.axis = 1.5,
       main = paste(substr(estim_period, 10, 15),"Fused map"), cex.main = 1.5,
       legend.width = 1.2, legend.shrink=0.75,
       legend.args = list(text = expression(paste("(", mu, "g/", m^3, ")")),
                          cex = 1.5, line = 1, font = 1),
       axis.args = list(at = brks[indbrks], labels = brkslab[indbrks],
                        cex.axis = 1.5))
  dev.off()
  
  
  
  
  # Create a raster and define color scale for the errors
  ras_StDev <- raster(list(x = sort(unique(coordinates(grid_l)[,1])),
                           y = sort(unique(coordinates(grid_l)[,2])),
                           z = matrix(grid_l$StDev_EDK, nrow = length(sort(
                             unique(coordinates(grid_l)[,1]))), byrow = F)))
  if (pol == "PM2.5"){
    zl <- c(0,20)
  }
  ras_StDev[which(ras_StDev@data@values < zl[1])] <- zl[1]
  ras_StDev[which(ras_StDev@data@values > zl[2])] <- zl[2]
  ncol <- 50
  brks <- seq(zl[1], zl[2], length.out = ncol+1)
  brkslab <- format(brks, scientific = FALSE, digits = 2)
  indbrks <-  seq(1, length(brks), by = 10)
  mycol_ncol2 <- colorRampPalette(mycol)(ncol)
  
  # Plot of the kriging error
  png(paste0(dirout, "Stdev_", city, "_", pol, "_", estim_period, "_", MS,".png"),
      width = 900, height = 900, type = "cairo", bg = "white")
  plot(ras_StDev, scale = 1, col = mycol_ncol2, zlim = zl,
       xlab = 'Longitude', ylab = 'Latitude', cex.lab = 1.5, cex.axis = 1.5,
       main = paste(substr(estim_period, 10, 15),"Error map"), cex.main = 1.5,
       legend.width = 1.2, legend.shrink = 0.75,
       legend.args = list(text = expression(paste("(", mu, "g/", m^3, ")")),
                          cex = 1.5, line = 1, font = 1),
       axis.args = list(at = brks[indbrks], labels = brkslab[indbrks],
                        cex.axis = 1.5))
  dev.off()
  
  
  
} # End of time loop





