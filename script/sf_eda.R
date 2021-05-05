## Snuffelfiets data 2020 and 2021: eda

## Load data 

sf <- read.csv("output/data/sf_c1.csv",
               sep = ",",
               header = TRUE)

## Plot


# 2021 

plot_sf <- openair::timeVariation(mydata=sf_c,
                                  pollutant = 'PM2.5')

openair::timeVariation(mydata=sf_c,
                       pollutant = 'PM2.5',
                       statistic = "median",
                       col = "firebrick") # shows how the data are distributed 

openair::timeVariation(mydata=sf_c,
                       pollutant = 'PM2.5', 
                       type = 'weekend')

# other plots 

plot_sf1 <- openair::timePlot(sf_c, pollutant = 'PM2.5', y.relation = "free")
#ylim = c(0,150)

# other plots 

trendLevel(sf_c, pollutant = "PM2.5", 
           border = "white", 
           cols = "jet")
