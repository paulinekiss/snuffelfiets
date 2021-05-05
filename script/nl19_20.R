## rivm 2020 pm2.5 data

## load libraries
library(tidyverse)
library(lubridate)
library(openair)

## load data

filename <- "https://data.rivm.nl/data/luchtmeetnet/Vastgesteld-jaar/2020/2020_PM25.csv"
filename19 <- "https://data.rivm.nl/data/luchtmeetnet/Vastgesteld-jaar/2019/2019_PM25.csv"

## arrange metadata

csv.metadata <- read.table(file = filename,
                           sep = ";",
                           header = FALSE,
                           nrows = 7)
head(csv.metadata)
file.metadata <- as_tibble(t(csv.metadata[1:4,2]))
names(file.metadata) <- make.names(tolower(csv.metadata[1:4,1]))
file.metadata

station.metadata <- as_tibble(t(csv.metadata[ ,6:ncol(csv.metadata)]))
names(station.metadata) <- make.names(tolower(csv.metadata[ ,5]))
head(station.metadata)

## read data (skipping the metadata)

csv.data <- read.table(file = filename,
                       sep = ";",
                       header = TRUE,
                       skip = 7)

csv.data19 <- read.table(file = filename19,
                         sep = ";",
                         header = TRUE,
                         skip = 7)

## arrange data for openair format

pollutant <- tolower(unique(csv.data$Component)) #in this case we only have 1 so 
#it's not very important

mydata <- csv.data %>%
  dplyr::mutate(
    date = lubridate::ymd_hm(Begindatumtijd, tz = "Etc/GMT-1"),
    date_end = lubridate::ymd_hm(Einddatumtijd, tz = "Etc/GMT-1")) %>%
  dplyr::select(-Component, 
                -Bep.periode, 
                -Eenheid, 
                -Begindatumtijd, 
                -Einddatumtijd) %>%
  tidyr::pivot_longer(!c("date", "date_end"), 
                      names_to = "site", 
                      values_to = pollutant)

mydata19 <- csv.data19 %>%
  dplyr::mutate(
    date = lubridate::ymd_hm(Begindatumtijd, tz = "Etc/GMT-1"),
    date_end = lubridate::ymd_hm(Einddatumtijd, tz = "Etc/GMT-1")) %>%
  dplyr::select(-Component, 
                -Bep.periode, 
                -Eenheid, 
                -Begindatumtijd, 
                -Einddatumtijd) %>%
  tidyr::pivot_longer(!c("date", "date_end"), 
                      names_to = "site", 
                      values_to = pollutant)


## select only sites in Utrecht (n=24(h) * 365(d) * 4(site))

ut_sites <- c('NL10636','NL10641','NL10643','NL10644')
mydata.u <- mydata[mydata$site %in% ut_sites, ]
mydata.u19 <- mydata19[mydata19$site %in% ut_sites, ]

## plot time variation 

# check for one site
openair::timeVariation(mydata=mydata.u %>% filter(site == 'NL10636'),
                       pollutant = 'pm2.5')
# compare the sites
openair::timeVariation(mydata=mydata.u,
                       pollutant = 'pm2.5',
                       group = 'site')

openair::timeVariation(mydata=mydata.u19,
                       pollutant = 'pm2.5',
                       group = 'site')

# average sites 
plot20_nl <- openair::timeVariation(mydata=mydata.u,
                                    pollutant = 'pm2.5')


print(plot20_sf, split = c(1, 1, 2, 1), subset = 'day')
print(plot20_nl, split = c(2, 1, 2, 1), subset = 'day', newpage = FALSE)

print(plot21_sf, split = c(1, 1, 2, 1), subset = 'day')
print(plot20_nl, split = c(2, 1, 2, 1), subset = 'day', newpage = FALSE)
