## Snuffelfiets project
## First check of snuffelfiets data: report 19/04-26/04 2021

## Load data 

# Snuffelfiets
getwd()
setwd('C:/Users/Popi/OneDrive/PAULINE/Docs/Msc ADS 2020/master_project/sf_datasets/snuffelfiets_data')
sf21 <- read.csv('resource_2021_04_19_2021_04_26.csv')
sf20 <- read.csv('resource_2020_04_13_2020_04_20.csv')
sf19 <- read.csv('resource_2019_05_27_2019_06_03.csv')

## Load packages

library(Hmisc)
library(dlookr)
library(dplyr)

#########################################################################
# Comment on the data: 
# 19 variables in total including 9 empty (NA)
# no missing values !

## Data cleaning (Joost)
# Remove all concentrations < 0.5 and > 150µg/m3
# Remove data far from Utrecht province
# Remove data with speed above 45km/h

# Question: do I remove it or replace by NA?
#########################################################################

## Exploration Data Analysis (EDA)

# Inspect data (n=7088)

str(sf21)
describe(sf21)

# Check format variables 

sf21$pm2_5 <- as.numeric(sf21$pm2_5)
sf21$pressure <- as.numeric(sf21$pressure)
sf21$humidity <- as.numeric(sf21$humidity)

# Missing values: no

# Outliers

dlookr::diagnose_outlier(sf21) 
dlookr::plot_outlier(sf21[c('lon','trip_sequence','humidity','pm2_5','pressure')])

sf21_1 <- filter(sf21, sf21$pm2_5>0.5 & sf21$pm2_5<150) #remove outliers
hist(sf21_1$pm2_5)

# Check correlations 
#if i add this 
str(data)

