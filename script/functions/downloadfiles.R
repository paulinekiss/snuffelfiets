## How to download data 

## Load libraries

library(data.table)
library(RCurl)

## Set directory

setwd('C:/Users/Popi/Documents/applied data study/thesis_project/sf_datasets/snuffelfiets_data/2021')

## Download files from directoy separately 

filenames <- gsub("\\.csv$","", list.files(pattern="\\.csv$"))

for(i in filenames){
  assign(i, read.csv(paste(i, ".csv", sep="")))
}

## Merge files together in one dataframe 

tbl_fread <- 
  list.files(pattern = "*.csv") %>% 
  map_df(~fread(.)) # row bind data together 

tbl_fread <- tbl_fread[order(tbl_fread$recording_time),] # order the data by time

## Download link from url 

url <- "https://ckan.dataplatform.nl/dataset/3660d2e1-84ee-46bf-a7b6-7e9ac1fcaf3a/resource/e2ff1875-eab1-4dcf-aa3b-df99c3dee6fd/download/resource_2020_02_03_2020_02_10.csv"
destfile <- "C:/Users/Popi/OneDrive/PAULINE/Docs/Msc ADS 2020/master_project/sf_datasets/snuffelfiets_data/2020/resource_2020_02_03_2020_02_10.csv"
download.file(url,destfile) # data are still empty! 


try <- download.file("https://ckan.dataplatform.nl/dataset/3660d2e1-84ee-46bf-a7b6-7e9ac1fcaf3a/resource/e2ff1875-eab1-4dcf-aa3b-df99c3dee6fd/download",
                     "resource_2020_02_03_2020_02_10.csv", method = "curl") # still empty 
