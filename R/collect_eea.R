setwd("~/R/projects/calicantus/run/eea/")
source('~/R/projects/calicantus/R/get_eea.R')
eea_get_metadata()
filein="eea_latest_data.csv"
if(file.exists(filein)) eea_clean_old_data(filein, days=8)
eea_get_latest_data(countries = c("SI", "AT", "HR", "CH", "MT", 
                                  "CZ", "HU", "SK", "RS", "MK",
                                  "BG", "IT"), 
                    pollutants = c("PM10", "PM2.5", "NO2", "O3"), 
                    action="append") -> dd

