setwd("/home/giovanni/R/projects/calicantus/run/openaq/")
source('/home/giovanni/R/projects/calicantus/R/get_openaq.R')
countries <- c("BA", "MK", "XK", "TR")
openaq_get_metadata(countries=countries) -> aa
filein <- "openaq_latest_data.csv"
if(file.exists(filein)) openaq_clean_old_data(filein, days=15)
openaq_get_data(countries = countries, 
                pollutants = c('pm10', 'pm25', 'no2', 'o3'), 
                action="append") -> dd

