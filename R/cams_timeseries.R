setwd("~/R/projects/calicantus/run/cams")
source("~/R/projects/calicantus/R/cams.R")

if(!interactive()) {
  reftime1 <- Sys.Date()
  reftime2 <- Sys.Date()
} else {
  reftime1 <- as.Date(readline(prompt = "First reference time? (YYYY-MM-DD)"))
  reftime2 <- as.Date(readline(prompt = "Last reference time? (YYYY-MM-DD)"))
}

reftimes <- seq.Date(reftime1,reftime2,by="1 days")
timeranges <- c("0H24H","25H48H","49H72H","73H96H")
validity <- 0:3 # must be coherent with timeranges
models <- c("CHIMERE","EMEP","EURAD","LOTOSEUROS","MATCH","MOCAGE","SILAM")
pollutants <- c("PM10","PM25","O3")

library(dplyr)
library(tidyr)
cities <- select_cities()
s1 <- read.csv("~/R/projects/calicantus/data/sites-info/extended_metadata/metadata_IT_O3.csv")
s2 <- read.csv("~/R/projects/calicantus/data/sites-info/extended_metadata/metadata_IT_PM10.csv")
s3 <- read.csv("~/R/projects/calicantus/data/sites-info/extended_metadata/metadata_IT_PM2.5.csv")
bind_rows(s1,s2,s3) %>% 
  filter(AirQualityStationType=="background") %>%
  select(-InletHeight,-BuildingDistance,-KerbDistance) %>%
  group_by(SiteCode) %>% slice(1) %>% ungroup() %>%
  distinct() -> stations_metadata
stations_metadata %>%
  select(SiteCode, Lat, Lon) %>% 
  distinct() -> stations

for(reftime in as.character(reftimes)) {
  cat(reftime,sep="\n")
  cities_data <- NULL
  stations_data <- NULL
  for(model in models) {
    cat(model,sep="\n")
    for(pollutant in pollutants) {
      cat(pollutant,sep="\n")
      Files <- paste0("CAMS50_",format(as.Date(reftime),"%Y%m%d"),
                      "_",pollutant,"_",model,"_",timeranges,".nc")
      if(all(file.exists(Files)) && all(file.size(Files)>0)) {
        tss  <- cams2ts(Files, Lon = cities$long,  Lat = cities$lat,   Names = cities$name)
        tss2 <- cams2ts(Files, Lon = stations$Lon, Lat = stations$Lat, Names = stations$SiteCode)
        tss  <- data.frame(tss,  Pollutant=pollutant, Model=model)
        tss2 <- data.frame(tss2, Pollutant=pollutant, Model=model)
        if(is.null(cities_data)) {
          cities_data  <- tss
          stations_data <- tss2
        } else {
          cities_data  <- bind_rows(cities_data ,tss)
          stations_data <- bind_rows(stations_data,tss2)
        }
      }
    }
  }
  cities_data$Pollutant  <- as.factor(cities_data$Pollutant)
  cities_data$Name       <- as.factor(cities_data$Name)
  cities_data$Model      <- as.factor(cities_data$Model)
  stations_data$Pollutant <- as.factor(stations_data$Pollutant)
  stations_data$Name      <- as.factor(stations_data$Name)
  stations_data$Model     <- as.factor(stations_data$Model)
  save(cities_data ,stations_data, stations_metadata,
       file=paste0("CAMS50_ref",format(as.Date(reftime),"%Y%m%d"),
                   "_timeseries.rda"))
}
