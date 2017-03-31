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

for(reftime in as.character(reftimes)) {
  cat(reftime,sep="\n")
  Dat <- NULL
  for(model in models) {
    cat(model,sep="\n")
    for(pollutant in pollutants) {
      cat(pollutant,sep="\n")
      Files <- paste0("CAMS50_",format(as.Date(reftime),"%Y%m%d"),
                      "_",pollutant,"_",model,"_",timeranges,".nc")
      if(all(file.exists(Files)) && all(file.size(Files)>0)) {
        tss <- cams2ts(Files, Lon = cities$long, Lat = cities$lat, Names = cities$name)
        tss <- data.frame(tss, Pollutant=pollutant, Model=model)
        if(is.null(Dat)) {
          Dat <- tss
        } else {
          Dat <- bind_rows(Dat,tss)
        }
      }
    }
  }
  Dat$Pollutant <- as.factor(Dat$Pollutant)
  Dat$Name <- as.factor(Dat$Name)
  Dat$Model <- as.factor(Dat$Model)
  save(Dat,file=paste0("CAMS50_ref",format(as.Date(reftime),"%Y%m%d"),
                       "_timeseries.rda"))
}
