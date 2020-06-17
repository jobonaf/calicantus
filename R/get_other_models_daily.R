# preliminar
setwd("~/R/projects/calicantus/run/other-models/")
source("~/R/projects/calicantus/R/get_other_models.R")
source("~/R/projects/calicantus/R/cams.R")
library(futile.logger)

# reference time (day of emission)
if(!interactive()) {
  aa <- commandArgs(trailingOnly = TRUE)
  if(length(aa)>1) {
    reftime <- as.Date(aa[2])
  } else {
    reftime <- Sys.Date()
  }
} else {
  reftime <- as.Date(readline(prompt = "Reference time? (YYYY-MM-DD)"))
}

# parameters
path_mod_grid <- "/home/giovanni/R/projects/calicantus/data/mod-data/grid/"
path_mod_ts   <- "/home/giovanni/R/projects/calicantus/data/mod-data/timeseries/"
pollutants = c("PM10","PM25","O3")
stats = list(PM10=c("Mean"), PM25=c("Mean"), O3=c("MaxAvg8h","Max"))

# points to extract
library(dplyr)
library(tidyr)
cities <- select_cities()
s1 <- read.csv("~/R/projects/calicantus/data/sites-info/extended_metadata/metadata_IT_O3.csv")
s2 <- read.csv("~/R/projects/calicantus/data/sites-info/extended_metadata/metadata_IT_PM10.csv")
s3 <- read.csv("~/R/projects/calicantus/data/sites-info/extended_metadata/metadata_IT_PM2.5.csv")
bind_rows(s1,s2,s3) %>% 
  filter(AirQualityStationType=="background") %>%
  dplyr::select(-InletHeight,-BuildingDistance,-KerbDistance) %>%
  group_by(SiteCode) %>% slice(1) %>% ungroup() %>%
  distinct() -> stations_metadata
stations_metadata %>%
  dplyr::select(SiteCode, Lat, Lon) %>% 
  distinct() -> stations


# RSE ---------------------------------------------------------------------

# parameters
validity <- 0:2
Source <- "RSE"
Model <- "CAMx"
Grid <- "Italy"
getmodel_ftp(config = "~/R/projects/calicantus/config/mod-data-access/access.RSE_CAMx_Italy.R",
             day = reftime) -> Files
Files <- Files[file.exists(Files)]
stations_data <- cities_data <- NULL
for (File in Files) {
  for (pollutant in pollutants) {
    # read and process gridded data
    dat <- read_rse(File, pollutant)
    Dat <- ctm_daily_stat(dat,   
                          dMean="Mean" %in% stats[[pollutant]],
                          dMax="Max" %in% stats[[pollutant]],
                          dMaxAvg8h="MaxAvg8h" %in% stats[[pollutant]])
    # extract timeseries
    s_data <- ctm_timeseries(dat, Lat=stations$Lat, Lon=stations$Lon, Names=stations$SiteCode)
    c_data <- ctm_timeseries(dat, Lat=cities$lat,   Lon=cities$long,  Names=cities$name)
    s_data <- data.frame(s_data, Pollutant=pollutant, Source=Source, Model=Model, Grid=Grid)
    c_data <- data.frame(c_data, Pollutant=pollutant, Source=Source, Model=Model, Grid=Grid)
    stations_data <- bind_rows(stations_data, s_data)
    cities_data   <- bind_rows(cities_data,   c_data)
    # convert gridded data to rasters and save them
    for (val in validity) {
      valtime <- as.Date(reftime)+val
      for (stat in stats[[pollutant]]) {
        r <- ctm2raster(Dat,day=format(valtime,"%Y-%m-%d"),stat,crs = dat$crs)
        if(!is.null(r)) {
          save(r,file=paste0(path_mod_grid,format(as.Date(reftime),"/%Y/%m/%d/"),
                             Source,"_ref",format(as.Date(reftime),"%Y%m%d"),
                             "_val",format(valtime,"%Y%m%d"),"_",Model,Grid,
                             "_",pollutant,"_",stat,".rda"))
        }
      }
    }
  }
}
# save timeseries
if(length(cities_data)>0 & length(stations_data)>0) {
  cities_data %>% rename(Conc=conc) -> cities_data
  stations_data %>% rename(Conc=conc) -> stations_data
  save(cities_data, stations_data, stations_metadata,
       file=paste0(path_mod_ts,format(as.Date(reftime),"/%Y/%m/%d/"),
                   Source,"_ref",format(as.Date(reftime),"%Y%m%d"),
                   "_timeseries.rda"))
}
flog.info(paste0("Removing files ",paste0(Files,collapse=" ")))
file.remove(Files)
Files <- NULL


# ARIANET -----------------------------------------------------------------

# parameters
validity <- 0:2
Source <- "ARIANET"
Model <- "FARM"
GridCodes <- c("g1","g2")
Grids <- c("Europe","Italy")
getmodel_ftp(config = "~/R/projects/calicantus/config/mod-data-access/access.ARIANET_FARM_Italy.R",
             day = reftime) -> Files
Files <- Files[file.exists(Files)]
stations_data <- cities_data <- NULL
for (ig in 1:length(Grids)) {
  for (pollutant in pollutants) {
    # read and process gridded data
    FFs <- NULL
    for (File in Files) {
      ffs <- prepare_arianet(File)
      ff <- ffs[grep(pattern = GridCodes[ig], x = ffs)]
      FFs <- c(FFs,ff)
    }
    dat <- read_arianet_multi(FFs, pollutant)
    Dat <- ctm_daily_stat(dat,   
                          dMean="Mean" %in% stats[[pollutant]],
                          dMax="Max" %in% stats[[pollutant]],
                          dMaxAvg8h="MaxAvg8h" %in% stats[[pollutant]])
    # extract timeseries
    s_data <- ctm_timeseries(dat, Lat=stations$Lat, Lon=stations$Lon, Names=stations$SiteCode)
    c_data <- ctm_timeseries(dat, Lat=cities$lat,   Lon=cities$long,  Names=cities$name)
    s_data <- data.frame(s_data, Pollutant=pollutant, Source=Source, Model=Model, Grid=Grids[ig])
    c_data <- data.frame(c_data, Pollutant=pollutant, Source=Source, Model=Model, Grid=Grids[ig])
    stations_data <- bind_rows(stations_data, s_data)
    cities_data   <- bind_rows(cities_data,   c_data)
    # convert gridded data to rasters and save them
    for (val in validity) {
      valtime <- as.Date(reftime)+val
      for (stat in stats[[pollutant]]) {
        r <- ctm2raster(Dat,day=format(valtime,"%Y-%m-%d"),stat,crs = dat$crs)
        if(!is.null(r)) {
          save(r,file=paste0(path_mod_grid,format(as.Date(reftime),"/%Y/%m/%d/"),
                             Source,"_ref",format(as.Date(reftime),"%Y%m%d"),
                             "_val",format(valtime,"%Y%m%d"),"_",Model,Grids[ig],
                             "_",pollutant,"_",stat,".rda"))
        }
      }
    }
  }
}
# save timeseries
if(length(cities_data)>0 & length(stations_data)>0) {
  cities_data %>% rename(Conc=conc) -> cities_data
  stations_data %>% rename(Conc=conc) -> stations_data
  save(cities_data, stations_data, stations_metadata,
       file=paste0(path_mod_ts,format(as.Date(reftime),"/%Y/%m/%d/"),
                   Source,"_ref",format(as.Date(reftime),"%Y%m%d"),
                   "_timeseries.rda"))
}
flog.info(paste0("Removing files ",paste0(Files,collapse=" ")))
file.remove(Files)
Files <- NULL


# ARPAE ---------------------------------------------------------------------

# parameters
validity <- 0:2
Source <- "ARPAE"
Model <- "CHIMERE"
Grid <- "NorthernItaly"
try(getmodel_gdrive(config = "~/R/projects/calicantus/config/mod-data-access/access.ARPAE_CHIMERE_NorthernItaly.R",
                day = reftime) -> Files)
Files <- Files[file.exists(Files)]
stations_data <- cities_data <- NULL
for (File in Files) {
  for (pollutant in pollutants) {
    # read and process gridded data
    dat <- read_ninfa(File, pollutant)
    Dat <- ctm_daily_stat(dat,   
                          dMean="Mean" %in% stats[[pollutant]],
                          dMax="Max" %in% stats[[pollutant]],
                          dMaxAvg8h="MaxAvg8h" %in% stats[[pollutant]])
    # extract timeseries
    s_data <- ctm_timeseries(dat, Lat=stations$Lat, Lon=stations$Lon, Names=stations$SiteCode)
    c_data <- ctm_timeseries(dat, Lat=cities$lat,   Lon=cities$long,  Names=cities$name)
    s_data <- data.frame(s_data, Pollutant=pollutant, Source=Source, Model=Model, Grid=Grid)
    c_data <- data.frame(c_data, Pollutant=pollutant, Source=Source, Model=Model, Grid=Grid)
    stations_data <- bind_rows(stations_data, s_data)
    cities_data   <- bind_rows(cities_data,   c_data)
    # convert gridded data to rasters and save them
    for (val in validity) {
      valtime <- as.Date(reftime)+val
      for (stat in stats[[pollutant]]) {
        r <- ctm2raster(Dat,day=format(valtime,"%Y-%m-%d"),stat,crs = dat$crs)
        if(!is.null(r)) {
          save(r,file=paste0(path_mod_grid,format(as.Date(reftime),"/%Y/%m/%d/"),
                             Source,"_ref",format(as.Date(reftime),"%Y%m%d"),
                             "_val",format(valtime,"%Y%m%d"),"_",Model,Grid,
                             "_",pollutant,"_",stat,".rda"))
        }
      }
    }
  }
}
# save timeseries
if(length(cities_data)>0 & length(stations_data)>0) {
  cities_data %>% rename(Conc=conc) -> cities_data
  stations_data %>% rename(Conc=conc) -> stations_data
  save(cities_data, stations_data, stations_metadata,
       file=paste0(path_mod_ts,format(as.Date(reftime),"/%Y/%m/%d/"),
                   Source,"_ref",format(as.Date(reftime),"%Y%m%d"),
                   "_timeseries.rda"))
}
flog.info(paste0("Removing files ",paste0(Files,collapse=" ")))
file.remove(Files)
Files <- NULL


