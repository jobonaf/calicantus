setwd("~/R/projects/calicantus/run/other-models/")
source("~/R/projects/calicantus/R/get_other_models.R")
source("~/R/projects/calicantus/R/cams.R")

if(!interactive()) {
  reftime <- Sys.Date()
} else {
  reftime <- as.Date(readline(prompt = "Reference time? (YYYY-MM-DD)"))
}

# parameters
pollutants = c("PM10","PM25","O3")
stats = list(PM10=c("Mean"), PM25=c("Mean"), O3=c("MaxAvg8h","Max"))

# RSE
validity <- 0:2
Source <- "RSE"
Model <- "CAMx"
Grid <- "Italy"
getmodel_ftp(config = "~/R/projects/calicantus/config/mod-data-access/access.RSE_CAMx_Italy.R",
             day = reftime) -> Files
for (File in Files) for (pollutant in pollutants) {
  dat <- read_rse(File, pollutant)
  Dat <- ctm_daily_stat(dat,   
                        dMean="Mean" %in% stats[[pollutant]],
                        dMax="Max" %in% stats[[pollutant]],
                        dMaxAvg8h="MaxAvg8h" %in% stats[[pollutant]])
  for (val in validity) {
    valtime <- as.Date(reftime)+val
    for (stat in stats[[pollutant]]) {
      r <- ctm2raster(Dat,day=format(valtime,"%Y-%m-%d"),stat,crs = dat$crs)
      if(!is.null(r)) {
        save(r,file=paste0(Source,"_ref",format(as.Date(reftime),"%Y%m%d"),
                           "_val",format(valtime,"%Y%m%d"),"_",Model,Grid,
                           "_",pollutant,"_",stat,".rda"))
      }
    }
  }
}
## INSERIRE NEL CICLO:
## - ARCHIVIAZIONE STATISTICHE GIORNALIERE (come raster)
## - ESTRAZIONE E ARCHIVIAZIONE SERIE TEMPORALI

# Arianet
validity <- 0:2
Source <- "ARIANET"
Model <- "FARM"
GridCodes <- c("g1","g2")
Grids <- c("Europe","Italy")
getmodel_ftp(config = "~/R/projects/calicantus/config/mod-data-access/access.ARIANET_FARM_Italy.R",
             day = reftime) -> Files
for (ig in 1:length(Grids)) {
  tsDat <- NULL
  for (pollutant in pollutants) {
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
    for (val in validity) {
      valtime <- as.Date(reftime)+val
      for (stat in stats[[pollutant]]) {
        r <- ctm2raster(Dat,day=format(valtime,"%Y-%m-%d"),stat,crs = dat$crs)
        if(!is.null(r)) {
          save(r,file=paste0(Source,"_ref",format(as.Date(reftime),"%Y%m%d"),
                             "_val",format(valtime,"%Y%m%d"),"_",Model,Grids[ig],
                             "_",pollutant,"_",stat,".rda"))
        }
      }
    }
  }
}
## INSERIRE NEL CICLO:
## - CALCOLO E ARCHIVIAZIONE STATISTICHE GIORNALIERE (come raster)
## - ESTRAZIONE E ARCHIVIAZIONE SERIE TEMPORALI



# nc <- get_cams(pollutant = c("PM10","O3","PM25"))
# source("~/R/projects/calicantus/R/cams_daily_synthesis.R")
# source("~/R/projects/calicantus/R/cams_timeseries.R")
# archive_cams()
# archive_cams_timeseries()
# file.remove(nc)