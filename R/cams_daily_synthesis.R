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
stats <- list(PM10=c("Mean"), PM25=c("Mean"), O3=c("MaxAvg8h","Max"))

for(reftime in as.character(reftimes)) {
  for(model in models) {
    for(pollutant in pollutants) {
      Files <- paste0("CAMS50_",format(as.Date(reftime),"%Y%m%d"),
                      "_",pollutant,"_",model,"_",timeranges,".nc")
      if(all(file.exists(Files)) && all(file.size(Files)>0)) {
        dat <- read_cams_multi(Files)
        dat <- cams_daily_stat(dat,   
                               dMean="Mean" %in% stats[[pollutant]],
                               dMax="Max" %in% stats[[pollutant]],
                               dMaxAvg8h="MaxAvg8h" %in% stats[[pollutant]])
        for (val in validity) {
          valtime <- as.Date(reftime)+val
          for (stat in stats[[pollutant]]) {
            r <- cams2raster(dat,day=format(valtime,"%Y-%m-%d"),stat)
            if(!is.null(r)) {
              save(r,file=paste0("CAMS50_ref",format(as.Date(reftime),"%Y%m%d"),
                                 "_val",format(valtime,"%Y%m%d"),"_",model,
                                 "_",pollutant,"_",stat,".rda"))
            }
          }
        }
      }
    }
  }
}
