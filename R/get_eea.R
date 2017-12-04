

eea_get_metadata <- function(filein = "http://discomap.eea.europa.eu/map/fme/metadata/PanEuropean_metadata.csv",
                             fileout = "eea_metadata.csv") {
  library(httr)
  library(dplyr)
  library(data.table)
  ana <- NULL
  st <- HEAD(filein)$status
  if(st==200) {
    ana <- fread(filein, sep="\t", header = T, stringsAsFactors = F, 
                 na.strings = c("-9999","-999","-9900","-100","-99","nan"), dec=".", 
                 encoding = "Latin-1")
  } else {
    warning(paste("problems with",filein,"- code:",st))
  }
  if(nrow(ana)>0) {
    if(!is.null(fileout)) {
      if(file.exists(fileout)) {
        oldana <- fread(fileout, encoding = "Latin-1")
        bind_rows(ana,oldana) %>% 
          filter(!is.na(Longitude), !is.na(Latitude)) %>%
          distinct(Namespace,AirQualityStationEoICode,AirPollutantCode,Sample, .keep_all=TRUE) -> ana
      }
      fwrite(ana, file = fileout)
    }
  } else {
    warning(paste(filein,"is empty"))
  }
  return(ana)
}


eea_get_latest_data <- function(countries = c("SI", "AT", "HR"), #unique(eea_get_metadata()$Countrycode),
                                pollutants = c("PM10", "PM2.5", "NO2", "O3", "CO", "C6H6", "SO2"),
                                path = "http://discomap.eea.europa.eu/map/fme/latest/",
                                fileout = "eea_latest_data.csv",
                                action = c("append", "overwrite")) {
  library(httr)
  library(dplyr)
  library(data.table)
  action <- match.arg(action)
  pollutants <- match.arg(pollutants, several.ok = T)
  Dat <- NULL
  for(country in countries) {
    for(pollutant in pollutants) {
      filein <- paste0(path,country,"_",pollutant,".csv")
      st <- HEAD(filein)$status
      if(st==200) {
        fread(filein, sep=",", header = T, stringsAsFactors = F, 
              na.strings = "-999", quote = '"', encoding="Latin-1") %>%
          filter(!is.na(value_datetime_begin)) -> dat
        readr::guess_encoding(filein)[1,1] -> enc
        dsn <- try(iconv(dat$station_name, from=enc, to="UTF-8"))
        if(class(dsn)!="try-error") dat$station_name <- dsn
        if(nrow(dat)>0) {
          Dat <- bind_rows(Dat, dat)
        }
      }
      
    }
  }
  if(!is.null(fileout) && !is.null(Dat) && nrow(Dat)>0) {
    kk <- c("pollutant","station_code", "network_countrycode",
            "value_datetime_begin","value_datetime_end",
            "value_datetime_inserted","value_datetime_updated")
    setDT(Dat, key = kk)
    if(file.exists(fileout) && action=="append") {
      oldDat <- fread(fileout, data.table = TRUE, encoding = "Latin-1")
      setkeyv(oldDat, cols = kk)
      DAT <- rbindlist(list(oldDat, Dat))
      setkeyv(DAT, cols = kk)
    } else {
      DAT <- Dat
    }
    DAT <- unique(DAT, fromLast = T, by=kk)
    fwrite(DAT, file = fileout, append=FALSE)
  }
  return(Dat)
}


eea_clean_old_data <- function(filein="eea_latest_data.csv", days=10) {
  library(data.table)
  dat <- fread(filein, encoding = "Latin-1")
  fTime <- Sys.time()-60*60*24*days
  uu <- dat$value_datetime_updated
  uu[nchar(uu)<25] <- format(Sys.time(),"%Y-%m-%d %H:%M:%S")
  ii <- as.POSIXct(dat$value_datetime_begin) >= fTime &
    as.POSIXct(dat$value_datetime_inserted) >= fTime &
    as.POSIXct(uu) >= fTime
  dat[ii,] -> dat
  fwrite(dat, file = filein, append=FALSE)
}
