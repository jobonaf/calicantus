

openaq_get_metadata <- function(fileout = "openaq_metadata.csv", countries = c("BA", "MK", "XK")) {
  library(ropenaq)
  library(dplyr)
  library(data.table)
  ana <- NULL
  for(country in countries) ana <- bind_rows(ana,aq_locations(country = country))
  ana %>% select(location, city, country, sourceName, latitude, longitude) -> ana
  
  if(nrow(ana)>0) {
    if(!is.null(fileout)) {
      if(file.exists(fileout)) {
        oldana <- fread(fileout)
        bind_rows(ana,oldana) %>% 
          filter(!is.na(longitude), !is.na(latitude)) %>%
          distinct(location, city, country, sourceName, latitude, longitude) -> ana
      }
      fwrite(ana, file = fileout)
    }
  } else {
    warning("problems with ropenaq::aq_locations")
  }
  return(ana)
}


openaq_get_data <- function(countries = c("BA", "MK", "XK"), 
                            pollutants = c('pm25', 'pm10', 'so2', 'no2', 'o3', 'co', 'bc'),
                            fileout = "openaq_latest_data.csv",
                            from = format(Sys.Date()-10, "%Y-%m-%d"),
                            to = format(Sys.Date(), "%Y-%m-%d"),
                            action = c("append", "overwrite")) {
  library(ropenaq)
  library(dplyr)
  library(data.table)
  action <- match.arg(action)
  pollutants <- match.arg(pollutants, several.ok = T)
  kk <- c("location","parameter","value","unit","country",
          "city","latitude","longitude","dateLocal",
          "averagingPeriod.value","averagingPeriod.unit",
          "attribution_name")
  
  Dat <- NULL
  for(country in countries) {
    for(pollutant in pollutants) {
      cat(paste0("from openaq: country ",country,", pollutant ",pollutant), sep="\n")
      dat <- NULL
      if(any(unclass(aq_locations(country = country)[,pollutant])[[1]])) {
        aq_measurements(country = country, date_from = from, 
                        date_to = to, parameter = pollutant, 
                        attribution = T, has_geo = T, averaging_period = T,
                        source_name = T) -> dat
      }
      if(!is.null(dat) && nrow(dat)>0) {
        dat <- dat[,kk]
        Dat <- bind_rows(Dat, dat)
      }
    }
  }
  

  if(!is.null(fileout) && !is.null(Dat) && nrow(Dat)>0) {
    setDT(Dat, key = kk)
    if(file.exists(fileout) && action=="append") {
      oldDat <- fread(fileout, data.table = TRUE)
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


openaq_clean_old_data <- function(filein="openaq_latest_data.csv", days=15) {
  library(data.table)
  library(dplyr)
  dat <- fread(filein)
  fTime <- Sys.time()-60*60*24*days
  dat %>% filter(dateLocal >= fTime) -> dat
  fwrite(dat, file = filein, append=FALSE)
}
