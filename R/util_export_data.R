
get_models_timeseries <- function(mod_sources=c("ARIANET","CAMS50","RSE","ARPAE"),
                                  obs_sources=c("ARPA-Piemonte","ARPA-VdA","ARPA-Lombardia",
                                                "APPA-Bolzano","APPA-Trento","ARPAV",
                                                "ARPA-FVG","ARPAE"),
                                  fday=Sys.Date()-30,
                                  lday=Sys.Date()-1,
                                  type=c("stations","cities"),
                                  scads=24:47,
                                  verbose=F,
                                  toRDS=T) {
  library(futile.logger)
  loglev <- flog.threshold()
  if(verbose) flog.threshold("DEBUG")->devnull
  mod_sources <- match.arg(mod_sources, several.ok = T)
  type <- match.arg(type)
  ts_dir <- "/home/giovanni/R/projects/calicantus/data/mod-data/timeseries/"
  dates <- seq.Date(from=fday, to=lday, by="1 day")
  ds <- expand.grid(dates, mod_sources)
  files <- format(ds[,1], format=paste0(ts_dir,"%Y/%m/%d/",ds[,2],
                                        "_ref%Y%m%d_timeseries.rda"))
  files <- files[file.exists(files)]
  files <- files[file.size(files)>20000]
  flog.info(paste0("Input files found: ",length(files)))
  flog.debug(paste("Input files:",paste(basename(files),collapse=", ")))
  library(dplyr)
  library(purrr)
  
  read_stations <- function(ff) {
    load(ff)
    EmTime <- as.POSIXct(substr(strsplit(basename(ff),"_")[[1]][2],4,11),
                     format="%Y%m%d", tz="UTC")
    flog.debug(EmTime)
    left_join(stations_data%>%
                mutate(EmissionTime=EmTime)%>%
                filter(as.numeric(difftime(Time,EmissionTime,units="hours"))%in%scads)%>%
                rename(SiteCode=Name)%>% 
                mutate_if(is.factor, as.character),
              stations_metadata%>%
                rename(ObsProvider=Source)%>% 
                mutate_if(is.factor, as.character), by = c("Lon", "Lat", "SiteCode"))%>%
      filter(ObsProvider%in%obs_sources)
  }
  
  read_cities <- function(ff) {
    load(ff)
    EmTime <- as.POSIXct(substr(strsplit(basename(ff),"_")[[1]][2],4,11),
                     format="%Y%m%d", tz="UTC")
    flog.debug(EmTime)
    cities_data%>%
      mutate(EmissionTime=EmTime)%>%
      filter(as.numeric(difftime(Time,EmissionTime,units="hours"))%in%scads)%>%
      rename(ModProvider=Source)%>% 
      mutate_if(is.factor, as.character)
  }
  
  if(type=="stations") {
    files %>% purrr::map(read_stations) %>% reduce(bind_rows) -> Dat
  }
  if(type=="cities") {
    files %>% purrr::map(read_cities) %>% reduce(bind_rows) -> Dat
  }
  
  Dat %>% rename(ModProvider=Source) -> Dat
  if(toRDS) {
    fileout <- paste0("Forecast_",
                      format(fday,format="%Y%m%d"),"-",format(lday,format="%Y%m%d"),"_",
                      sprintf(min(scads),fmt="%03i"),"-",sprintf(max(scads),fmt="%03i"),
                      ".rds")
    flog.debug(paste("Writing",fileout))
    saveRDS(object = Dat, file = fileout)
  }
  flog.threshold(loglev)->devnull
  return(Dat)
}

