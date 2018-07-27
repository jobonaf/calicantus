library(dplyr)
library(readr)
library(stringr)
library(tidyr)

source('~/R/projects/calicantus/R/util.R')
ref_date <- Sys.Date()-1
days2check <- c(ref_date, 
                ref_date-10, 
                ref_date-100)
eea_get_metadata() -> all_eea_metadata


harmonize_metadata <- function(local_metadata, eea_metadata) {
  local_metadata %>% 
    rename(Lon=LON, Lat=LAT, LocalCode=ID, LocalName=NAME) %>% 
    guess_eoi(., eea.metadata = eea_metadata) %>%
    mutate(EoICode=as.character(EoICode)) %>%
    left_join(., eea_metadata, 
              by=c("EoICode"="AirQualityStationEoICode"), 
              suffix = c("", ".eea")) %>%
    select(Countrycode, LocalCode, LocalName, Lat, Lon,  
           EoICode, Eea.Lat, Eea.Lon, Altitude, 
           AirQualityStationType, AirQualityStationArea,
           MeasurementType, InletHeight, BuildingDistance, KerbDistance) %>%
    mutate(LocalCode=as.character(LocalCode))
}

translate <- function(.x) {
  recode(.x,
         `fondo`      ="background",
         `bakground`  ="background",
         `ind./background`="industrial",
         `traffico`   ="traffic",
         `industriale`="industrial",
         `urbano`     ="urban",
         `suburbano`  ="suburban",
         `urbana`     ="urban",
         `suburbana`  ="suburban",
         `rurale`     ="rural",
         .default     =.x)
}


available_stations <- function(Source, Days, Pollutant) {
  library(dplyr)
  Dat <- NULL
  for(i in 1:length(Days)) {
    ff <- paste0("/home/giovanni/R/projects/calicantus/data/obs-data/",
                 format(Days[i],"%Y/%m/%d/%Y%m%d_"), Pollutant,
                 "_",Source,".rda")
    if(file.exists(ff)) {
      load(ff)
      Dat <- bind_rows(Dat, dat%>%mutate_if(is.factor,as.character))
    }
  }
  if(!is.null(Dat)) Dat <- Dat %>% dplyr::filter(!is.na(Name), !is.na(Value))  %>% select(Name)  %>% distinct() %>% combine()
  futile.logger::flog.info(paste0(Pollutant,", ",Source,": found ",length(Dat)," active stations in days ",paste(Days,collapse=", ")))
  Dat
}


poll2url <- function(.x) {
  recode(.x,
         `PM10`  ="http://dd.eionet.europa.eu/vocabulary/aq/pollutant/5",
         `PM2.5` ="http://dd.eionet.europa.eu/vocabulary/aq/pollutant/6001",
         `O3`    ="http://dd.eionet.europa.eu/vocabulary/aq/pollutant/7",
         `NO2`   ="http://dd.eionet.europa.eu/vocabulary/aq/pollutant/8")
}


filter_eea_metadata <- function(all_eea_metadata, Pollutant, Day) {
  all_eea_metadata %>%
    filter(AirPollutantCode==poll2url(Pollutant),
           ObservationDateBegin=="" || as.Date(substr(ObservationDateBegin,1,10))<=Day,
           ObservationDateEnd=="" || as.Date(substr(ObservationDateEnd,1,10))>=Day) %>%
    group_by(AirQualityStationEoICode) %>%
    arrange(desc(nchar(ObservationDateBegin)), .by_group=T) %>%
    slice(1)
}


read_csv_quietly <- function(x) suppressMessages(read_csv(x))


prepare_metadata <- function(Pollutant, Days, all_eea_metadata) {
  eea_metadata <- filter_eea_metadata(all_eea_metadata, Pollutant, Days[1])
  
  bind_rows(list(
    read_csv_quietly("~/R/projects/calicantus/data/sites-info/metadata.ARPA-VdA.csv") %>%
      mutate(Countrycode="IT", Altitude=HEIGHT) %>%
      separate(SITE, c("AirQualityStationArea","AirQualityStationType"), sep = " ", extra="drop", fill="right") %>%
      mutate(AirQualityStationType=translate(str_trim(str_to_lower(AirQualityStationType))),
             AirQualityStationArea=translate(str_trim(str_to_lower(AirQualityStationArea)))) %>%
      harmonize_metadata(.,eea_metadata) %>%
      mutate(Source="ARPA-VdA") %>%
      filter(LocalName %in% available_stations("ARPA-VdA", Days, Pollutant)),
    read_csv_quietly("~/R/projects/calicantus/data/sites-info/metadata.ARPA-Lombardia.csv") %>%
      mutate(Countrycode="IT", Altitude=Height, ID=idStazione) %>%
      harmonize_metadata(.,eea_metadata) %>%
      mutate(Source="ARPA-Lombardia") %>%
      filter(LocalName %in% available_stations("ARPA-Lombardia", Days, Pollutant)),
    read_csv_quietly("~/R/projects/calicantus/data/sites-info/metadata.APPA-Bolzano.csv") %>%
      filter(!is.na(LAT)) %>%
      mutate(Countrycode="IT", ID=SCODE, NAME=NAME_I) %>%
      harmonize_metadata(.,eea_metadata) %>%
      mutate(Source="APPA-Bolzano") %>%
      filter(LocalName %in% available_stations("APPA-Bolzano", Days, Pollutant)),
    read_csv_quietly("~/R/projects/calicantus/data/sites-info/metadata.APPA-Trento.csv") %>%
      mutate(Countrycode="IT", Altitude=Height, AirQualityStationType=StationType,
             AirQualityStationArea=ZoneType, ID=ID_IT, NAME=Name) %>%
      harmonize_metadata(.,eea_metadata)%>%
      mutate(Source="APPA-Trento") %>%
      filter(LocalName %in% available_stations("APPA-Trento", Days, Pollutant)),
    read_csv_quietly("~/R/projects/calicantus/data/sites-info/metadata.ARPA-Lazio.csv") %>%
      mutate(Countrycode="IT", Altitude=as.numeric(zMSL), NAME=NOME) %>%
      separate(TIPO, c("AirQualityStationType","AirQualityStationArea"), sep = " ", extra="drop", fill="right") %>%
      mutate(AirQualityStationType=translate(str_trim(str_to_lower(AirQualityStationType))),
             AirQualityStationArea=translate(str_trim(str_to_lower(AirQualityStationArea)))) %>%
      harmonize_metadata(.,eea_metadata) %>%
      mutate(Source="ARPA-Lazio") %>%
      filter(LocalName %in% available_stations("ARPA-Lazio", Days, Pollutant)),
    read_csv_quietly("~/R/projects/calicantus/data/sites-info/metadata.ARPA-Piemonte.csv") %>%
      mutate(Countrycode="IT",AirQualityStationType=StationType, AirQualityStationArea=ZoneType) %>%
      harmonize_metadata(.,eea_metadata) %>%
      mutate(Source="ARPA-Piemonte") %>%
      filter(LocalName %in% available_stations("ARPA-Piemonte", Days, Pollutant)),
    read_csv_quietly("~/R/projects/calicantus/data/sites-info/metadata.ARPAV.csv") %>%
      mutate(Countrycode="IT",AirQualityStationType=StationType, AirQualityStationArea=ZoneType,
             Altitude=ALT) %>%
      harmonize_metadata(.,eea_metadata) %>%
      mutate(Source="ARPAV") %>%
      filter(LocalName %in% available_stations("ARPAV", Days, Pollutant)),
    read_csv_quietly("~/R/projects/calicantus/data/sites-info/metadata.ARPA-Puglia.csv") %>%
      filter(!is.na(LAT)) %>%
      mutate(Countrycode="IT", NAME=CABINA, ID=as.character(NA),
             AirQualityStationType=translate(str_to_lower(TIPO.STAZIONE)),
             AirQualityStationArea=translate(str_to_lower(TIPO.ZONA))) %>%
      harmonize_metadata(.,eea_metadata) %>%
      mutate(Source="ARPA-Puglia") %>%
      filter(LocalName %in% available_stations("ARPA-Puglia", Days, Pollutant)),
    read_csv_quietly("~/R/projects/calicantus/data/sites-info/metadata.ARPAE.csv") %>%
      filter(!is.na(LAT)) %>%
      mutate(Countrycode="IT",ID=ID_STAZIONE, NAME=NOME_STAZIONE) %>%
      harmonize_metadata(.,eea_metadata) %>%
      mutate(Source="ARPAE") %>%
      filter(LocalName %in% available_stations("ARPAE", Days, Pollutant)),
    read_csv_quietly("~/R/projects/calicantus/data/sites-info/metadata.ARPA-Umbria.csv") %>%
      mutate(Countrycode="IT") %>%
      harmonize_metadata(.,eea_metadata) %>%
      mutate(Source="ARPA-Umbria") %>%
      filter(LocalName %in% available_stations("ARPA-Umbria", Days, Pollutant)),
    read_csv_quietly("~/R/projects/calicantus/data/sites-info/metadata.ARPA-Liguria.csv") %>%
      mutate(Countrycode="IT",AirQualityStationType=StationType, AirQualityStationArea=ZoneType) %>%
      harmonize_metadata(.,eea_metadata) %>%
      mutate(Source="ARPA-Liguria") %>%
      filter(LocalName %in% available_stations("ARPA-Liguria", Days, Pollutant)),
    read_csv_quietly("~/R/projects/calicantus/data/sites-info/metadata.ARPAB.csv") %>%
      mutate(Countrycode="IT") %>%
      harmonize_metadata(.,eea_metadata) %>%
      mutate(Source="ARPAB") %>%
      filter(LocalName %in% available_stations("ARPAB", Days, Pollutant)),
    read_csv_quietly("~/R/projects/calicantus/data/sites-info/metadata.ARPAC.csv") %>%
      mutate(Countrycode="IT") %>%
      harmonize_metadata(.,eea_metadata) %>%
      mutate(Source="ARPAC") %>%
      filter(LocalName %in% available_stations("ARPAC", Days, Pollutant)),
    read_csv_quietly("~/R/projects/calicantus/data/sites-info/metadata.ARPA-FVG.csv") %>%
      mutate(Countrycode="IT") %>%
      harmonize_metadata(.,eea_metadata) %>%
      mutate(Source="ARPA-FVG") %>%
      filter(LocalName %in% available_stations("ARPA-FVG", Days, Pollutant)),
    read_csv_quietly("~/R/projects/calicantus/data/sites-info/metadata.ARPA-Sicilia.csv") %>%
      mutate(Countrycode="IT") %>%
      harmonize_metadata(.,eea_metadata) %>%
      mutate(Source="ARPA-Sicilia") %>%
      filter(LocalName %in% available_stations("ARPA-Sicilia", Days, Pollutant)),
    read_csv_quietly("~/R/projects/calicantus/data/sites-info/metadata.ARPAT.csv") %>%
      mutate(Countrycode="IT", ID=COD_STAZIONE, NAME=NOME_STAZIONE,
             AirQualityStationType=translate(str_to_lower(TIPO_STAZIONE)),
             AirQualityStationArea=translate(str_to_lower(TIPO_ZONA))) %>%
      harmonize_metadata(.,eea_metadata) %>%
      mutate(Source="ARPAT") %>%
      filter(LocalName %in% available_stations("ARPAT", Days, Pollutant))
  )
  ) 
}


for (poll in c("PM10", "PM2.5", "NO2", "O3")) {
  futile.logger::flog.info(paste0("Processing ",poll))
  new_metadata <- prepare_metadata(Pollutant = poll, Days = days2check, all_eea_metadata = all_eea_metadata)
  for(cc in c("IT")) {
    write.csv(new_metadata %>% 
                dplyr::filter(Countrycode==cc) %>%
                mutate_if(is.double, round, 5), 
              file = paste0("metadata_",cc,"_",poll,".csv"),
              row.names = F, quote=T)
  }
}
