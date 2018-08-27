library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(maptools)
library(sp)

source('~/R/projects/calicantus/R/util.R')
ref_date <- Sys.Date()-1
nn <- sort(sample(x = 1:450, size = 20, replace = F), decreasing = T)
days2check <- c(ref_date - nn)
eea_get_metadata() -> all_eea_metadata
nuts <- readShapePoly("~/data/NUTS2016_WGS84_LatLong/NUTS_RG_01M_2016_4326_LEVL_3")


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


active_stations <- function(Source, Days, Pollutant) {
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
  if(!is.null(Dat)) Dat <- Dat %>% 
    dplyr::filter(!is.na(Name), !is.na(Value))  %>% 
    select(Name, Lat, Lon)  %>% 
    distinct()
  futile.logger::flog.info(paste0(Pollutant,", ",Source,": found ",nrow(Dat)," active stations in days ",paste(Days,collapse=", ")))
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

mark_active_stations <- function(.x, Source, Days, Pollutant) {
  avst <- active_stations(Source, Days, Pollutant)
  .x %>% mutate(Active= LocalName %in% avst$Name & 
                  paste(Lat,Lon) %in% paste(avst$Lat,avst$Lon))
}


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
      mark_active_stations("ARPA-VdA", Days, Pollutant),
    read_csv_quietly("~/R/projects/calicantus/data/sites-info/metadata.ARPA-Lombardia.csv") %>%
      mutate(Countrycode="IT", Altitude=Height, ID=idStazione) %>%
      harmonize_metadata(.,eea_metadata) %>%
      mutate(Source="ARPA-Lombardia") %>%
      mark_active_stations("ARPA-Lombardia", Days, Pollutant),
    read_csv_quietly("~/R/projects/calicantus/data/sites-info/metadata.APPA-Bolzano.csv") %>%
      filter(!is.na(LAT)) %>%
      mutate(Countrycode="IT", ID=SCODE, NAME=NAME_I) %>%
      harmonize_metadata(.,eea_metadata) %>%
      mutate(Source="APPA-Bolzano") %>%
      mark_active_stations("APPA-Bolzano", Days, Pollutant),
    read_csv_quietly("~/R/projects/calicantus/data/sites-info/metadata.APPA-Trento.csv") %>%
      mutate(Countrycode="IT", Altitude=Height, AirQualityStationType=StationType,
             AirQualityStationArea=ZoneType, ID=ID_IT, NAME=Name) %>%
      harmonize_metadata(.,eea_metadata)%>%
      mutate(Source="APPA-Trento") %>%
      mark_active_stations("APPA-Trento", Days, Pollutant),
    read_csv_quietly("~/R/projects/calicantus/data/sites-info/metadata.ARPA-Lazio.csv") %>%
      mutate(Countrycode="IT", Altitude=as.numeric(zMSL), NAME=NOME) %>%
      separate(TIPO, c("AirQualityStationType","AirQualityStationArea"), sep = " ", extra="drop", fill="right") %>%
      mutate(AirQualityStationType=translate(str_trim(str_to_lower(AirQualityStationType))),
             AirQualityStationArea=translate(str_trim(str_to_lower(AirQualityStationArea)))) %>%
      harmonize_metadata(.,eea_metadata) %>%
      mutate(Source="ARPA-Lazio") %>%
      mark_active_stations("ARPA-Lazio", Days, Pollutant),
    read_csv_quietly("~/R/projects/calicantus/data/sites-info/metadata.ARPA-Piemonte.csv") %>%
      mutate(Countrycode="IT",AirQualityStationType=StationType, AirQualityStationArea=ZoneType) %>%
      harmonize_metadata(.,eea_metadata) %>%
      mutate(Source="ARPA-Piemonte") %>%
      mark_active_stations("ARPA-Piemonte", Days, Pollutant),
    read_csv_quietly("~/R/projects/calicantus/data/sites-info/metadata.ARPAV.csv") %>%
      mutate(Countrycode="IT",AirQualityStationType=StationType, AirQualityStationArea=ZoneType,
             Altitude=ALT) %>%
      harmonize_metadata(.,eea_metadata) %>%
      mutate(Source="ARPAV") %>%
      mark_active_stations("ARPAV", Days, Pollutant),
    read_csv_quietly("~/R/projects/calicantus/data/sites-info/metadata.ARPA-Puglia.csv") %>%
      filter(!is.na(LAT)) %>%
      mutate(Countrycode="IT", NAME=CABINA, ID=as.character(NA),
             AirQualityStationType=translate(str_to_lower(TIPO.STAZIONE)),
             AirQualityStationArea=translate(str_to_lower(TIPO.ZONA))) %>%
      harmonize_metadata(.,eea_metadata) %>%
      mutate(Source="ARPA-Puglia") %>%
      mark_active_stations("ARPA-Puglia", Days, Pollutant),
    read_csv_quietly("~/R/projects/calicantus/data/sites-info/metadata.ARPAE.csv") %>%
      filter(!is.na(LAT)) %>%
      mutate(Countrycode="IT",ID=ID_STAZIONE, NAME=NOME_STAZIONE) %>%
      harmonize_metadata(.,eea_metadata) %>%
      mutate(Source="ARPAE") %>%
      mark_active_stations("ARPAE", Days, Pollutant),
    read_csv_quietly("~/R/projects/calicantus/data/sites-info/metadata.ARPA-Umbria.csv") %>%
      mutate(Countrycode="IT") %>%
      harmonize_metadata(.,eea_metadata) %>%
      mutate(Source="ARPA-Umbria") %>%
      mark_active_stations("ARPA-Umbria", Days, Pollutant),
    read_csv_quietly("~/R/projects/calicantus/data/sites-info/metadata.ARPA-Liguria.csv") %>%
      mutate(Countrycode="IT", AirQualityStationType=StationType, AirQualityStationArea=ZoneType) %>%
      harmonize_metadata(.,eea_metadata) %>%
      mutate(Source="ARPA-Liguria") %>%
      mark_active_stations("ARPA-Liguria", Days, Pollutant),
    read_csv_quietly("~/R/projects/calicantus/data/sites-info/metadata.ARPAB.csv") %>%
      mutate(Countrycode="IT") %>%
      harmonize_metadata(.,eea_metadata) %>%
      mutate(Source="ARPAB") %>%
      mark_active_stations("ARPAB", Days, Pollutant),
    read_csv_quietly("~/R/projects/calicantus/data/sites-info/metadata.ARPAC.csv") %>%
      mutate(Countrycode="IT") %>%
      harmonize_metadata(.,eea_metadata) %>%
      mutate(Source="ARPAC") %>%
      mark_active_stations("ARPAC", Days, Pollutant),
    read_csv_quietly("~/R/projects/calicantus/data/sites-info/metadata.ARPA-FVG.csv") %>%
      mutate(Countrycode="IT", AirQualityStationType=str_to_lower(StationType),
             AirQualityStationArea=str_to_lower(ZoneType)) %>%
      harmonize_metadata(.,eea_metadata) %>%
      mutate(Source="ARPA-FVG") %>%
      mark_active_stations("ARPA-FVG", Days, Pollutant),
    read_csv_quietly("~/R/projects/calicantus/data/sites-info/metadata.ARPA-Sicilia.csv") %>%
      mutate(Countrycode="IT") %>%
      harmonize_metadata(.,eea_metadata) %>%
      mutate(Source="ARPA-Sicilia") %>%
      mark_active_stations("ARPA-Sicilia", Days, Pollutant),
    read_csv_quietly("~/R/projects/calicantus/data/sites-info/metadata.ARPAT.csv") %>%
      mutate(Countrycode="IT", ID=COD_STAZIONE, NAME=NOME_STAZIONE,
             AirQualityStationType=translate(str_to_lower(TIPO_STAZIONE)),
             AirQualityStationArea=translate(str_to_lower(TIPO_ZONA))) %>%
      harmonize_metadata(.,eea_metadata) %>%
      mutate(Source="ARPAT") %>%
      mark_active_stations("ARPAT", Days, Pollutant)
  )
  ) 
}

abbr <- function(x, nc) {
  abbreviate(str_to_title(str_replace_all(iconv(x, to='ASCII//TRANSLIT'), 
                                          "[^[:alpha:] ]", " ")),
             minlength = nc) %>% 
    combine() %>% 
    str_replace_all(" ","_") %>% 
    str_replace_all("__","_") %>% 
    str_pad(nc,side = "right",pad="_") %>%
    str_trunc(nc, ellipsis="")
}

coord2nuts <- function(Lat, Lon, nuts=nuts) {
  library(rgeos)
  pp <- data.frame(lon=Lon, lat=Lat)
  coordinates(pp) <- ~lon+lat
  n0 <- over(pp,nuts)
  n1 <- over(gBuffer(pp, byid = T, width = 0.05), nuts)
  nn <- n0
  nn[which(is.na(n0$CNTR_CODE)),] <- n1[which(is.na(n0$CNTR_CODE)),]
  return(nn)
}


# distinguish <- function(.x) {
#   data.frame(x0=x) %>%
#     mutate(x1=str_trunc(as.character(x0),
#               width = str_length(x0)-digits,
#               ellipsis = "")) %>%
#     group_by(x0) %>% mutate(n0=n()) %>% ungroup() %>% 
#     group_by(x1) %>% mutate(n1=n(), i1=sequence(n())-1) %>% ungroup() %>% 
#     mutate(x2=ifelse(n0>1,
#                      paste0(x1,sprintf(paste0("%0",digits,"i"),i1)),
#                      x0)) %>%
#     ungroup() %>%
#     select(x2) %>% combine()
# }


to_base <- function(x, base) {
  ch <- c(0:9,LETTERS,letters)
  if(x%%1!=0 || x<0 || base<2 || base>length(ch) || base%%1!=0) futile.logger::flog.error(paste0("to_base(",x,",",base,")"))
  out <- NULL
  while(x>0) {
    out <- paste0(ch[x%%base + 1], out)
    x <- x%/%base
  }
  out
}
To_base <- function(X,base=62) sapply(X, to_base, base=base, simplify = T)

from_base <- function(x, base) {
  ch <- c(0:9,LETTERS,letters)
  if(x%%1!=0 || x<0 || base<2 || base>length(ch) || base%%1!=0) futile.logger::flog.error(paste0("from_base(",x,",",base,")"))
  sum((charmatch(strsplit(x,"")[[1]], ch) - 1) * base^((nchar(x)-1):0))
}
From_base <- function(X,base=62) sapply(X, from_base, base=base, simplify = T)


station2code <- function(Lat, Lon, nuts=nuts) {
  library(stringr)
  nn <- coord2nuts(Lat, Lon, nuts)
  paste0(nn$NUTS_ID,
         "_",
         str_pad(To_base(round(Lon*10000),62), 4, "left", pad = "0"), 
         str_pad(To_base(round(Lat*10000),62), 4, "left", pad = "0"), 
         ".")
}


new_metadata <- NULL
for (poll in c("PM10", "PM2.5", "NO2", "O3")) {
  futile.logger::flog.info(paste0("Processing ",poll))
  md <- prepare_metadata(Pollutant = poll, 
                                   Days = days2check, 
                                   all_eea_metadata = all_eea_metadata)%>% 
    mutate(SiteCode=station2code(Lat, Lon, nuts),
           Pollutant=poll) %>% 
    select(SiteCode, everything())
  new_metadata <- bind_rows(new_metadata, md)
}

# avoid duplications in SiteCode
library(dplyr)
new_metadata %>%
  group_by(SiteCode, Pollutant) %>%
  arrange(desc(Active), .by_group=T) %>%
  mutate(n=n(),i=sequence(n())) %>% ungroup() %>%
  mutate(SiteCode=ifelse(n>1 & i>1,
                         str_replace(SiteCode,"[.]",sprintf("%i",i-1)),
                         SiteCode)) %>%
  select(-n, -i) -> new_metadata

for (poll in c("PM10", "PM2.5", "NO2", "O3")) {
  for(cc in c("IT")) {
    write.csv(new_metadata %>% 
                dplyr::filter(Countrycode==cc,
                              Pollutant==poll,
                              Active==TRUE) %>%
                select(-Pollutant, -Active, -Eea.Lat, -Eea.Lon) %>%
                mutate_if(is.double, round, 5), 
              file = paste0("metadata_",cc,"_",poll,".csv"),
              row.names = F, quote=T)
  }
}
