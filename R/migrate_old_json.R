library(sp)
library(maptools)
library(rgdal)
regioni  <- readShapePoly("~/util/geodata/reg2011_g")
proj4string(obj=regioni)  <- CRS("+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0")
regioni <- spTransform(regioni,CRS("+proj=longlat +datum=WGS84"))

ll2source <- function(lat,lon) {
  pp <- data.frame(lon,lat)
  coordinates(pp) <- ~lon+lat
  proj4string(obj=pp)  <- CRS("+proj=longlat +datum=WGS84")
  reg <- as.character((pp %over% regioni)$COD_REG)
  out <- lapply(reg,
                function(x) switch(as.character(x),
                                   "1"="ARPA-Piemonte",
                                   "3"="ARPA-Lombardia",
                                   "5"="ARPAV",
                                   "6"="ARPA-FVG",
                                   "7"="ARPA-Liguria",
                                   "8"="ARPAE",
                                   "9"="ARPAT",
                                   "10"="ARPA-Umbria",
                                   "19"="ARPA-Sicilia",
                                   NA))
  unlist(out)
}

read.json.single <- function(date,poll="PM10") {
  #library("RJSONIO", lib.loc="/usr/local/lib/R/site-library")
  library("RJSONIO")
  date <- as.POSIXct(date)
  filein <-  paste("~/R/projects/map-po/maps/",format(date,"%Y-%m-%d"),
                   "/",poll,"_",format(date,"%Y%m%d"),".geojson",sep="") 
  out <- try(fromJSON(filein))
  return(out)
}

read.json.multi <- function(first=as.Date("2014-10-10"),last=as.Date("2016-12-12"),poll="PM10") {
  library(dplyr)
  first <- as.POSIXct(first, tz="UTC")
  last <- as.POSIXct(last, tz="UTC")
  days <- seq.POSIXt(from=first,to=last,by="1 days")
  nd <- length(days)
  Data <- NULL
  for(id in 1:nd) {
    date <- days[id]
    print(date)
    dum <- read.json.single(date,poll=poll)
    ns <- length(dum[[2]])
    records <- NULL
    for(iS in 1:ns) {
      record <- data.frame(
        Name=dum[[2]][[iS]]$properties[1],
        Lon=as.numeric(dum[[2]][[iS]]$geometry$coordinates[1]),
        Lat=as.numeric(dum[[2]][[iS]]$geometry$coordinates[2]),
        Value=as.numeric(dum[[2]][[iS]]$properties[2]),
        Day=as.Date(date))
      records <- bind_rows(records,record)
    }
    #rownames(records) <- rep("",nrow(records))
    #if(flag) {
    #  Data <- records; flag=FALSE
    #} else {
      Data <- bind_rows(Data, records)
    #}
  }
  #colnames(Data) <- c("Lon","Lat","Name","Value","Day")
  Data <- data.frame(Name=Data$Name, Lat=Data$Lat, Lon=Data$Lon,
                     Value=Data$Value, Pollutant=poll, Day=as.character(Data$Day))
  return(Data)
}

expand_dataset <- function(Data) {
  ll <- unique(Data[,c("Lat","Lon")])
  extra <- data.frame(Lat=ll$Lat, Lon=ll$Lon, Source=ll2source(ll$Lat,ll$Lon))
  Data <- merge(Data,extra)
  data.frame(Data, Sys_time=Sys.time())
}

save_to_multiple_rda <- function(Data,path="/home/giovanni/R/projects/calicantus/",pollutant="PM10") {
  library(dplyr)
  for(day in unique(Data$Day)) {
    Data %>% filter(Day==day & Pollutant==pollutant) -> Dat
    for(ss in unique(Dat$Source)) {
      Dat %>% filter(Source==ss) -> dat
      if(length(dat)>0) {
        day <- as.POSIXct(day,tz="Africa/Algiers")
        Dir <- paste0(path,"data/obs-data/",format(day,"%Y/%m/%d/"))
        if(!dir.exists(Dir)) dir.create(Dir,recursive=TRUE)
        fileout <- paste0(path,"data/obs-data/",format(day,"%Y/%m/%d/%Y%m%d_"),pollutant,"_",ss,".rda")
        save(dat, file = fileout)
        cat(paste0("saved ",fileout),sep="\n")
      }
      
    }
  }
}

ddd <- read.json.multi(first=as.Date("2016-11-03"),last=as.Date("2016-11-03"),poll="PM10")
eee <- expand_dataset(ddd)
save_to_multiple_rda(eee)