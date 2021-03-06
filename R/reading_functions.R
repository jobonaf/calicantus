
read.ArpaV <- function(file,sep=";",skip=0) {
  hdr <- scan(file,sep=sep,nlines=1,skip=skip,what="")
  hdr <- hdr[-length(hdr)]
  Encoding(hdr) <- "latin1"
  dat <- as.numeric(unlist(strsplit(scan(file,what="",skip=1+skip)[2],split=sep)))
  out <- data.frame(NAME=hdr,PM10=dat)
  return(out)
}


read.ArpaT <- function(file,day) {
  cat(paste0("file=",file,"\n"))
  cat(paste0("day=",day,"\n"))
  day <- as.Date(day,tz="Africa/Algiers")
  Day <- paste(format(day,"%d"),
               c("JAN","FEB","MAR",
                 "APR","MAY","JUN",
                 "JUL","AUG","SEP",
                 "OCT","NOV","DEC")[as.numeric(format(day,"%m"))],
               format(day,"%y"),sep="-")
  library("RJSONIO")
  dat <- RJSONIO::fromJSON(file)
  as.data.frame(dat)->dat
  dat <- t(dat)
  rownames(dat)<-rep(NULL,nrow(dat))
  dat <- as.data.frame(dat)
  dat$DATA_OSSERVAZIONE <- as.character(dat$DATA_OSSERVAZIONE)
  dat$NOME_STAZIONE     <- as.character(dat$NOME_STAZIONE)
  dat$COMUNE            <- as.character(dat$COMUNE)
  dat$PM10 <- as.numeric(gsub('\\*','',as.character(dat$PM10)))
  cat(paste0("seleziono per ArpaT il giorno ",Day,"\n"))
  dat <- dat[which(dat$DATA_OSSERVAZIONE==Day),]
  return(dat)
}

read.ArpaLazio <- function(file,sep,day,stat=NULL) {
  library(dplyr)
  dat <- NULL
  for(ff in file) {
    dd <- try(read.table(ff,head=T,check.names = F,stringsAsFactors = FALSE))
    if(class(dd)!="try-error") {
      dd[dd<0] <- NA
      dd$anno <- NULL
      if(!is.null(stat) && stat=="max") {
        Max <- function(x,nreq) ifelse(sum(!is.na(x))>=nreq,max(x,na.rm=T),NA)
        dd %>% dplyr::select(-h) %>% dplyr::group_by(jd) %>% dplyr::summarize_all(Max,nreq=18) -> dd
      }
      if(is.null(dat)) {
        dat<-dd
      } else {
        dat <- merge(x=dat,y=dd,by=c("jd"),all=T)
      }
    }
  }
  idx <- which(dat$jd==(as.POSIXlt(day)$yday+1))
  out <- data.frame(ID=as.character(colnames(dat)[-1:-2]), 
                    Valore=c(unlist(dat[idx[1],][-1:-2])),
                    stringsAsFactors = F)
  return(out)
}


read.ArpaPuglia <- function(file,sep,day) {
  dat <- read.table(file,sep = sep,blank.lines.skip = T,fill = T,stringsAsFactors = F)
  dat[,1] <- stringr::str_trim(dat[,1])
  dat[,colSums(!is.na(dat))==0] <- NULL
  hdr <- dat[as.character(dat[,1])=="NomeCentralina",][1,]
  colnames(dat) <- stringr::str_trim(as.character(hdr))
  dat$Valore <- as.numeric(as.character(dat$Valore))
  out <- dat[which(!is.na(dat$Valore)),]
  return(out)
}


round_awayfromzero <- function(x,digits=0) trunc(x*10^digits+sign(x)*0.5)*10^-digits

read.ArpaCampania <- function(file,sep,day,poll,stat) {
  #library("Rcpp", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
  library(dplyr)
  library(caTools)
  dat <- read.table(file,sep = sep,stringsAsFactors = F,header = T)
  dat %>% mutate(data=substr(data_ora,1,10)) %>%
    filter(as.POSIXct(data,tz="Africa/Algiers")==
             as.POSIXct(day,tz="Africa/Algiers"),
           inquinante==poll,
           !is.na(valore)) %>% 
    group_by(stazione, data) -> dat
  if(stat=="mean") {
    dat %>% summarize(valore=round_awayfromzero(mean(valore)), nh=n()) %>%
      filter(nh>=18) -> out
  }
  if(stat=="max") {
    dat %>% summarize(valore=round_awayfromzero(max(valore)), nh=n()) %>%
      filter(nh>=18) -> out
  }
  out <- as.data.frame(out)
  return(out)
}


read.AzoCroatia <- function(file,IDs, stat=NULL) {
  library(RJSONIO)
  out <- data.frame(ID=IDs, VAL=NA)
  nf <- length(file)
  if(nf!=length(IDs)) stop("IDs and files must have the same number of elements!")
  for (i in 1:nf) {
    tmp <- RJSONIO::fromJSON(file[i])
    if(length(tmp)>0) {
      if(is.null(stat)) {
        out$VAL[i] <- tmp[[1]]$vrijednost
      }else if(stat=="max") {
        mm <- max(unlist(lapply(tmp, function(x) {x$vrijednost})), na.rm=T)
        if(mm>=0) out$VAL[i] <- mm
      }
    }
  }
  return(out)
}

read.AppaTrento <- function(file,IDs, stat, sep) {
  out <- data.frame(ID=IDs, VAL=NA)
  nf <- length(file)
  if(nf!=length(IDs)) stop("IDs and files must have the same number of elements!")
  for (i in 1:nf) {
    tmp <- try(read.table(file[i], sep=sep, header=T, fileEncoding  = "Latin1"))
    if(class(tmp)!="try-error" && length(tmp)>0) {
      if(stat=="mean") {
        out$VAL[i] <- ifelse(length(tmp$Valore)>=18,mean(tmp$Valore),NA)
      }else if(stat=="max") {
        out$VAL[i] <- max(tmp$Valore)
      }
    }
  }
  return(out)
}

read.UacerTicino <- function(file,IDs, stat=NULL) {
  out <- data.frame(ID=IDs, VAL=NA)
  nf <- length(file)
  if(nf!=length(IDs)) stop("IDs and files must have the same number of elements!")
  for (i in 1:nf) {
    tmp <- read.table(file[i],comment.char = "#", blank.lines.skip = T, header = T, sep=";")
    if(length(tmp)>0 && ncol(tmp)>1) {
      if(is.null(stat)) {
        out$VAL[i] <- tmp[1,2]
      }else if(stat=="max") {
        h <- format(strptime(tmp[,1],"%d.%m.%Y %H:%M"),"%H")
        mm <- try(max(tapply(tmp[,2],h,mean,na.rm=T),na.rm=T))
        if(class(mm)[1]=="try-error" && mm>=0) {
          out$VAL[i] <- mm
        }else{
          out$VAL[i] <- NA
        }
      }
    }
  }
  return(out)
}

read.SepaSerbia <- function(file,sep,day) { 
  library(XML)
  dat <- readHTMLTable(file, colClasses = c("character",rep("numeric",length(sep))))[[1]]
  colnames(dat) <- c("date",sep)
  dat <- dat[as.Date(dat$date)==as.Date(day),-1]
  ave <- colMeans(dat, na.rm = T)
  valid <- colSums(!is.na(dat))
  ave[valid<24*0.75] <- NA
  out <- data.frame(ID=sep, VAL=ave)
  return(out)
}

read.ArpaER <- function(file, stat) {
  library(RJSONIO)
  library(dplyr)
  dat <- RJSONIO::fromJSON(file)
  dat <- do.call("bind_rows", dat$result$records)
  if(stat=="max" & nrow(dat)>0) {
    dat %>% group_by(station_id) %>%
      summarize(value=max(value,na.rm=T),
                ndata=n()) -> dat
    dat$value[dat$ndata<18] <- NA
  }
  dat <- as.data.frame(dat)
  return(dat)
}

read.EEA <- function(file, poll, day, sep=",",
                     namespace="AT.0008.20.AQ",             # see unique(eea_get_metadata()$Namespace)
                     data_source="Umweltbundesamt-Austria", # convenient name, corresponding to network_namespace
                     filein_metadata="/home/giovanni/R/projects/calicantus/run/eea/eea_metadata.csv") {
  library(data.table)
  library(dplyr)
  
  ## read metadata
  fread(filein_metadata, encoding = "Latin-1") %>%
    filter(Namespace==namespace) %>%
    select(Countrycode,Namespace,AirQualityStationEoICode,
           Longitude,Latitude,Altitude,AirQualityStationType,
           AirQualityStationArea,BuildingDistance,KerbDistance) %>%
    group_by(Countrycode,Namespace,AirQualityStationEoICode) %>%
    summarise(LON=round(mean(Longitude,na.rm=T),6),
              LAT=round(mean(Latitude,na.rm=T),6),
              Altitude=round(mean(Altitude,na.rm=T)),
              AirQualityStationType=paste(unique(AirQualityStationType),collapse = "/"),
              AirQualityStationArea=paste(unique(AirQualityStationArea),collapse = "/"),
              BuildingDistance=mean(BuildingDistance,na.rm=T),
              KerbDistance=mean(KerbDistance,na.rm=T)) -> ana
  
  ## read data
  fread(file,sep=sep, encoding = "Latin-1") %>%
    filter(network_namespace==namespace,
           pollutant==poll,
           as.Date(as.POSIXct(value_datetime_begin))==as.Date(day),
           value_validity>0) %>%
    select(network_countrycode,network_name,network_namespace,
           network_timezone,pollutant,station_code,station_name,
           value_datetime_begin,value_datetime_end,
           value_datetime_inserted,value_datetime_updated,
           value_numeric,value_validity,value_verification,value_unit) %>%
    arrange(value_datetime_updated, value_datetime_inserted) %>%
    group_by(network_namespace,station_code,value_datetime_begin) %>%
    summarise_all(last) %>%
    filter(!is.na(value_datetime_end)) %>%
    mutate(delta = difftime(as.POSIXct(value_datetime_end),
                            as.POSIXct(value_datetime_begin),
                            units="hours"),
           day = format(as.POSIXct(value_datetime_begin),"%Y-%m-%d")) %>%
    filter(!is.na(delta)) %>%
    group_by(network_namespace,station_code, day) %>%
    summarise(coverage = sum(delta),
              station_name = last(station_name),
              mean = mean(value_numeric, na.rm=T),
              max = max(value_numeric, na.rm=T)) %>%
    filter(coverage >= 18, coverage <= 24) %>%
    select(-coverage) %>%
    ungroup() -> dat
  
  ## expand metadata: station name
  dat %>% select(station_code,station_name) %>% distinct() -> ana2
  merge(ana,ana2,by.x="AirQualityStationEoICode",by.y="station_code") -> ana
  fileout_metadata <- paste0("/home/giovanni/R/projects/calicantus/data/sites-info/metadata.",
                             data_source,".csv")
  if(file.exists(fileout_metadata)) {
    oldana <- fread(fileout_metadata, encoding = "Latin-1")
    bind_rows(ana,oldana) %>% 
      distinct(Namespace,AirQualityStationEoICode, .keep_all=TRUE) -> ana
  }
  
  ## write metadata
  fwrite(x = ana, file = fileout_metadata)
  
  return(data.frame(dat))
}


read.ArpaFVG <- function(file,sep,day,stat) {
  dat <- read.csv(file=file,sep=sep,header=T, stringsAsFactors = F)
  library(dplyr)
  dat %>% filter(day==substr(Time,1,10)) -> dat
  if(!is.null(stat) && stat=="max") dat %>% group_by(Station) %>% 
    summarize(Value=max(Value,na.rm=T)) -> dat
  as.data.frame(dat)
}


read.ArpaT_OpenData <- function(file,poll) {
  library("RJSONIO")
  dat <- RJSONIO::fromJSON(file,simplify = T)
  as.data.frame(dat, stringsAsFactors = F)->dat
  dat <- t(dat)
  rownames(dat)<-rep(NULL,nrow(dat))
  dat <- as.data.frame(dat, stringsAsFactors = F)
  
  library(dplyr)
  dat %>% 
    transmute_(Day="DATA_OSSERVAZIONE",
               Value=gsub("\\.", "dot", poll),
               Name="NOME_STAZIONE") %>%
    mutate(Value=as.numeric(Value),
           Day=as.Date(Day, format = "%d-%b-%y")) %>%
    filter(!is.na(Value))-> dat
  return(dat)
}


## qui e nel download devo usare RSocrata, se no c'è un limite a 1000 righe!!
read.ArpaLombardia <- function(file,poll,stat,token,path,
                               file_ana="https://www.dati.lombardia.it/resource/t4f9-i4k5.json") {
  library("RSocrata")
  ana <- read.socrata(url = file_ana, app_token = token)
  cat(paste(path,file,sep="/"), sep="\n")
  dat <- read.socrata(url = paste(path,file,sep="/"), app_token = token)
  
  library(dplyr)
  Dat <- left_join(dat,ana) %>% 
    mutate(valore=as.numeric(valore))
  
  NomeTipoSensore <- switch(poll,
                            "PM10"="PM10 (SM2005)",
                            "NO2"="Biossido di Azoto",
                            "PM2.5"="Particelle sospese PM2.5",
                            "O3"="Ozono")
  
  Dat %>% filter(nometiposensore==NomeTipoSensore,
                 stato=="VA",
                 valore>=0) -> Dat
  if(stat=="max")  Dat %>% 
    dplyr::group_by(idstazione) %>%
    dplyr::summarize(valore=max(valore,na.rm=T),
              valid=n()) %>%
    filter(valid>=18) -> Dat
  as.data.frame(Dat)
}

get_metadata.ArpaLombardia <- function(file_ana="https://www.dati.lombardia.it/resource/t4f9-i4k5.json") {
  library("RSocrata")
  source("/home/giovanni/R/projects/calicantus/config/obs-data-access/access.ARPA-Lombardia.O3.R", local = T)
  ana <- read.socrata(url = file_ana, app_token = Pwd)
  library(dplyr)
  ana %>% transmute(idStazione=idstazione,
                    LAT=as.numeric(lat),
                    LON=as.numeric(lng),
                    Height=as.numeric(quota),
                    NAME=gsub(" \\$","",nomestazione)) %>%
    distinct()
}


get_metadata.AppaBolzano <- function(fix=F) {
  ana <- RJSONIO::fromJSON("http://dati.retecivica.bz.it/services/airquality/stations")$features
  ana <- lapply(X = ana, 
                FUN = function(x) sapply(X = x$properties, 
                                         FUN = function(x) ifelse(is.null(x),NA,x)))
  as.data.frame(ana, stringsAsFactors = F)->ana
  ana <- t(ana)
  rownames(ana)<-rep(NULL,nrow(ana))
  ana <- as.data.frame(ana, stringsAsFactors = F)
  library(dplyr)
  ana %>% mutate(LON=as.numeric(LONG),
                 LAT=as.numeric(LAT)) %>%
    select(-LONG) -> ana
  sx2dc <- function(x) {
    x <- x*10000
    s <- x %% 100
    x <- (x-s) /100
    m <- x %% 100
    x <- (x-m) /100
    x + (m/60) + (s/3600)
  }
  if(fix) ana %>% mutate(LON=sx2dc(LON), 
                         LAT=sx2dc(LAT)) -> ana
  ana
}

read.AppaBolzano <- function(file, day) {
  dat <-  RJSONIO::fromJSON(file, simplify = T)
  as.data.frame(dat, stringsAsFactors = F)->dat
  dat <- t(dat)
  rownames(dat)<-rep(NULL,nrow(dat))
  dat <- as.data.frame(dat, stringsAsFactors = F)
  
  library(dplyr)
  dat %>% transmute(Value=as.numeric(VALUE),
                    Day=substring(DATE,1,10),
                    SCODE=SCODE) %>%
    filter(Value>=0, Day==day)
}

read.ArpaVda <- function(file,sep,stat) {
  library(tidyr)
  library(dplyr)
  read.table(file,sep=sep,header=T) %>%
    gather(key=ID,value=Value,-date) %>%
    mutate(Value=as.numeric(as.character(Value)),
           date=as.POSIXct(date),
           day=format(date,format="%Y%m%d")) -> dat
  out <- NULL
  if(stat=="mean") dat %>% group_by(day,ID) %>% summarize(Value=mean(Value, na.rm=T)) -> out
  if(stat=="max")  dat %>% group_by(day,ID) %>% summarize(Value=max(Value, na.rm=T)) -> out
  return(as.data.frame(out))
}
