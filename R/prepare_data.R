source("/home/giovanni/R/projects/calicantus/R/reading_functions.R")
source("/home/giovanni/R/projects/calicantus/R/get_data.R")
library(readr)

read_data <- function(config,filedata,metadata,day,pollutant,
                      clean=!interactive(),verbose=F){
  source(config)
  cat(paste0("extracting data; source: ",Source,", day: ",day,"\n"))
  check <- any(nchar(filedata)>0)
  if(check) {
    dat <- NULL
    if(verbose) cat(paste0("reading file[s] ",paste(filedata,collapse=", ")), sep="\n")
    if(!select.by.day) dat <- try(readFun(file = filedata, sep=Sep))
    if(select.by.day) dat <- try(readFun(file = filedata, sep=Sep, day=day))
    check <- class(dat)[1]!="try-error"
    if(check) check <- nrow(dat)>0
  }
  if(check) {
    if(verbose) cat(paste0("file[s] read: ",paste(filedata,collapse=", ")), sep="\n")
#    ana <- read.csv(metadata, stringsAsFactors = FALSE)
    ana <- as.data.frame(read_csv(metadata, 
                                  locale = locale(encoding = guess_encoding(metadata)$encoding[1]),
                                  col_types = cols(LAT = "d",
                                                   LON = "d",
                                                   .default = "c")))
    id.dat <- match(as.character(ana[,Id.metadata]),as.character(dat[,Id.data]))
    id.ana <- which(!is.na(id.dat))
    check <- length(id.ana)>0
  } else {
    if(verbose) cat(paste0("problem reading file[s] ",paste(filedata,collapse=", ")), sep="\n")
  }
  if(check) {
    if(verbose) cat(paste0("file read: ",metadata), sep="\n")
    id.dat <- na.omit(id.dat)
    value <- as.numeric(as.character(dat[id.dat,Val.data]))
    lat <- ana$LAT[id.ana]
    lon <- ana$LON[id.ana]
    name <- ana[id.ana,Name.metadata]
    value[value<0] <- NA
    if(!is.null(Flag.data)) {
      Flag <- as.character(dat[id.dat,Flag.data])
      value[Flag%in%invalFlag] <- NA
    }
    if(verbose) cat(paste0("'Dat' data.frame: ",
                           "\nName:",  paste(name,collapse=","),
                           "\nLat:",   paste(lat,collapse=","),
                           "\nLon:",   paste(lon,collapse=","),
                           "\nValue:", paste(value,collapse=","),
                           "\nPollutant:",paste(pollutant,collapse=","),
                           "\nDay:",   paste(day,collapse=","),
                           "\nSource:",paste(Source,collapse=",")), sep="\n")
    Dat <- data.frame(Name=name,Lat=lat,Lon=lon,Value=value,
                      Pollutant=pollutant,Day=day,Source=Source)
  } else {
    if(verbose) cat(paste0("problem reading: ",metadata), sep="\n")
    Dat <- NULL
  }  
  if(clean && file.exists(filedata)) file.remove(filedata)
  return(Dat)
}

import_data <- function(Source,day,pollutant,path="/home/giovanni/R/projects/calicantus/",verbose=F,...) {
  config <- paste0(path,"config/obs-data-access/access.",Source,".",pollutant,".R")
  if(file.exists(config)) {
    if(verbose) cat(paste("config:",config))
    metadata <- paste0(path,"data/sites-info/metadata.",Source,".csv")
    filedata <- get_data(config = config, day = day, verbose=verbose)
    dat <- read_data(config = config, filedata = filedata, 
                     metadata = metadata, day = day, 
                     pollutant = pollutant, verbose=verbose,
                     ...)
  }else{
    if(verbose) cat(paste("file not found:",config))
    dat <- NULL
  }
  return(dat)
}

read_info <- function(config,filein){}

data2db <- function(Source,day,pollutant,path="/home/giovanni/R/projects/calicantus/",verbose=F){
  dat <- import_data(Source=Source,day=day,pollutant=pollutant,path=path,verbose=verbose)
  if(length(dat)>0) {
    dat <- data.frame(dat, Sys_time=Sys.time())
    day <- as.POSIXct(day,tz="Africa/Algiers")
    Dir <- paste0(path,"data/obs-data/",format(day,"%Y/%m/%d/"))
    if(!dir.exists(Dir)) dir.create(Dir,recursive=TRUE)
    fileout <- paste0(path,"data/obs-data/",format(day,"%Y/%m/%d/%Y%m%d_"),pollutant,"_",Source,".rda")
    save(dat, file = fileout)
    cat(paste0("saved ",fileout),sep="\n")
  }
}

info2db <- function(info){}