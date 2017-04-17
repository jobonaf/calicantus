source("/home/giovanni/R/projects/calicantus/R/reading_functions.R")
source("/home/giovanni/R/projects/calicantus/R/get_data.R")


read_data <- function(config,filedata,metadata,day,pollutant,clean=TRUE){
  source(config)
  cat(paste("estraggo dati",Source,day,"\n"))
  check <- any(nchar(filedata)>0)
  if(check) {
    dat <- NULL
    if(!select.by.day) dat <- try(readFun(file = filedata, sep=Sep))
    if(select.by.day) dat <- try(readFun(file = filedata, sep=Sep, day=day))
    check <- class(dat)[1]!="try-error"
    if(check) check <- nrow(dat)>0
  }
  if(check) {
    ana <- read.csv(metadata)
    id.dat <- match(as.character(ana[,Id.metadata]),as.character(dat[,Id.data]))
    id.ana <- which(!is.na(id.dat))
    check <- length(id.ana)>0
  }
  if(check) {
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
    Dat <- data.frame(Name=name,Lat=lat,Lon=lon,Value=value,
                      Pollutant=pollutant,Day=day,Source=Source)
  } else {
    Dat <- NULL
  }  
  if(clean && file.exists(filedata)) file.remove(filedata)
  return(Dat)
}

import_data <- function(Source,day,pollutant,path="/home/giovanni/R/projects/calicantus/",...) {
  config <- paste0(path,"config/data-access/access.",Source,".",pollutant,".R")
  if(file.exists(config)) {
    metadata <- paste0(path,"data/sites-info/metadata.",Source,".csv")
    filedata <- get_data(config = config, day = day)
    dat <- read_data(config = config, filedata = filedata, 
                     metadata = metadata, day = day, pollutant = pollutant,...)
  }else{
    dat <- NULL
  }
  return(dat)
}

read_info <- function(config,filein){}

data2db <- function(Source,day,pollutant,path="/home/giovanni/R/projects/calicantus/"){
  dat <- import_data(Source=Source,day=day,pollutant=pollutant,path=path)
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