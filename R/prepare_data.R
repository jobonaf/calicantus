read_data <- function(config,File,metadata,day,pollutant,clean=TRUE){
  source("/home/giovanni/R/projects/calicantus/R/reading_functions.R")
  source(config,local=TRUE)
  cat(paste0("config=",config,"\n"))
  cat(paste0("select.by.day=",select.by.day,"\n"))
  cat(paste("estraggo dati",Source,day,"\n"))
  check <- nchar(File)>0
  if(check) {
    dat <- NULL
    cat(paste0("select.by.day=",select.by.day,"\n"))
    if(!select.by.day) dat <- try(readFun(file = File, sep=Sep))
    if(select.by.day) dat <- try(readFun(file = File, sep=Sep, day=day))
    check <- class(dat)!="try-error"
    if(check) check <- nrow(dat)>0
  }
  if(check) {
    ana <- read.csv(metadata)
    id.dat <- match(as.character(ana[,Id.metadata]),as.character(dat[,Id.data]))
    id.ana <- which(!is.na(id.dat))
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
  if(clean) file.remove(File)
  return(Dat)
}

import_data <- function(Source,day,pollutant,path="/home/giovanni/R/projects/calicantus/",...) {
  config <- paste0(path,"config/access.",Source,".",pollutant,".R")
  metadata <- paste0(path,"data/sites-info/metadata.",Source,".csv")
  filedata <- get_data(config = config, day = day)
  dat <- read_data(config = config, File = filedata, 
                   metadata = metadata, day = day, pollutant = pollutant,...)
  return(dat)
}

read_info <- function(config,filein){}

data2db <- function(Source,day,pollutant,path="/home/giovanni/R/projects/calicantus/"){
  dat <- import_data(Source=Source,day=day,pollutant=pollutant,path=path)
  day <- as.POSIXct(day,tz="Africa/Algiers")
  Dir <- paste0(path,"data/obs-data/",format(day,"%Y/%m/%d/"))
  if(!dir.exists(Dir)) dir.create(Dir,recursive=TRUE)
  save(dat, file = paste0(path,"data/obs-data/",format(day,"%Y/%m/%d/%Y%m%d_"),pollutant,"_",Source,".rda"))
}

info2db <- function(info){}