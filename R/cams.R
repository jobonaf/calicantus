# get NetCDF
get_cams <- function(
  model=c("CHIMERE","EMEP","EURAD","LOTOSEUROS","MATCH","MOCAGE","SILAM"),
  pollutant=c("O3","CO","NO2","SO2","PM25","PM10","PANS","NMVOC","NO","NH3",
              "BIRCHPOLLEN","OLIVEPOLLEN","GRASSPOLLEN"),
  validity=0:3,
  level=c("SURFACE","ALLLEVELS"),
  address="http://download.regional.atmosphere.copernicus.eu/services/",
  service="CAMS50",
  token="__M0bChV6QsoOFqHz31VRqnpr4GhWPtcpaRy3oeZjBNSg__",
  grid=0.1,
  reftime=Sys.Date(),
  proxyconfig="/home/giovanni/R/projects/calicantus/config/config_proxy.R",
  test=F)
{
  # manage arguments
  model <- match.arg(model,several.ok = T)
  pollutant <- match.arg(pollutant,several.ok = T)
  if(any(!validity %in% -1:3)) stop("Parameter 'validity' can be: -1, 0, 1, 2, or 3")
  level <- match.arg(level)
  
  # combinations
  Args <- expand.grid(reftime, model, pollutant, validity)
  reftime <- Args[,1]
  model <- Args[,2]
  pollutant <- Args[,3]
  validity <- Args[,4]
  
  # extra arguments
  timerange=c("-24H-1H","0H24H","25H48H","49H72H","73H96H")[validity+2]
  type=c("ANALYSIS","FORECAST","FORECAST","FORECAST","FORECAST")[validity+2]
  
  # wget
  ncRemote <- paste0(address,service,
                     "?&token=",token,
                     "&grid=",grid,
                     "&model=",model,
                     "&package=",type,"_",pollutant,"_",level,
                     "&time=",timerange,
                     "&referencetime=",format(reftime,"%Y-%m-%dT00:00:00Z"),
                     "&format=NETCDF",
                     "&licence=yes")
  ncLocal <- paste0(service,"_",format(reftime,"%Y%m%d"),"_",pollutant,"_",model,"_",timerange,".nc")
  source(proxyconfig)
  Sys.setenv(http_proxy=paste0("http://",proxy_usr,":",proxy_pwd,"@",proxy_addr,":",proxy_port,"/"))
  commands <- paste0("wget '",ncRemote,"'",
                    " --proxy-user=",proxy_usr," --proxy-password=",proxy_pwd,
                    " --no-check-certificate",
                    " -O '",ncLocal,"'")
  if(!test) for(command in commands) system(command)
  return(ncLocal)
}

# read NetCDF
read_cams <- function(ncLocal, reftime=NULL, digits.coord=2) {
  library(ncdf4)
  nc_open(ncLocal)->nc
  conc <- ncvar_get(nc,names(nc$var)[1])
  lon <- ncvar_get(nc,"longitude")
  lat <- ncvar_get(nc,"latitude")
  hours <- ncvar_get(nc,"time")
  nc_close(nc)
  if(is.null(reftime)) {
    reftime <- as.POSIXct(strptime(strsplit(nc$filename,"_")[[1]][2],
                                   format = "%Y%m%d", tz = "UTC"))
  }
  Time <- reftime + 3600*hours
  lon <- lon-360*(lon%/%180)
  out <- list(conc=conc, lat=round(lat,digits.coord), 
              lon=round(lon,digits.coord), Time=Time)
  return(out)
}

read_cams_multi <- function(ncLocal, reftime=NULL, digits.coord=2) {
  library(abind)
  for (nc in ncLocal) {
    cat(paste("Reading",nc),sep="\n")
    dum <- read_cams(nc, reftime=reftime, digits.coord = digits.coord)
    if(nc==ncLocal[1]) {
      out <- dum
    } else {
      out$conc <- abind(out$conc, dum$conc)
      out$lat <- c(out$lat, dum$lat)
      out$lon <- c(out$lon, dum$lon)
      out$Time <- c(out$Time, dum$Time)
    }
  }
  return(out)
}


cams_daily_stat <- function(dat,dMean=T,dMax=T,dMaxAvg8h=T) {
  library(RcppRoll)
  library(data.table)
  
  Lon <- unique(dat$lon)
  Lat <- unique(dat$lat)
  nx <- length(Lon)
  ny <- length(Lat)
  
  cat(paste0(Sys.time()," preparing data.table..."),sep="\n")
  DT<-data.table(Dat=dat$conc,
                 Lon=Lon,
                 Lat=rep(Lat,each=nx),
                 Day=rep(format(dat$Time-1,"%Y-%m-%d",tz="UTC"),each=nx*ny))
  if(dMaxAvg8h) {
    cat(paste0(Sys.time()," calculating rolling mean..."),sep="\n")
    DT <- DT[,.(Avg8h=roll_meanr(Dat,8),
                Day=Day,Dat=Dat), 
             by=.(Lon,Lat)]
  }
  cat(paste0(Sys.time()," calculating daily statistic..."),sep="\n")
  DT <- DT[,.(Max=ifelse(dMax,max(Dat),NA),
              Mean=ifelse(dMean,mean(Dat),NA),
              MaxAvg8h=ifelse(dMaxAvg8h,max(Avg8h,na.rm=T),NA),
              N=.N), 
           by=.(Lon,Lat,Day)]
  DT <- subset(DT, N>=18)
  return(DT)
}


cams2raster <- function(DT, day=format(Sys.Date(),"%Y-%m-%d"), 
                        stat=c("Mean","Max","MaxAvg8h"),
                        crs="+init=epsg:4326") {
  library(raster)
  stat <- match.arg(stat)
  dt <- subset(DT, Day==day, select=c("Lon","Lat",stat))
  cat(paste0("Converting to raster: ",stat," of day ",day,cat="\n"))
  r <- rasterFromXYZ(dt, crs = crs)
  return(r)
}


archive_cams <- function(Day=Sys.Date(),
                         pathIn="/home/giovanni/R/projects/calicantus/run/cams/",
                         pathOut="/home/giovanni/R/projects/calicantus/data/mod-data/grid/") {
  FF <- system(paste0("ls ",pathIn,"CAMS50_ref",format(Day,"%Y%m%d"),
                      "_val????????_*.rda"), intern=T)
  dirOut <- paste0(pathOut,format(Day,"%Y/%m/%d/"))
  if(!dir.exists(dirOut)) dir.create(dirOut,recursive = T)
  for (ff in FF) file.rename(from=ff,to=paste0(dirOut,basename(ff)))
}


# 1) GENERALIZZARE
# 2) USARE STESSI LIVELLI DELLE STAZIONI
# 3) AGGIUNGERE A MAPPA-PROXY GIA' ESISTENTE
plot_cams <- function(r, title="PM10", 
                      bb=c(0,25,50,75,100,150,200,300,600)) {
  library(leaflet)
  rmax <- round(cellStats(r,"max"))
  bb <- c(bb[bb<rmax],rmax)
  pal <- colorBin(palette = c("white","steelblue","olivedrab", "greenyellow","orange", "darkred", 
                              "purple","grey","black","cyan"), 
                  domain = 0:600,
                  bins = bb,
                  na.color = "transparent")
  leaflet() %>% addTiles() %>% 
    addProviderTiles("Stamen.TonerLite", group = "toner lite") %>%
    addRasterImage(r, colors = pal, opacity = 0.5) %>%
    addLegend(pal = pal, values = values(r), title = title)
}
