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
  commands <- paste0("wget --no-proxy '",ncRemote,"'",
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
ctm_daily_stat <- cams_daily_stat


cams2raster <- function(DT, day=format(Sys.Date(),"%Y-%m-%d"), 
                        stat=c("Mean","Max","MaxAvg8h"),
                        crs="+init=epsg:4326") {
  library(raster)
  stat <- match.arg(stat)
  dt <- subset(DT, Day==day, select=c("Lon","Lat",stat))
  cat(paste0("Converting to raster: ",stat," of day ",day,cat="\n"))
  if(nrow(dt)>0) {
    r <- rasterFromXYZ(dt, crs = crs)
  } else {
    r <- NULL
  }
  return(r)
}
ctm2raster <- cams2raster


archive_cams <- function(Day=Sys.Date(),
                         pathIn="/home/giovanni/R/projects/calicantus/run/cams/",
                         pathOut="/home/giovanni/R/projects/calicantus/data/mod-data/grid/") {
  FF <- system(paste0("ls ",pathIn,"CAMS50_ref",format(Day,"%Y%m%d"),
                      "_val????????_*.rda"), intern=T)
  dirOut <- paste0(pathOut,format(Day,"%Y/%m/%d/"))
  if(!dir.exists(dirOut)) dir.create(dirOut,recursive = T)
  for (ff in FF) file.rename(from=ff,to=paste0(dirOut,basename(ff)))
}

archive_cams_timeseries <- function(Day=Sys.Date(),
                         pathIn="/home/giovanni/R/projects/calicantus/run/cams/",
                         pathOut="/home/giovanni/R/projects/calicantus/data/mod-data/timeseries/") {
  FF <- system(paste0("ls ",pathIn,"CAMS50_ref",format(Day,"%Y%m%d"),
                      "_timeseries.rda"), intern=T)
  dirOut <- paste0(pathOut,format(Day,"%Y/%m/%d/"))
  if(!dir.exists(dirOut)) dir.create(dirOut,recursive = T)
  for (ff in FF) file.rename(from=ff,to=paste0(dirOut,basename(ff)))
}


plot_cams <- function(r, title="PM10", 
                      bb=c(0,25,50,75,100,150,200,300,600)) {
  library(leaflet)
  rmax <- round(cellStats(r,"max"))
  bb <- c(bb[bb<rmax],rmax)
  vv <- quantile(values(r),c(0.05,1))
  Pal <- colorNumeric(c("steelblue", "olivedrab", "yellow", "orange", "darkred", "purple"),
                      vv,
  na.color = "transparent")
  leaflet() %>% addTiles() %>% 
    addProviderTiles("Stamen.TonerLite", group = "toner lite") %>%
    addRasterImage(r, colors = Pal, opacity = 0.5) %>%
    addLegend(pal = Pal, values = vv, title = title)
}



cams_latlon2ij <- function(ncLocal, Lon, Lat){
  library(ncdf4)
  nc_open(ncLocal)->nc
  lon <- ncvar_get(nc,"longitude")
  lat <- ncvar_get(nc,"latitude")
  nc_close(nc)
  np <- length(Lon)
  out <- list(i=rep(NA,np),j=rep(NA,np))
  for (k in 1:np) {
    iLon <- ifelse(Lon[k]<0,Lon[k]+360,Lon[k])
    iLat <- Lat[k]
    if(iLon>max(lon) | iLon<min(lon) | iLat>max(lat) | iLat<min(lat)) {
      out$i[k] <- NA
      out$j[k] <- NA
    } else {
      out$i[k] <- which.min(abs(iLon-lon))
      out$j[k] <- which.min(abs(iLat-lat))
    }
  }
  return(out)
}

cams2ts <- function(ncLocal, Lon, Lat, Names=paste0("p",1:length(Lon))) {
  library(ncdf4)
  library(dplyr)
  ij <- cams_latlon2ij(ncLocal[1], Lon, Lat)
  notna <- which(!is.na(ij$i) & !is.na(ij$j))
  Lon <- Lon[notna]
  Lat <- Lat[notna]
  Names <- Names[notna]
  ij$i <- ij$i[notna]
  ij$j <- ij$j[notna]
  out <- NULL
  np <- length(Lon)
  for (ff in ncLocal) {
    nc_open(ff)->nc
    for (i in 1:np) {
      conc <- ncvar_get(nc,names(nc$var)[1],
                        start=c(ij$i[i],ij$j[i],1,1),
                        count=c(1,1,1,-1))
      if(i==1) {
        hours <- ncvar_get(nc,"time")
        reftime <- as.POSIXct(strptime(strsplit(nc$filename,"_")[[1]][2],
                                       format = "%Y%m%d", tz = "UTC"))
        Time <- reftime + 3600*hours
      }
      dat <- data.frame(Lon=Lon[i], Lat=Lat[i], Name=Names[i], Conc=conc, Time=Time)
      if(is.null(out)) {
        out <- dat
      } else {
        out <- bind_rows(out,dat)
      }
    }
    nc_close(nc)
  }
  return(out)
}

select_cities <- function(filein="~/R/projects/calicantus/data/sites-info/selected_cities.csv",
                          fileout=NULL) {
  if(!is.null(filein) & file.exists(filein)) {
    cities <- read.csv(filein, stringsAsFactors = F)
  } else {
    country.focus <- c("Italy","Sicily","Slovenia","Switzerland","Croatia","Austria",
                       "Vatican City","San Marino","Tunisia")
    library(maps)
    library(cluster)
    library(dplyr)
    data(world.cities)
    
    rangeLat <- c( 30.05,69.95)
    rangeLon <- c(-24.95,44.95)
    world.cities %>% 
      filter(!country.etc%in%country.focus,
             lat>=rangeLat[1],
             lat<=rangeLat[2],
             long>=rangeLon[1],
             long<=rangeLon[2],
             capital>0|pop>100000) -> c1
    cl1 <- clara(cbind(c1$long,c1$lat),200,samples = 20)
    c1 %>% mutate(cluster=cl1$clustering) %>% 
      arrange(desc(pop)) %>%
      group_by(cluster) %>%
      dplyr::summarize(name=dplyr::first(name),pop=dplyr::first(pop),
                       lat=dplyr::first(lat),long=dplyr::first(long),
                       country.etc=dplyr::first(country.etc)) %>%
      ungroup() %>% dplyr::select(-cluster) -> c1
    world.cities %>% 
      filter(country.etc%in%country.focus,
             lat>=rangeLat[1],
             lat<=rangeLat[2],
             long>=rangeLon[1],
             long<=rangeLon[2],
             capital>0|pop>30000) -> c2
    cl2 <- clara(cbind(c2$long,c2$lat),200,samples = 20)
    c2 %>% mutate(cluster=cl2$clustering) %>%
      arrange(desc(pop)) %>%
      group_by(cluster) %>%
      dplyr::summarize(name=dplyr::first(name),pop=dplyr::first(pop),
                       lat=dplyr::first(lat),long=dplyr::first(long),
                       country.etc=dplyr::first(country.etc)) %>%
      ungroup() %>% dplyr::select(-cluster) -> c2
    cities <- bind_rows(c2,c1)
    dd <- which(duplicated(cities$name)|duplicated(cities$name,fromLast = T))
    if(length(dd)>0) cities$name[dd] <- paste0(cities$name[dd]," (",cities$country.etc[dd],")")
    cities$country.etc <- NULL
  }
  
  if(!is.null(fileout)) {
    write.table(cities, file = fileout, sep=",", row.names = F, col.names = T)
  } 
  return(cities)
}
