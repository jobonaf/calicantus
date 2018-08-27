getmodel_ftp <- function(config,day){
  source(config)
  FileIn <- format(as.Date(day), format=FileTemplate)
  command <- paste0("wget --no-proxy -N 'ftp://",Usr,":",gsub("@","%40",Pwd),"@",Addr,"/",Path,"/",FileIn,"'")
  cat(command,sep="\n")
  system(command, intern = T)
  out <- system(paste0("ls ",basename(FileIn)), intern = T)
  return(out)
}

read_rse <- function(ncLocal, poll="PM10") {
  library(ncdf4)
  nc_open(ncLocal)->nc
  ga <- ncatt_get(nc,0)
  
  # grid: CRS and coordinates
  # https://www.cmascenter.org/sa-tools/documentation/4.2/html/proj_griddesc.html
  if(ga$GDTYP==2) {
    crs <- paste0("+proj=lcc +lat_1=",ga$P_ALP," +lat_2=",ga$P_BET,
                  " +lat_0=",ga$YCENT," +lon_0=",ga$XCENT,
                  " +x_0=",0," +y_0=",0,
                  " +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
    x <- ga$XORIG + nc$dim$COL$vals * ga$XCELL
    y <- ga$YORIG + nc$dim$ROW$vals * ga$YCELL
  }
  
  # concentration
  conc <- ncvar_get(nc,poll)
  
  # time
  tt <- ncvar_get(nc,"TFLAG")
  yy <- tt[1,1,] %/% 1000
  jj <- tt[1,1,] %% 1000
  library(lubridate)
  Time <- as.Date(paste0(yy,"-01-01")) + jj - 1
  hour(Time)   <- tt[2,1,] %/% 10000
  minute(Time) <- tt[2,1,] %/% 100 %% 100
  second(Time) <- tt[2,1,] %%  100
  
  nc_close(nc)
  out <- list(conc=conc, crs=crs, lon=x, lat=y, Time=Time)
  return(out)
}

prepare_arianet <- function(inFiles=system("ls ARIANET4CALICANTUS_20180607*tgz", intern=T)) {
  outFiles <- NULL
  for (File in inFiles) {
    ff <- system(paste("tar -xvf ",File), intern = TRUE)
    outFiles <- c(outFiles, ff)
  }
  return(outFiles)
}
read_arianet <- function(ncLocal="QualeAria_g1_20180607.nc",
                         crs="+init=epsg:32632 +units=km",
                         poll="PM10") {
  library(ncdf4)
  nc_open(ncLocal)->nc
  
  # coordinates
  x <- nc$dim$x$vals
  y <- nc$dim$y$vals
  
  # concentration
  conc <- ncvar_get(nc,paste0("c_",poll))

  # time
  Time <- ymd_hms("0001-01-01 00:00:00")-days(2)+hours(nc$dim$time$vals)
  
  nc_close(nc)
  out <- list(conc=conc, crs=crs, lon=x, lat=y, Time=Time)
  return(out)
}
read_arianet_multi <- function(ncLocal, poll) {
  library(abind)
  for (nc in ncLocal) {
    cat(paste("Reading",nc),sep="\n")
    dum <- read_arianet(nc, poll=poll)
    if(nc==ncLocal[1]) {
      out <- dum
    } else {
      out$conc <- abind(out$conc, dum$conc)
      if(any(out$lat != dum$lat)) stop("Problem with x coordinate")
      if(any(out$lon != dum$lon)) stop("Problem with y coordinate")
      out$Time <- c(out$Time, dum$Time)
    }
  }
  return(out)
}


plot_ctms <- function(rs, title="PM10", 
                      bb=c(0,25,50,75,100,150,200,300,600),
                      modelnames) {
  library(leaflet)
  leaflet() %>% addTiles() %>% 
    addProviderTiles("Stamen.TonerLite", group = "toner lite") -> m
  
  vv <- NULL
  for (r in rs) vv <- c(vv, values(r))
  rmax <- round(max(vv))
  bb <- c(bb[bb<rmax],rmax)
  vv <- quantile(vv,c(0.01,0.999))
  Pal <- colorNumeric(c("white","steelblue", "olivedrab", "yellow", "orange",
                        "darkred", "purple", "black"),
                      vv,
                      na.color = "transparent")
  for (i in 1:length(rs)) m %>% 
    addRasterImage(rs[[i]], colors = Pal, opacity = 0.5, group=modelnames[i]) ->m
  m %>% 
    addLegend(pal = Pal, values = vv, title = title) %>%
    addLayersControl(baseGroups = modelnames,
                     options = layersControlOptions(collapsed = FALSE)) -> m
  m
}

ctm_timeseries <- function(dat, Lat, Lon) {
  
}
