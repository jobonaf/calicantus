# generate records to be appended to config/ui_credentials.R
# (and text for email)
string_users <- function(users=read.csv(file = "/home/giovanni/R/projects/calicantus/data/data-sources/contacts.csv",
                                        stringsAsFactors = F)$data.source,
                         email=paste(read.csv(file = "/home/giovanni/R/projects/calicantus/data/data-sources/contacts.csv",
                                              stringsAsFactors = F)$e.mail..1,
                                     read.csv(file = "/home/giovanni/R/projects/calicantus/data/data-sources/contacts.csv",
                                              stringsAsFactors = F)$e.mail..2,
                                     sep="; "),
                         simple=T) {
  if(simple) sapply(stringi::stri_split_fixed(users,"-"), 
                    function(x) tolower(paste(x[(length(x)-1):length(x)],
                                              collapse="_"))) -> users
  nu <- length(users)
  pwds <- rep("",nu)
  for (i in 1:nu) pwds[i] <- stringi::stri_rand_strings(n=1, length=8, pattern="[A-Za-z0-9]")
  for (i in 1:nu) {
    nc <- nchar(users[i])
    blanks <- paste(rep(" ",max(0,max(nchar(users))-nc)),collapse="")
    out <- paste0('ui_usr<-append(ui_usr,"',users[i],'"',blanks,
                  ');ui_pwd<-append(ui_pwd,"',pwds[i],'")')
    cat(out,sep="\n")
  }
  for (i in 1:nu) {
    out <- paste0('\nto: ',email[i],'\n',
                  'user: "',users[i],'"\n',
                  'password: "',pwds[i],'"')
    cat(out,sep="\n")
  }
}


# encrypt a file
encrypt_file <- function(filein, fileout=paste0(filein,".enc"), pwd=NULL) {
  if(is.null(pwd)) pwd <- readline("password?")
  command <- paste0("openssl enc -in ",filein," -aes-256-cbc -pass stdin > ",fileout)  
  system(command, input=pwd)
  return(fileout)
}

# decrypt a file
decrypt_file <- function(filein, fileout=paste0(filein,".dec"), pwd=NULL) {
  if(is.null(pwd)) pwd <- readline("password?")
  command <- paste0("openssl enc -in ",filein," -d -aes-256-cbc -pass stdin > ",fileout)  
  system(command, input=pwd)
  return(fileout)
}

# guess AQ station EoI code
source("/home/giovanni/R/projects/calicantus/R/get_eea.R")
library(dplyr)
guess_eoi <- function(new.coords, 
                      eea.metadata=eea_get_metadata() %>% 
                        filter(AirPollutantCode=="http://dd.eionet.europa.eu/vocabulary/aq/pollutant/5"), 
                      max.dist=200) {
  library(dplyr)
  eea.metadata %>% 
    dplyr::select(AirQualityStationEoICode,Latitude,Longitude) %>% 
    distinct() -> eea.meta
  library(geosphere)
  eea.coords <- cbind(eea.meta$Longitude,eea.meta$Latitude)
  nearest.eea <- function(coords) {
    which.min(distm(coords, eea.coords))
  }
  dist <- function(ll) {
    round(distm(x = c(ll[1],ll[2]), y = c(ll[3],ll[4])))
  }
  new.coords$Nearest <- apply(X = cbind(new.coords$Lon,
                                        new.coords$Lat), 
                              MARGIN = 1, 
                              FUN = nearest.eea)
  new.coords %>% 
    mutate(Eea.Lon=eea.coords[Nearest,1],
           Eea.Lat=eea.coords[Nearest,2]) -> new.coords
  new.coords$Dist <- apply(X = data.frame(lo0=new.coords$Lon,
                                          la0=new.coords$Lat,
                                          lo1=new.coords$Eea.Lon,
                                          la1=new.coords$Eea.Lat),
                           MARGIN = 1,
                           FUN = dist)
  new.coords %>% 
    mutate(EoICode=ifelse(Dist<=max.dist,
                          eea.meta$AirQualityStationEoICode[Nearest],
                          NA)) %>%
    mutate(Dist=ifelse(is.na(EoICode),NA,Dist),
           Eea.Lon=ifelse(is.na(EoICode),NA,Eea.Lon),
           Eea.Lat=ifelse(is.na(EoICode),NA,Eea.Lat)) %>%
    dplyr::select(-Nearest)
}

convertcoords <- function(coords,
                          crs.in="+init=epsg:25832",
                          crs.out="+init=epsg:4326") {
  library(rgdal)
  if(is.list(coords)) {
    d <- data.frame(x=coords[[1]], y=coords[[2]])
  } else if(is.matrix(coords)) {
    d <- data.frame(x=coords[,1], y=coords[,2])
  }
  coordinates(d) <- c("x", "y")
  proj4string(d) <- CRS(crs.in)
  CRS.new <- CRS(crs.out)
  d.out <- spTransform(d, CRS.new)
}
