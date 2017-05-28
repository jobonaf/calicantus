get_ftp <- function(config,FileIn){
  source(config)
  # command <- paste0("ftp -np ",Addr,"<<EOF\n",
  #                   "user ",Usr," ",Pwd,"\n",
  #                   "cd ./",Path,"\n",
  #                   "get ",FileIn,"\n",
  #                   "bye\nEOF")
  command <- paste0("wget --no-proxy 'ftp://",Usr,":",gsub("@","%40",Pwd),"@",Addr,"/",Path,"/",FileIn,"'")
  cat(command,sep="\n")
  system(command)
  check <- file.exists(FileIn)
  return(check)
}

get_http <- function(config,FileIn,proxyconfig){
  source(config)
  source(proxyconfig)
  Sys.setenv(http_proxy=paste0("http://",proxy_usr,":",proxy_pwd,"@",proxy_addr,":",proxy_port,"/"))
  commands <- paste0("wget '",Addr,"/",Path,"/",FileIn,"'",
                    " --proxy-user=",proxy_usr," --proxy-password=",proxy_pwd,
                    " --no-check-certificate",
                    " -O '",FileIn,"'")
  for(command in commands) system(command)
  check <- any(file.exists(FileIn))
  return(check)
}

get_ssh <- function(config,FileIn){
  source(config)
  command <- paste0("sshpass -p '",Pwd,
                    "' scp ",Usr,"@",Addr,":",Path,"/",FileIn," .")
  system(command)
  check <- file.exists(FileIn)
  return(check)
}

get_local <- function(config,FileIn){
  source(config)
  command <- paste0("cp ",Path,"/",FileIn," .")
  system(command)
  check <- file.exists(FileIn)
  return(check)
}

get_dbqaemr <- function(config,day) {
  source(config)
  day <- format(as.POSIXct(day,tz="Africa/Algiers"),"%Y-%m-%d")
  con <- dbqa.connect(dbqa_usr, dbqa_pwd, dbqa_name)
  query <- "select distinct ID_STAZIONE, NOME_STAZIONE, LAT, LON from AA_ARIA.ANG_CONFIG_SENSORI"
  ana <- dbGetQuery(con, query)
  
  SSS <- NULL
  for (prov in c("PC","PR","RE","MO","BO","FE","RA","FC","RN")) {
    sss <- dbqa.list.active.staz(con,prov,as.POSIXct(day,tz="Africa/Algiers"))
    idx <- which(dbqa.isrrqa(con,sss))
    sss <- sss[idx]
    SSS <- c(SSS,sss)
  }
  ns <- length(SSS)
  PM10 <- rep(NA,ns)
  for(i in 1:ns) {
    pp <- dbqa.get.datastaz(con,
                            ts.range=c(as.POSIXct(day,tz="Africa/Algiers"),
                                       as.POSIXct(day,tz="Africa/Algiers")),
                            id.staz=SSS[i],
                            id.param=5, 
                            tstep="d",
                            table="annuale",
                            flg.null=TRUE)   ## prendo anche i dati non ancora validati
    if(!is.null(pp))if(!is.na(pp))if(pp!=0) PM10[i]<-round(pp)
  }
  dbDisconnect(con)
  
  idx <- match(SSS,as.character(ana$ID_STAZIONE))
  lat <- ana$LAT[idx]
  lon <- ana$LON[idx]
  nome <- ana$NOME_STAZIONE[idx]
  id <- ana$ID_STAZIONE[idx]
  Dat <- data.frame(id=id,nome=nome,lat=lat,lon=lon,PM10=PM10)
  check <- nrow(Dat)>0
  write.table(Dat,file=paste0("ARPAE_PM10_",day,".csv"),sep=",",col.names = T,row.names = F)
  return(check)
}

get_metadata_dbqaemr <- function(config) {
  source(config)
  con <- dbqa.connect(dbqa_usr, dbqa_pwd, dbqa_name)
  query <- "select distinct ID_STAZIONE, NOME_STAZIONE, LAT, LON from AA_ARIA.ANG_CONFIG_SENSORI"
  ana <- dbGetQuery(con, query)
  dbDisconnect(con)
  write.table(ana,file="../data/sites-info/metadata.ARPAE.csv",sep=",",col.names = T,row.names = F)
}

get_data <- function(config,day,proxyconfig = "../config/config_proxy.R"){
  source(config)
  FileIn <- format(as.POSIXct(day), format=File)
  suppressWarnings(file.remove(FileIn))

  check <- switch(Type,
                  ftp  =get_ftp  (config,FileIn),
                  http =get_http (config,FileIn,proxyconfig),
                  ssh  =get_ssh  (config,FileIn),
                  local=get_local(config,FileIn),
                  dbqaemr=get_dbqaemr(config,day))  
    
  if(check) {
    nf <- length(FileIn)
    FileOut <- paste0(Source,".",
                      paste0(sprintf(1:nf,fmt="%03i"),".")[nf>1],
                      Content,".",
                      format(as.POSIXct(day),format="%Y%m%d"),
                      ".dat")
    file.rename(FileIn, FileOut)
    cat(paste0("rinominato ",FileIn, " in ",FileOut,"\n"))
  } else {
    FileOut <- ""
  }
  return(FileOut)
}