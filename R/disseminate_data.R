available_sources <- function(user) {
  source("/home/giovanni/R/projects/calicantus/config/config_users.R",local=TRUE)
  auth_sources <- authorizedSources(user)
  aS <- system("cd /home/giovanni/R/projects/calicantus/data/sites-info/; ls metadata*csv | sed s/metadata.//g | sed s/.csv//g",intern=TRUE)
  aS <- intersect(aS, auth_sources)
  return(aS)
}


data_for_user <- function(user,
                          first=Sys.Date()-10,
                          last=Sys.Date()-1,
                          verbose=F) {
  library(dplyr)
  dd <- as.character(seq.Date(first,last,"1 day"))
  ff <- NULL
  for(d in dd) {
    ff <- c(ff,system(paste0("ls /home/giovanni/R/projects/calicantus/data/obs-data/",
                             format(as.Date(d),"%Y/%m/%d/%Y%m%d_"),
                             "*_*.rda 2>/dev/null"),
                      intern=TRUE))
  }
  Dat <- NULL
  nf <- length(ff)
  for (i in 1:nf) {
    if(verbose) cat(paste0("Loading file ",ff[i]),sep="\n")
    load(ff[i])
    Dat <- bind_rows(Dat,dat)
  }
  Dat %>% dplyr::filter(!is.na(Name) & !is.na(Lat) & !is.na(Lon) & 
                          Source%in%available_sources(user))  %>%
    dplyr::mutate(Value=round(Value)) -> Dat
  return(Dat)
}


prepare_dissemination <- function(first=NA, last=NA) {
  setwd("/home/giovanni/R/projects/calicantus/run/diss/")
  source("/home/giovanni/R/projects/calicantus/config/ui_credentials.R", local=TRUE)
  source("/home/giovanni/R/projects/calicantus/R/util.R")
  period <- NA
  if (is.na(first) & is.na(last))  period <- "latest"
  if (is.na(first)) first <- Sys.Date()-10
  if (is.na(last))  last  <- Sys.Date()-1
  if (is.na(period)) period <- paste(first,last,sep="-")
  out <- NULL
  for(i in 1:length(ui_usr)) {
    usr <- ui_usr[i]
    cat(paste("preparing data for",usr), cat="\n")
    pwd <- ui_pwd[i]
    dat <- data_for_user(user = usr, first = first, last = last)
    fileout <- paste0(period,"_data_for_",usr,".json")
    jsonlite::write_json(dat, path = fileout)
    fileenc <- encrypt_file(filein = fileout, pwd = pwd)
    system(paste0("gzip -f ",fileenc))
    out <- c(out,paste0(fileenc,".gz"))
    file.remove(fileout)
  }
  return(out)
}


setwd("/home/giovanni/R/projects/calicantus/run/diss/")
source("/home/giovanni/R/projects/calicantus/config/config_dissemination.R")
files <- prepare_dissemination()
for(ff in files) {
  cat(paste0("uploading ",ff),sep="\n")
  RCurl::ftpUpload(what = ff, 
                   to = paste0("ftp://",diss_ftp_Usr,":",
                               diss_ftp_Pwd,"@",diss_ftp_Addr,"/",
                               diss_ftp_Path,"/",ff))
}


## to get the data from the bash:
# wget 'ftp://<user_ftp>:<passw_ftp>@ftp.smr.arpa.emr.it/data-out/latest_data_for_<user_calicantus>.json.enc.gz'
# gunzip latest_data_for_<user_calicantus>.json.enc.gz
# openssl enc -in latest_data_for_<user_calicantus>.json.enc -d -aes-256-cbc -out mydata.json -k <passw_calicantus>



