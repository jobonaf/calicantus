get_ftp <- function(config,FileIn){
  source(config)
  command <- paste("ftp -n ",Addr,"<<EOF\n",
                   "user ",Usr," ",Pwd,"\n",
                   "cd ./",Path,"\n",
                   "get ",FileIn,"\n",
                   "bye\nEOF",
                   sep="")
  system(command)
  check <- file.exists(FileIn)
  return(check)
}

get_http <- function(config,FileIn,proxyconfig){
  source(config)
  source(proxyconfig)
  Sys.setenv(http_proxy=paste0("http://",proxy_usr,":",proxy_pwd,"@",proxy_addr,":",proxy_port,"/"))
  command <- paste0("wget ",Addr,"/",Path,"/",FileIn,
                    " --proxy-user=",proxy_usr," --proxy-password=",proxy_pwd,
                    " --no-check-certificate")
  system(command)
  check <- file.exists(FileIn)
  return(check)}

get_ssh <- function(config,FileIn){
  source(config)
  command <- paste("sshpass -p '",Pwd,
                   "' scp ",Usr,"@",Addr,":",Path,"/",FileIn," .",
                   sep="")
  system(command)
  check <- file.exists(FileIn)
  return(check)
}

get_local <- function(config,FileIn){
  source(config)
  command <- paste("cp ",Path,"/",FileIn," .",
                   sep="")
  system(command)
  check <- file.exists(FileIn)
  return(check)
}

get_file <- function(config,FileIn){
  source(config)
  dd <- ifelse(!is.na(Datefmt) && nchar(Datefmt)>0,
               format(as.POSIXct(day),
                      format=Datefmt),
               "")
  FileIn <- paste(Before, dd, After,sep="")
  if(file.exists(FileIn)) file.remove(FileIn)

  check <- switch(Type,
                  ftp  =get_ftp  (config,FileIn),
                  http =get_http (config,FileIn,proxyconfig),
                  ssh  =get_ssh  (config,FileIn),
                  local=get_local(config,FileIn))  
    
  if(check) {
    FileOut <- paste(Source,Content,
                     format(as.POSIXct(day),format="%Y%m%d"),
                     "dat",
                     sep=".")
    file.rename(FileIn, FileOut)
    cat(paste0("rinominato ",FileIn, " in ",FileOut,"\n"))
  } else {
    FileOut <- ""
  }
  return(FileOut)
}