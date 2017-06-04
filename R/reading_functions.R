
read.ArpaV <- function(file,sep=";",skip=0) {
  hdr <- scan(file,sep=sep,nlines=1,skip=skip,what="")
  hdr <- hdr[-length(hdr)]
  Encoding(hdr) <- "latin1"
  dat <- as.numeric(unlist(strsplit(scan(file,what="",skip=1+skip)[2],split=sep)))
  out <- data.frame(NAME=hdr,PM10=dat)
  return(out)
}


read.ArpaT <- function(file,day) {
  cat(paste0("file=",file,"\n"))
  cat(paste0("day=",day,"\n"))
  day <- as.Date(day,tz="Africa/Algiers")
  Day <- paste(format(day,"%d"),
               c("JAN","FEB","MAR",
                 "APR","MAY","JUN",
                 "JUL","AUG","SEP",
                 "OCT","NOV","DEC")[as.numeric(format(day,"%m"))],
               format(day,"%y"),sep="-")
  library("RJSONIO")
  dat <- RJSONIO::fromJSON(file)
  as.data.frame(dat)->dat
  dat <- t(dat)
  rownames(dat)<-rep(NULL,nrow(dat))
  dat <- as.data.frame(dat)
  dat$DATA_OSSERVAZIONE <- as.character(dat$DATA_OSSERVAZIONE)
  dat$NOME_STAZIONE     <- as.character(dat$NOME_STAZIONE)
  dat$COMUNE            <- as.character(dat$COMUNE)
  dat$PM10 <- as.numeric(gsub('\\*','',as.character(dat$PM10)))
  cat(paste0("seleziono per ArpaT il giorno ",Day,"\n"))
  dat <- dat[which(dat$DATA_OSSERVAZIONE==Day),]
  return(dat)
}

read.ArpaLazio <- function(file,sep,day,stat=NULL) {
  library(dplyr)
  dat <- NULL
  for(ff in file) {
    dd <- try(read.table(ff,head=T,check.names = F,stringsAsFactors = FALSE))
    if(class(dd)!="try-error") {
      dd[dd<0] <- NA
      dd$anno <- NULL
      if(!is.null(stat) && stat=="max") {
        Max <- function(x,nreq) ifelse(sum(!is.na(x))>=nreq,max(x,na.rm=T),NA)
        dd %>% select(-h) %>% group_by(jd) %>% summarize_all(Max,nreq=18) -> dd
      }
      if(is.null(dat)) {
        dat<-dd
      } else {
        dat <- merge(x=dat,y=dd,by=c("jd"),all=T)
      }
    }
  }
  idx <- which(dat$jd==(as.POSIXlt(day)$yday+1))
  out <- data.frame(ID=as.character(colnames(dat)[-1:-2]), 
                    Valore=c(unlist(dat[idx[1],][-1:-2])),
                    stringsAsFactors = F)
  return(out)
}


read.ArpaPuglia <- function(file,sep,day) {
  dat <- read.table(file,sep = sep,blank.lines.skip = T,fill = T,stringsAsFactors = F)
  dat[,1] <- stringr::str_trim(dat[,1])
  dat[,colSums(!is.na(dat))==0] <- NULL
  hdr <- dat[as.character(dat[,1])=="NomeCentralina",][1,]
  colnames(dat) <- stringr::str_trim(as.character(hdr))
  dat$Valore <- as.numeric(as.character(dat$Valore))
  out <- dat[which(!is.na(dat$Valore)),]
  return(out)
}


round_awayfromzero <- function(x,digits=0) trunc(x*10^digits+sign(x)*0.5)*10^-digits

read.ArpaCampania <- function(file,sep,day,poll,stat) {
  #library("Rcpp", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
  library(dplyr)
  library(caTools)
  dat <- read.table(file,sep = sep,stringsAsFactors = F,header = T)
  dat %>% mutate(data=substr(data_ora,1,10)) %>%
    filter(as.POSIXct(data,tz="Africa/Algiers")==
             as.POSIXct(day,tz="Africa/Algiers"),
           inquinante==poll,
           !is.na(valore)) %>% 
    group_by(stazione, data) -> dat
  if(stat=="mean") {
    dat %>% summarize(valore=round_awayfromzero(mean(valore)), nh=n()) %>%
      filter(nh>=18) -> out
  }
  if(stat=="max") {
    dat %>% summarize(valore=round_awayfromzero(max(valore)), nh=n()) %>%
      filter(nh>=18) -> out
  }
  out <- as.data.frame(out)
  return(out)
}


read.AzoCroatia <- function(file,IDs, stat=NULL) {
  library(RJSONIO)
  out <- data.frame(ID=IDs, VAL=NA)
  nf <- length(file)
  if(nf!=length(IDs)) stop("IDs and files must have the same number of elements!")
  for (i in 1:nf) {
    tmp <- RJSONIO::fromJSON(file[i])
    if(length(tmp)>0) {
      if(is.null(stat)) {
        out$VAL[i] <- tmp[[1]]$Podatak$vrijednost
      }else if(stat=="max") {
        mm <- max(unlist(lapply(tmp, function(x) {x$Podatak$vrijednost})), na.rm=T)
        if(mm>=0) out$VAL[i] <- mm
      }
    }
  }
  return(out)
}

read.UacerTicino <- function(file,IDs, stat=NULL) {
  out <- data.frame(ID=IDs, VAL=NA)
  nf <- length(file)
  if(nf!=length(IDs)) stop("IDs and files must have the same number of elements!")
  for (i in 1:nf) {
    tmp <- read.table(file[i],comment.char = "#", blank.lines.skip = T, header = T, sep=";")
    if(length(tmp)>0) {
      if(is.null(stat)) {
        out$VAL[i] <- tmp[1,2]
      }else if(stat=="max") {
        h <- format(strptime(tmp[,1],"%d.%m.%Y %H:%M"),"%H")
        mm <- max(tapply(tmp[,2],h,mean,na.rm=T),na.rm=T)
        if(mm>=0) out$VAL[i] <- mm
      }
    }
  }
  return(out)
}

read.SepaSerbia <- function(file,sep,day) { 
  library(XML)
  dat <- readHTMLTable(file, colClasses = c("character",rep("numeric",length(sep))))[[1]]
  colnames(dat) <- c("date",sep)
  dat <- dat[as.Date(dat$date)==as.Date(day),-1]
  ave <- colMeans(dat, na.rm = T)
  valid <- colSums(!is.na(dat))
  ave[valid<24*0.75] <- NA
  out <- data.frame(ID=sep, VAL=ave)
  return(out)
}
