
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

read.ArpaLazio <- function(file,sep,day) {
  dat <- NULL
  for(ff in file) {
    dd <- try(read.table(ff,head=T,check.names = F,stringsAsFactors = FALSE))
    if(class(dd)!="try-error") {
      if(is.null(dat)) {
        dat<-dd
      } else {
        dat <- merge(x=dat,y=dd,by=c("anno","jd"),all=T)
      }
    }
  }
  idx <- which(as.character(dat$anno)==format(as.POSIXct(day),"%Y") & dat$jd==(as.POSIXlt(day)$yday+1))
  out <- data.frame(ID=as.character(colnames(dat)[-1:-2]), Valore=c(unlist(dat[idx[1],][-1:-2])),stringsAsFactors = F)
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