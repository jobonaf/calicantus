# scripts
source("/home/giovanni/R/projects/calicantus/R/reading_functions.R")
source("/home/giovanni/R/projects/calicantus/R/get_data.R")
source("/home/giovanni/R/projects/calicantus/R/prepare_data.R")

# parameters
if(interactive()) {
  sources <- c("ARPA-Marche")
  pollutants <- c("PM10","NO2","O3")
} else {
  sources <- c("ARPA-Sicilia","ARPA-Lombardia","ARPAV","ARPA-FVG","ARPAB",
               "ARPA-Liguria","ARPAC","ARPA-Lazio","ARPA-Umbria","ARPA-Puglia",
               "ARPAT","AZO-Croatia","DT-DA-SPAAS-UACER-Ticino","ARPA-Piemonte",
               "ARPAE","ARSO-Slovenia", "Umweltbundesamt-Austria","SEPA-Serbia",
               "APPA-Trento","APPA-Bolzano","ExEA-Bulgaria", "ARPA-VdA",
               "ARPA-Marche")
  pollutants <- c("PM10","PM2.5","NO2","O3")
}

# arguments
if(interactive()) {
  aa <- rep(NA,2)
  aa[1] <- readline(prompt = "first day: how many days ago?")
  aa[2] <- readline(prompt = "last day: how many days ago?")
  verbose <- T
} else {
  aa <- commandArgs(trailingOnly=TRUE)
  print(aa)
  if(length(aa)==0) aa[1] <- 1
  if(length(aa)<=1) aa[2] <- 8
  verbose <- F
}
by <- ifelse(as.numeric(aa[1])>as.numeric(aa[2]),"1 days","-1 days")
Days <- as.character(seq.Date(Sys.Date()-as.numeric(aa[1]),
                              Sys.Date()-as.numeric(aa[2]),
                              by=by))

# data collection
setwd("/home/giovanni/R/projects/calicantus/run/")
for(dd in Days) {
  for(pp in pollutants) {
    for(ss in sources) {
      data2db(Source = ss, day = dd, pollutant = pp, verbose=verbose)
    }
  }
}

