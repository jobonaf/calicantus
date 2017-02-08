# scripts
source("/home/giovanni/R/projects/calicantus/R/reading_functions.R")
source("/home/giovanni/R/projects/calicantus/R/get_data.R")
source("/home/giovanni/R/projects/calicantus/R/prepare_data.R")

# parameters
sources <- c("ARPAV","ARPAT","ARPAE","ARPA-Umbria",
             "ARPA-Sicilia","ARPA-Piemonte","ARPA-Lombardia",
             "ARPA-Liguria","ARPA-FVG","ARPA-Lazio",
             "ARPA-Puglia","ARPAC","AZO-Croatia")
#sources <- c("AZO-Croatia")
pollutants <- c("PM10")

# arguments
if(interactive()) {
  aa <- rep(NA,2)
  aa[1] <- readline(prompt = "first day: how many days ago?")
  aa[2] <- readline(prompt = "last day: how many days ago?")
} else {
  aa <- commandArgs(trailingOnly=TRUE)
  print(aa)
  if(length(aa)==0) aa[1] <- 1
  if(length(aa)<=1) aa[2] <- 6
}
by <- ifelse(as.numeric(aa[1])>as.numeric(aa[2]),"1 days","-1 days")
days <- as.character(seq.Date(Sys.Date()-as.numeric(aa[1]),
                              Sys.Date()-as.numeric(aa[2]),
                              by=by))

# data collection
setwd("/home/giovanni/R/projects/calicantus/run/")
for(dd in days) {
  for(pp in pollutants) {
    for(ss in sources) {
      data2db(Source = ss, day = dd, pollutant = pp)
    }
  }
}

