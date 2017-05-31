req.pckgs <- c("shiny", "shinyBS","base64enc","lubridate", "dplyr", 
               "plyr","geosphere","shinyjs", "leaflet","raster", "data.table",
               "tidyr", "scales","ggplot2","ggrepel","stringi","RColorBrewer", 
               "maps", "cluster", "ncdf4", "abind", "RcppRoll", "raster", "ncdf4",
               "sp", "maptools", "rgdal", "RJSONIO", "caTools", "XML", "RCurl")
inst.pckgs <- as.data.frame(installed.packages())
for (pckg in setdiff(req.pckgs,as.character(inst.pckgs$Package))) {
  install.packages(pckg, dependencies = T)
}

