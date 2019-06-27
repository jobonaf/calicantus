# hints:
#
# 1) run this script many times, until the final check is ok,
# the first time you install Calicantus and every time you update
# R to a new minor release;
#
# 2) maybe you need to reinstall some package from github with devtools::install_github()

# detach packages
basic.packages <- c("stats","graphics","grDevices",
                    "utils","datasets","methods","base")
basic.packages <- paste0("package:", basic.packages)
package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
package.list <- setdiff(package.list,basic.packages)
if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)

# list of required packages
req.pckgs <- c("Rcpp", "mime", "shinyBS","base64enc","glue", "lubridate", "dplyr", 
               "units", "survival","gdtools", "pkgconfig","digest",
               "plyr","geosphere","shinyjs", "leaflet","raster", "data.table",
               "tidyr", "scales","ggplot2","ggrepel","stringi","RColorBrewer", 
               "maps", "cluster", "ncdf4", "abind", "RcppRoll", "raster", "ncdf4",
               "sp", "maptools", "rgdal", "RJSONIO", "caTools", "XML", "RCurl",
               "Rcpp","R6","cluster",
               "promises", "later", "httpuv", "htmltools",
               "shinyBS","base64enc","dplyr","plyr","lubridate","sp","rgdal",
               "geosphere","shinyjs","raster","data.table","tidyr",
               "scales","ggplot2","ggrepel","stringi","RColorBrewer","maps",
               "cluster","bitops","RCurl","markdown","yaml","tidyselect",
               "htmlwidgets","leaflet",
               "shinyBS","plyr","markdown",
               "httr","dplyr","data.table","abind","RcppRoll",
               "futile.logger","googledrive","ncdf4","lubridate",
               "leaflet","raster")

req.pckgs <- unique(req.pckgs)

# reinstall obsolete packages
Rvers <- paste(R.Version()$major, R.Version()$minor, sep=".")
rv <- function(x) paste(strsplit(x,split = "\\.")[[1]][1:2], collapse=".")
ip <- function() {
  out <- as.data.frame(installed.packages(), stringsAsFactors = F)
  out <- out[order(out$Version, decreasing=T),]
  out <- out[!duplicated(out$Package),]
  out
}
iold <- mapply(rv, ip()$Built) != rv(Rvers)
old_packages <- req.pckgs[(req.pckgs %in% ip()$Package[iold])]
if(length(old_packages)) {
  cat(paste0("reinstalling obsolete packages ",paste(old_packages,collapse=", ")), sep="\n")
  install.packages(old_packages, 
                   repos='https://cran.rstudio.com/',
                   dependencies = TRUE)
}
  
# install new packages
new_packages <- req.pckgs[!(req.pckgs %in% ip()$Package)]
if(length(new_packages)) {
  cat(paste0("installing new packages ",paste(old_packages,collapse=", ")), sep="\n")
  install.packages(new_packages, 
                   repos='https://cran.rstudio.com/',
                   dependencies = TRUE)
}

# check
for (i.pack in 1:length(req.pckgs)){
  suppressMessages(require(req.pckgs[i.pack], 
                           character.only = TRUE, 
                           quietly = TRUE, 
                           warn.conflicts = FALSE))
}

iold <- mapply(rv, ip()$Built) != rv(Rvers)
old_packages <- req.pckgs[(req.pckgs %in% ip()$Package[iold])]
View(ip()[ip()$Package[] %in% old_packages, ])

