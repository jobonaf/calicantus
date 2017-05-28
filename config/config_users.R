authorizedSources <- function(usr){
  userInfo <- read.csv("/home/giovanni/R/projects/calicantus/config/config_users.csv", stringsAsFactors = F,strip.white = T)
  sourcesPolicy <- read.csv("/home/giovanni/R/projects/calicantus/data/data-sources/policy.csv", stringsAsFactors = F,strip.white = T)
  userCategory <- userInfo$category[match(usr,userInfo$user)[1]]
  if(is.na(userCategory)) {
    aS <- NA
  } else if(userCategory=="EnvAgency") {
    aS <- sourcesPolicy$data_source
  } else if (userCategory=="PublAdmin") {
    aS <- sourcesPolicy$data_source[which(sourcesPolicy$user_publadmin %in% c("yes","tmp"))]
  } else if (userCategory=="Research") {
    aS <- sourcesPolicy$data_source[which(sourcesPolicy$user_research %in% c("yes","tmp"))]
  } else if (userCategory=="PrivModel") {
    aS <- sourcesPolicy$data_source[which(sourcesPolicy$user_private %in% c("yes","tmp"))]
  }
  return(aS)
}
