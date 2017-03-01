policyBySource <- function(file="/home/giovanni/R/projects/calicantus/data/data-sources/policy.csv") {
  read.table(file,stringsAsFactors = F, sep=",", header=T)->pp
  ns <- nrow(pp)
  users <- c("public administrators and decisors (for technical purposes)",
             "universities and researchers (for scientific purposes)",
             "everyone (for models evaluation only)")
  use <- c("technical support to the administrators",
           "technical reports",
           "scientific papers, presentations in scientific conferences, workshops, seminars")
  out <- rep("",ns)
  for (i in 1:ns) {
    out[i] <- paste0(pp[i,1],": ")
    if (sum(pp[i,-1]=="")<6) {
      Users <- paste(
        paste0("Access allowed to ",
               paste(users[pp[i,2:4]=="yes"],collapse=", "),".")[any(pp[i,2:4]=="yes")],
        paste0("Temporary access allowed to ",
               paste(users[pp[i,2:4]=="tmp"],collapse=", "),".")[any(pp[i,2:4]=="tmp")],
        sep=" ")
      Use <- paste(
        paste0("Products and data can be used for ",
               paste(use[pp[i,5:7]=="yes"],collapse=", "),".")[any(pp[i,5:7]=="yes")],
        paste0("Products and data can be used on request (user must ask the data provider for permission) for ",
               paste(use[pp[i,5:7]=="req"],collapse=", "),".")[any(pp[i,5:7]=="req")],
        sep=" ")
      out[i] <- paste0(out[i],Users," ",Use) 
    } else {
      out[i] <- paste0(out[i],"to be defined") 
    }
  }
  out <- paste(out,collapse="\n")
  return(out)
}