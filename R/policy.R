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

sources2permission <- function(sources,
                               use=c("adm_support", "tech_rep", "scientific"),
                               file="/home/giovanni/R/projects/calicantus/data/data-sources/policy.csv") {
  read.table(file,stringsAsFactors = F, sep=",", header=T)->pp
  use <- match.arg(use)
  pp <- pp[pp[,1] %in% sources,c("data_source",paste0("use_",use))]
  Perm <- unique(pp[,2])
  out <- list()
  for(i in 1:length(Perm)) {
    out[[i]] <- pp[pp[,2]==Perm[i],1]
  }
  names(out) <- Perm
  return(out)
}

policyByPurpose <- function(sources) {
  if(length(sources)>0) {
    use <- c("technical support to the administrators",
             "technical reports",
             "scientific papers, presentations in scientific conferences, workshops, seminars")
    usecode <- c("adm_support", "tech_rep", "scientific")
    nu <- length(usecode)
    out <- "Depending on the purpose, you need different permissions to use these plots.<br>"
    for (i in 1:nu) {
      pp <- sources2permission(sources,use=usecode[i])
      out <- paste0(out,"<br>For <strong>",use[i],"</strong>:<br>")
      oo <-NULL
      if("no" %in% names(pp)) oo <- c(oo,paste0("you cannot use data from ",paste(pp$no,collapse=", ")))
      if("req" %in% names(pp)) oo <- c(oo,paste0("you must ask for permission to use data from ",paste(pp$req,collapse=", ")," (see contact persons list)"))
      if("yes" %in% names(pp)) oo <- c(oo,paste0("you can use data from ",paste(pp$yes,collapse=", ")," without asking for permission"))
      out <- paste0(out,paste(oo,collapse=";<br>"),".<br>")
    }
    return(out)
  }
}

permissionButton <- function(id,sources) {
  if(length(sources)>0) {
    p1 <- sources2permission(sources = sources, use = "adm_support")
    p2 <- sources2permission(sources = sources, use = "tech_rep")
    p3 <- sources2permission(sources = sources, use = "scientific")
    nn <- unique(c(names(p1),names(p2),names(p3)))
    st <- "success"
    if("req" %in% nn) st <- "warning"
    if("no" %in% nn) st <- "danger"
    bsButton(inputId = id,label = "permissions", style = st)
  }
}
  

citeHtml <- function(files="/home/giovanni/R/projects/calicantus/CITATION",
                     pkgs=c("ggplot2","dplyr")) {
  cHtml <- NULL; cFirstAuthor <- NULL
  for(ff in files) {
    cc <- readCitationFile(ff,meta = list(Encoding="UTF-8"))
    cHtml <- c(cHtml, paste(capture.output(print(cc,style="html")),collapse=" "))
    cFirstAuthor <- c(cFirstAuthor, paste(cc$author[1]$family,cc$author[1]$given))
  }
  for(pp in pkgs) {
    cc <- citation(pp)
    cHtml <- c(cHtml, paste(capture.output(print(cc,style="html")),collapse=" "))
    cFirstAuthor <- c(cFirstAuthor, paste(cc$author[1]$family,cc$author[1]$given))
  }
  return(cHtml[order(cFirstAuthor)])
}