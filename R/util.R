# generate records to be appended to config/ui_credentials.R
# (and text for email)
string_users <- function(users=read.csv(file = "/home/giovanni/R/projects/calicantus/data/data-sources/contacts.csv",
                                        stringsAsFactors = F)$data.source,
                         email=paste(read.csv(file = "/home/giovanni/R/projects/calicantus/data/data-sources/contacts.csv",
                                              stringsAsFactors = F)$e.mail..1,
                                     read.csv(file = "/home/giovanni/R/projects/calicantus/data/data-sources/contacts.csv",
                                              stringsAsFactors = F)$e.mail..2,
                                     sep="; "),
                         simple=T) {
  if(simple) sapply(stringi::stri_split_fixed(users,"-"), 
                    function(x) tolower(paste(x[(length(x)-1):length(x)],
                                              collapse="_"))) -> users
  nu <- length(users)
  pwds <- rep("",nu)
  for (i in 1:nu) pwds[i] <- stringi::stri_rand_strings(n=1, length=8, pattern="[A-Za-z0-9]")
  for (i in 1:nu) {
    nc <- nchar(users[i])
    blanks <- paste(rep(" ",max(0,max(nchar(users))-nc)),collapse="")
    out <- paste0('ui_usr<-append(ui_usr,"',users[i],'"',blanks,
                  ');ui_pwd<-append(ui_pwd,"',pwds[i],'")')
    cat(out,sep="\n")
  }
  for (i in 1:nu) {
    out <- paste0('\nto: ',email[i],'\n',
                  'user: "',users[i],'"\n',
                  'password: "',pwds[i],'"')
    cat(out,sep="\n")
  }
}


# encrypt a file
encrypt_file <- function(filein, fileout=paste0(filein,".enc"), pwd=NULL) {
  if(is.null(pwd)) pwd <- readline("password?")
  command <- paste0("openssl enc -in ",filein," -aes-256-cbc -pass stdin > ",fileout)  
  system(command, input=pwd)
}

# decrypt a file
decrypt_file <- function(filein, fileout=paste0(filein,".dec"), pwd=NULL) {
  if(is.null(pwd)) pwd <- readline("password?")
  command <- paste0("openssl enc -in ",filein," -d -aes-256-cbc -pass stdin > ",fileout)  
  system(command, input=pwd)
}
