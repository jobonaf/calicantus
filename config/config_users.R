# authorized data sources for each category of user:
# for Environmental Protection Agencies
au_EnvAgency <- c("AZO-Croatia")
# for other Public Administrations
au_PublAdmin <- c("AZO-Croatia")
# for Universities and Researchers
au_Research  <- c("AZO-Croatia")
# for Private Modellers
au_PrivModel <- c("AZO-Croatia")



if(input$Usr=="usr1") {
  auth_sources <- c("EPA-Botswana","EPA-Namibia")
}else if(input$Usr=="usr2"){
  auth_sources <- c("EPA-Botswana","EPA-Kenya")
}else{
  auth_sources <- NULL
}
