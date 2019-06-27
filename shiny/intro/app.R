
# preliminar --------------------------------------------------------------

options(shiny.sanitize.errors = FALSE)

# detach packages
basic.packages <- c("stats","graphics","grDevices", "shiny",
                    "utils","datasets","methods","base")
basic.packages <- paste0("package:", basic.packages)
package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
package.list <- setdiff(package.list,basic.packages)
if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)

# load packages
#suppressMessages({
  lib1 <- "/home/giovanni/R/x86_64-pc-linux-gnu-library/3.5"
  .libPaths(unique(c(.libPaths(),lib1)))
  libs <- c("httpuv","shinyBS","plyr","markdown")
  require_last <- function(package) {
    ip <- as.data.frame(installed.packages(), stringsAsFactors = F)
    ip <- ip[ip$Package==package,]
    ip <- ip[order(ip$Built, decreasing = T), ]
    lib_loc <- ip$LibPath[1]
    ok <- require(package, lib.loc = lib_loc, character.only = T)
    cat(paste0("package ",package,c(" loaded"," not available")[2-ok]),sep="\n")
  }
  lapply(libs, require_last) -> devnull
#})

#scripts
source("/home/giovanni/R/projects/calicantus/shiny/forms/forms.R")

# ui ----------------------------------------------------------------------

# UI: webpage with tabs
ui <- navbarPage(
  title = "[calicantus]", inverse = TRUE,
  tabPanel("home", 
           fluidRow(
             column(
               6,offset = 0,
               wellPanel(includeMarkdown("/home/giovanni/R/projects/calicantus/shiny/intro/info.md")),
               bsButton(type="action", inputId = "login",label = "Log in",icon = icon("sign-in"), width="120",
                            onclick ="location.href='https://sdati.arpae.it/calicantus';",
                            style = "primary"),
               bsButton(type="action", inputId = "require_registration",label = "Register",
                        icon = icon("user-plus"), width="120",
                        style = "primary"),
               bsButton(type="action", inputId = "require_participation",label = "Participate",
                        icon = icon("database"), width="120",
                        style = "primary"),
               bsTooltip("login","If you are already registered, you can log in here"),
               bsTooltip("require_registration","If you want to register to the service, please click here and fill the form"),
               bsTooltip("require_participation","If you want to participate as data provider, please click here and fill the form. Please note that before becoming data provider, you should be registered as end-user."),
               br(),
               includeMarkdown("/home/giovanni/R/projects/calicantus/shiny/intro/info2.md")
             ),
             column(3,offset = 0, includeMarkdown("/home/giovanni/R/projects/calicantus/shiny/intro/figs.md"))
           ),
           
           fluidRow(
             registration_form(),
             participation_form()
           )
           
  )
)



# server ------------------------------------------------------------------

server <- function(input, output, session) {
  output$use_policy <- renderTable({
    pol <- read.csv("/home/giovanni/R/projects/calicantus/data/data-sources/policy.csv",stringsAsFactors = F)
    Pol <- data.frame(pol[,c("data_source","use_adm_support","use_tech_rep","use_scientific")])
    colnames(Pol) <- c("data provider","support to administrators in decision making",
                          "technical reports","scientific workshops, conferences and papers")
    Pol[Pol=="yes"] <- "allowed"
    Pol[Pol=="no"] <- "not allowed"
    Pol[Pol=="req"] <- "ask the data provider"
    Pol[Pol==""] <- "not yet defined, ask the data provider"
    Pol
  },striped=T,rownames=F)
  
  observeEvent(eventExpr = input$register,
               handlerExpr = {
                 write.table(sep=",",row.names = F,
                             data.frame(name=input$name,
                                        surname=input$surname,
                                        institution=input$institution,
                                        department=input$department,
                                        email=input$email,
                                        phone=input$phone,
                                        category=input$category,
                                        purpose=paste(input$purpose,collapse="|"),
                                        other_purpose=input$other_purpose),
                             file=paste0("/home/giovanni/R/projects/calicantus/config/registrations/",
                                         format(Sys.time(),"%Y%m%d%H%M%S"),".csv"),
                             fileEncoding="UTF-8")
               })
  
  observeEvent(eventExpr = input$policy_submit,
               handlerExpr = {
                 write.table(sep=",",row.names = F,
                             data.frame(institution_short=input$institution_short,
                                        institution_long=input$institution_long,
                                        name1=input$name1,
                                        email1=input$email1,
                                        name2=input$name2,
                                        email2=input$email2,
                                        access_allowed=paste(input$access_allowed,collapse="|"),
                                        use_admin=input$use_admin,
                                        use_tech=input$use_tech,
                                        use_scient=input$use_scient,
                                        data_type=paste(input$data_type,collapse="|")
                                        ),
                             file=paste0("/home/giovanni/R/projects/calicantus/config/policy-preferences/",
                                         format(Sys.time(),"%Y%m%d%H%M%S"),".csv"),
                             fileEncoding="UTF-8")
               })
}

shinyApp(ui = ui, server = server)