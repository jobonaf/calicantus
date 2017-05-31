# preliminar --------------------------------------------------------------

options(shiny.sanitize.errors = FALSE)

#packages
suppressMessages({
  pkgO <- names(sessionInfo()$otherPkgs)
  pkg1 <- c(pkgO)
  pkg2 <- setdiff(pkg1,"shiny")
  if(length(pkg2)>0) lapply(paste0('package:',pkg2), detach, character.only = TRUE, unload = TRUE)
  lib0 <- "/usr/lib/R/library"
  lib1 <- "/home/giovanni/R/x86_64-pc-linux-gnu-library/3.4"
  .libPaths(unique(c(.libPaths(),lib1)))
  library("shinyBS")
  library("markdown")
  library("plyr")
})

# ui ----------------------------------------------------------------------

# UI: webpage with tabs
ui <- navbarPage(
  title = "[calicantus]", inverse = TRUE,
  tabPanel("home", 
           fluidRow(
             column(
               6,offset = 1,
               includeMarkdown("/home/giovanni/R/projects/calicantus/shiny/intro/info.md"),
               actionButton(inputId = "login",label = "Log in",icon = icon("sign-in"), width="120",
                            onclick ="location.href='https://sdati.arpae.it/calicantus';"),
               actionButton(inputId = "require_registration",label = "Register",icon = icon("user-plus"), width="120"),
               actionButton(inputId = "require_participation",label = "Participate",icon = icon("database"), width="120"),
               bsTooltip("login","If you are already registered, you can log in here"),
               bsTooltip("require_registration","If you want to register to the service, please click here and fill the form"),
               bsTooltip("require_participation","If you want to participate as data provider, please click here and fill the form. Please note that before becoming data provider, you should be registered as end-user.")
             ),
             column(3,offset = 0, includeMarkdown("/home/giovanni/R/projects/calicantus/shiny/intro/figs.md"))
           ),
           
           fluidRow(
             
             
             conditionalPanel(
               "input.require_registration==true",
               column(
                 6,offset = 1,
                 h3("Register"),
                 p("If you want to register as",
                   strong("end-user"),
                   "to the service, please fill the following form and read carefully the data policy."),
                 fluidRow(
                   column(6, textInput("name","name",width = "100%")),
                   column(6, textInput("surname","surname",width = "100%"))
                 ),
                 fluidRow(
                   column(6, textInput("email","e-mail",width = "100%"),
                          textInput("institution","institution",width = "100%"),
                          textInput("department","department or working group",
                                    width = "100%")
                   ),
                   column(6, textInput("phone","phone",placeholder = "+39-06-123456",width = "100%"),
                          selectInput("category","category",
                                      c("environmental agency",
                                        "other public administration",
                                        "university/reasearch institute",
                                        "private air quality modeller"),width = "100%")
                   )
                 ),
                 fluidRow(
                   helpText("Please specify for which purpose(s) you will use data and graphical products."),
                   column(4,
                          checkboxGroupInput("purpose","purpose(s)",
                                             c("episode analysis",
                                               "air quality forecast",
                                               "research",
                                               "air quality management",
                                               "model evaluation",
                                               "other"))
                   ),
                   column(8,
                          conditionalPanel(
                            "input.purpose.indexOf('other') != -1",
                            helpText("Please specify the other purpose(s)."),
                            textInput("other_purpose","",width = "100%"))
                   )
                 ),
                 actionButton("show_policy","show me the data policy",icon=icon("eye")),
                 bsModal("policy",title = "Data Policy",trigger = "show_policy", size="large",
                         includeMarkdown("/home/giovanni/R/projects/calicantus/shiny/intro/policy.md"),
                         tableOutput("use_policy")
                 ),
                 fluidRow(
                   column(6,
                          checkboxInput("accept_policy","I agree to the data use policy.")),
                   column(6,
                          conditionalPanel("input.accept_policy==true",
                                           actionButton("register","register now",icon=icon("sign-in"))),
                          bsModal("registration_success",title = "Your registration was successful.",trigger = "register", size="small",
                                  p("Within one week you will receive the login credentials. Otherwise, please contact",
                                    a("the platform manager",href="mailto:giovanni.bonafe@arpa.fvg.it"),
                                    ".")
                          )
                   )
                 )
               )
             ),
           

             conditionalPanel(
               "input.require_participation==true",
               column(
                 6,offset = 1,
                 h3("Participate"),
                 p("If you want to participate as",
                   strong("data provider"),
                   "to the service, please check the box and fill the following form.",
                   "Please note that before becoming data provider, you should be registered as end-user."),
                 checkboxInput("data_provider","I would like to participate as data provider"),
                 fluidRow(
                   column(4,
                          textInput("institution_short","institution (acronym)",width = "100%")),
                   column(8,
                          textInput("institution_long","institution (full name)",width = "100%"))
                 ),
                 helpText("Please identify one or two contact persons."),
                 fluidRow(
                   column(6,
                          textInput("name1","name and surname #1",width = "100%"),
                          textInput("name2","name and surname #2",width = "100%")),
                   column(6,
                          textInput("email1","e-mail #1",width = "100%"),
                          textInput("email2","e-mail #2",width = "100%"))
                 ),
                 helpText("Please specify the policy of access for your data."),
                 checkboxGroupInput("access_allowed",width="100%",
                                    "which users will be allowed to access your data, besides environmental agencies?",
                                    c("public administrations",
                                      "universities, reasearch institutes",
                                      "private air quality modellers")),
                 helpText("Please specify the policy of use for the products (plots, maps) made with your data.",
                          "For which purposes do you authorize the use?"),
                 fluidRow(
                   column(6,selectInput("use_admin","support to administrators/decision makers",
                                        c("yes","no","ask me before use"),width = "100%")),
                   column(6,selectInput("use_tech","technical reports",
                                        c("yes","no","ask me before use"),width = "100%"))
                 ),
                 fluidRow(
                   column(6,selectInput("use_scient","workshops, scientific conferences and papers",
                                        c("yes","no","ask me before use"),width = "100%"))
                 ),
                 actionButton("policy_submit","submit",icon=icon("paper-plane")),
                 br(),
                 br(),
                 bsModal("policy_success",title = "Welcome in calicantus.",trigger = "policy_submit", size="small",
                         p("Thanks for your cooperation. We will contact you soon to agree the details of data sharing.",
                           "For any question, please contact",
                           a("the platform manager",href="mailto:giovanni.bonafe@arpa.fvg.it"),
                           ".")
                 )
               )
             )
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
                                        use_scient=input$use_scient
                                        ),
                             file=paste0("/home/giovanni/R/projects/calicantus/config/policy-preferences/",
                                         format(Sys.time(),"%Y%m%d%H%M%S"),".csv"),
                             fileEncoding="UTF-8")
               })
}

shinyApp(ui = ui, server = server)