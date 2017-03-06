# preliminar --------------------------------------------------------------

#packages
suppressMessages({
  pkgO <- names(sessionInfo()$otherPkgs)
  pkg1 <- c(pkgO)
  pkg2 <- setdiff(pkg1,"shiny")
  if(length(pkg2)>0) lapply(paste0('package:',pkg2), detach, character.only = TRUE, unload = TRUE)
  library(shinyBS, lib.loc="/home/giovanni/R/x86_64-pc-linux-gnu-library/3.2")
  library(markdown)
})

# ui ----------------------------------------------------------------------

# UI: webpage with tabs
ui <- navbarPage(title = "[calicantus]", inverse = TRUE,
                 tabPanel("info",
                          column(4,offset = 2,
                                 includeMarkdown("/home/giovanni/R/projects/calicantus/shiny/intro/info.md")
                          ),
                          column(3,offset = 0.2,
                                 includeMarkdown("/home/giovanni/R/projects/calicantus/shiny/intro/figs.md")
                          )
                 ),
                 tabPanel("access",
                          column(5,offset = 2,
                                 h3("Log in"),
                                 p("If you already registered to",
                                   strong("calicantus"),
                                   "and got by e-mail the confirmation of the activation of your account, you can",
                                   a("log in here", href="https://shiny.arpae.it/calicantus"),
                                   "."),
                                 h3("Register"),
                                 p("If you want to register as",
                                   strong("end-user"),
                                   "to the service, fill the following form."),
                                 fluidRow(
                                   column(6,
                                          textInput("name","name"),
                                          textInput("email","e-mail"),
                                          textInput("institution","institution"),
                                          checkboxGroupInput("purpose","purpose(s)",
                                                             c("episode analysis",
                                                               "air quality forecast",
                                                               "research",
                                                               "air quality management",
                                                               "other"))
                                   ),
                                   column(6,
                                          textInput("surname","surname"),
                                          textInput("phone","phone",value = "+39-06-123456"),
                                          selectInput("category","category",
                                                      c("environmental agency",
                                                        "other public administration",
                                                        "university/reasearch institute",
                                                        "private air quality modeller")),
                                          conditionalPanel("input.purpose.indexOf('other') != -1",
                                                           helpText("Please specify the other purpose(s)."),
                                                           textInput("other_purpose",""))
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
                                 ),
                                 h3("Participate"),
                                 p("If you want to participate as",
                                   strong("data provider"),
                                   "to the service, please check the box and fill the following form."),
                                 fluidRow(
                                   checkboxInput("data_provider","I would like to participate as data provider"),
                                   conditionalPanel("input.data_provider==true",
                                                    helpText("Please identify one or two contact persons."),
                                                    fluidRow(
                                                      column(6,
                                                             textInput("name1","name and surname #1"),
                                                             textInput("name2","name and surname #2")),
                                                      column(6,
                                                             textInput("email1","e-mail #1"),
                                                             textInput("email2","e-mail #2"))
                                                    ),
                                                    helpText("Please specify the policy of access for your data."),
                                                    checkboxGroupInput("access_allowed",width="100%",
                                                                       "which users will be allowed to access your data, besides environmental agencies?",
                                                                       c("public administrations",
                                                                         "universities, reasearch institutes",
                                                                         "private air quality modellers")),
                                                    helpText("Please specify the policy of use for the products (plots, maps) produced with your data.",
                                                             "For which purposes do you authorize the use?"),
                                                    fluidRow(
                                                      column(6,selectInput("use_admin","support to administrators/decision makers",
                                                                           c("yes","no","ask me before use"))),
                                                      column(6,selectInput("use_tech","technical reports",
                                                                           c("yes","no","ask me before use")))
                                                    ),
                                                    fluidRow(
                                                      column(6,selectInput("use_scient","workshops, scientific conferences and papers",
                                                                           c("yes","no","ask me before use")))
                                                    ),
                                                    actionButton("policy_submit","submit",icon=icon("paper-plane")),
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
                             data.frame(institution=input$institution,
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