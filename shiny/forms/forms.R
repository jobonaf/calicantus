registration_form <- function(){
  
  bsModal(id = "registration_form", title = "Registration", trigger = "require_registration", size = "large",
          column(
            12,offset = 0,
            p("If you want to register as",
              strong("end-user"),
              "to the service, please fill the following form and read carefully the data policy and the privacy policy below.",
              em(" Please note that all fields marked with an asterisk (*) are required.")),
            fluidRow(
              column(6, textInput("name","name*",width = "100%")),
              column(6, textInput("surname","surname*",width = "100%"))
            ),
            fluidRow(
              column(6, textInput("email","e-mail*",width = "100%"),
                     textInput("institution","institution*",width = "100%"),
                     textInput("department","department or working group",
                               width = "100%")
              ),
              column(6, textInput("phone","phone*",placeholder = "+39-06-123456",width = "100%"),
                     selectInput("category","category*",
                                 c("",
                                   "environmental agency",
                                   "other public administration",
                                   "university/reasearch institute",
                                   "private air quality modeller"),width = "100%")
              )
            ),
            fluidRow(
              helpText("Please specify for which purpose(s) you will use data and graphical products."),
              column(4,
                     checkboxGroupInput("purpose","purpose(s)*",
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
            bsCollapse(id = "policy_collapse", open = NULL,
                       bsCollapsePanel(title = "Data Policy", style = "warning", 
                                       includeMarkdown("/home/giovanni/R/projects/calicantus/shiny/intro/policy.md"),
                                       tableOutput("use_policy")
                       )
            ),
            bsCollapse(id = "privacy_it_collapse", open = NULL,
                       bsCollapsePanel(title = "Informativa sulla privacy", style = "warning", 
                                       includeMarkdown("/home/giovanni/R/projects/calicantus/shiny/intro/privacy_IT.md")
                       )
            ),
            bsCollapse(id = "privacy_en_collapse", open = NULL,
                       bsCollapsePanel(title = "Privacy Policy", style = "warning", 
                                       includeMarkdown("/home/giovanni/R/projects/calicantus/shiny/intro/privacy_EN.md")
                       )
            ),
            fluidRow(
              column(6,
                     checkboxInput("accept_policy","I agree to the data use policy.")),
              column(6,
                     checkboxInput("accept_privacy","I agree to the privacy policy.")),
              column(6,
                     conditionalPanel(paste("input.accept_policy==true",
                                            "input.accept_privacy==true",
                                            "input.surname!==''",
                                            "input.name!==''",
                                            "input.institution!==''",
                                            "input.email!==''",
                                            "input.phone!==''",
                                            "input.category!==''",
                                            "input.purpose.length>0",
                                            sep=" && "),
                                      actionButton("register","register now",icon=icon("sign-in"))),
                     bsModal("registration_success",title = "Your registration was successful.",trigger = "register", size="small",
                             p("Within one week you will receive the login credentials. Otherwise, please contact",
                               a("the platform manager",href="mailto:giovanni.bonafe@arpa.fvg.it"),
                               ".")
                     )
              )
            )
          )
  )
}






participation_form <- function(){
  
  bsModal(id = "participation_form", title = "Participation", trigger = "require_participation", size = "large",
          column(
            12,offset = 0,
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
            helpText("Please specify the type of data you intend to provide."),
            checkboxGroupInput("data_type",width="100%",
                               "data type",
                               c("observations",
                                 "forecasts")),
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
}