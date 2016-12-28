#packages
suppressMessages({
  library(shiny)
  library(shinyBS)
  library(shinyjs)
  library(leaflet)
  library(DT)
  library(rgdal)
  library(raster)
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(RColorBrewer)
  library(htmltools)
})

# credentials
source("/home/giovanni/R/projects/calicantus/config/ui_credentials.R")

# load data
load_data <- function(lday=Sys.Date()-1,fday=lday-30) {
  dd <- as.character(seq.Date(fday,lday,"1 day"))
  ff <- NULL
  for(d in dd) {
    ff <- c(ff,system(paste0("ls /home/giovanni/R/projects/calicantus/data/obs-data/",
                             format(as.Date(d),"%Y/%m/%d/%Y%m%d_*.rda 2>/dev/null")),
                      intern=TRUE))
  }
  Dat <- NULL
  for (f in ff) {
    cat(f,sep="\n")
    load(f)
    Dat <- bind_rows(Dat,dat)
  }
  Dat$Value <- round(Dat$Value)
  return(Dat)
}
Dat <- load_data()
na.omit(Dat) -> Dat
lon0 <- (min(Dat$Lon)+max(Dat$Lon))/2
lat0 <- (min(Dat$Lat)+max(Dat$Lat))/2
cat(paste(lon0,lat0))

# parameters
bb <- c(0,25,50,75,100,300)
cc <- c("steelblue","olivedrab","orange","red","purple")

# UI: map
ui_map <- fluidPage(leafletOutput("Map", width="100%", height="600px")
          ,absolutePanel(id="controls", top=100, right=100, height=200, width=100
                         ,dateInput("day", "", Sys.Date()-1)
          )
)

# UI: about
ui_about <- fluidPage(
  h3("data policy"),
  tags$ol(
    tags$li("Data are not validated."),
    tags$li("Maps and data cannot be published."),
    tags$li("Changes in the policy will be agreed between participants.")
  ),
  h3("contacts"),
  p("Platform manager: ", a("Giovanni Bonafè",  href="mailto:giovanni.bonafe@arpa.fvg.it"), "(ARPA-FVG)"),
  p("AQ data:"),
  tags$ul(
    tags$li("Emilia-Romagna: ", a("Marco Deserti", href="mailto:mdeserti@arpae.it"), "(Arpae Emilia-Romagna)"),
    tags$li("Veneto: ", a("Max Ferrario", href="mailto:mferrario@arpa.veneto.it"), "(ARPAV)"),
    tags$li("Umbria: ", a("Monica Angelucci,", href="mailto:m.angelucci@arpa.umbria.it"),
            a("Marco Vecchiocattivi", href="mailto:m.vecchiocattivi@arpa.umbria.it"), "(ARPA Umbria)"),
    tags$li("Piedmont: ", a("Stefano Bande", href="mailto:stefano.bande@arpa.piemonte.it"), "(ARPA Piemonte)"),
    tags$li("Friuli Venezia Giulia: ", a("Fulvio Stel", href="mailto:fulvio.stel@arpa.fvg.it"), "(ARPA-FVG)"),
    tags$li("Liguria: ", a("Monica Beggiato", href="mailto:monica.beggiato@arpal.gov.it"), "(ARPAL)"),
    tags$li("Lombardy: ", a("Anna Di Leo", href="mailto:a.dileo@arpalombardia.it"), "(ARPA Lombardia)"),
    tags$li("Sicily: ", a("Anna Abita", href="mailto:abita@arpa.sicilia.it"), "(ARPA Sicilia)")
  ),
  h3("code"),
  p(code("calicantus"), " is open source and available", a("here", href="https://github.com/jobonaf/calicantus"))
)


# UI: login
Logged = FALSE
ui_login <- function(){
  tagList(
    div(id = "login",
        wellPanel(textInput("Usr", "", "user"),
                  passwordInput("Pwd", "", "password"),
                  br(),actionButton("Login", "Log in")))
    ,tags$style(type="text/css", "#login {font-size:12px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
  )}

# UI: webpage with tabs
ui_main <- function(){tagList(tabPanel("map",ui_map),
                              tabPanel("about",ui_about))}
ui <- (htmlOutput("page"))

## server
server <- function(input, output, session) {
  USER <- reactiveValues(Logged = Logged)
  # check credentials
  observe({ 
    if (!USER$Logged && !is.null(input$Login) && input$Login>0) {
          usr <- isolate(input$Usr)
          pwd <- isolate(input$Pwd)
          Id.usr <- which(ui_usr == usr)
          Id.pwd <- which(ui_pwd == pwd)
          if (length(Id.usr)==1 & length(Id.pwd)==1 && Id.usr==Id.pwd) USER$Logged <- TRUE
    }    
  })
  observe({
    # login page
    if (!USER$Logged) {
      output$page <- renderUI({
        div(class="outer",do.call(bootstrapPage,c("",ui_login())))
      })
    }
    # main page
    if (USER$Logged) {
      output$page <- renderUI({
        div(class="outer",do.call(navbarPage,c(inverse=TRUE,title = "calicantus",ui_main())))
      })
      print(ui)
    }
  })
  
  
  # data selection
  selectedData <- reactive({
    Dat[Dat$Day==as.character(input$day),]
  })
  
  # daily map
  output$Map <- renderLeaflet({
    leaflet() %>% setView(lon0, lat0, 6) %>% addTiles()  %>% addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(data=selectedData()
                       ,lng=~Lon, lat=~Lat
                       ,radius=6 
                       ,color=as.character(cut(selectedData()$Value,
                                               breaks=bb,
                                               labels=cc))
                       ,stroke=FALSE, fillOpacity=0.5
                       ,popup= ~htmlEscape(paste0(Name,": ",Value))
      )
  })
}

shinyApp(ui = ui, server = server)