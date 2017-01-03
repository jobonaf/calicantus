#packages
suppressMessages({
  .libPaths(new=unique(c("/home/giovanni/R/x86_64-pc-linux-gnu-library/3.2", .libPaths())))
  library(shiny)
#  library(shinyBS)
#  library(shinyjs)
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

# parameters
bb <- c(0,25,50,75,100,300)
cc <- c("steelblue","olivedrab","orange","red","purple")

# UI: map
ui_map <- uiOutput("ui_map")

# UI: data table
ui_data <- uiOutput("ui_data")

# UI: time series
ui_ts <- sidebarLayout(
  sidebarPanel(selectInput("colorby","color by",c("data source"="Source","same color"="same")),
               selectInput("splitby","split by",c("data source"="Source","don't split"="none")),
               selectInput("geom_type","plot type",c("boxplot","violin","jitter")),
               selectInput("emphasis","emphasis",c("period max","daily max","data source max","none")),
               sliderInput("howmany","how many peaks do you want to emphasize?",1,5,1,1,ticks = FALSE),
               checkboxInput("labels","peaks labelled with values?",FALSE),
               width=3),
  mainPanel(plotOutput("ts"))
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
    tags$li("Sicily: ", a("Anna Abita", href="mailto:abita@arpa.sicilia.it"), "(ARPA Sicilia)"),
    tags$li("Tuscany: ", a("Marco Stefanelli", href="mailto:m.stefanelli@arpat.toscana.it"), "(ARPAT)")
  ),
  h3("code"),
  p(code("calicantus"), " is open source and available", a("here", href="https://github.com/jobonaf/calicantus"))
)


# UI: login
Logged = FALSE
ui_login <- function(){
  tagList(
    div(id = "login",
        wellPanel(textInput("Usr", "user:", "user"),
                  passwordInput("Pwd", "password:", "password"),
                  br(),actionButton("Login", "Log in")))
    ,tags$style(type="text/css", "#login {font-size:12px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
  )}

# UI: webpage with tabs
ui_main <- function(){tagList(tabPanel("data",ui_data),
                              tabPanel("map",ui_map),
                              tabPanel("timeseries",ui_ts),
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
        div(class="outer",do.call(navbarPage,c(inverse=TRUE,title = "[calicantus]",ui_main())))
      })
      print(ui)
    }
  })
  
  # UI: data table
  output$ui_data <- renderUI({
    sidebarLayout(
      sidebarPanel(dateRangeInput("daterange","period of interest",
                                  start=Sys.Date()-10,
                                  end=Sys.Date()-1,
                                  max = Sys.Date()-1),
                   width=3),
      mainPanel(dataTableOutput("df"))
    )
  })
  
  observe({
    updateDateInput(session=session, inputId = "day", 
                    value=max(min(input$daterange[2],input$day),input$daterange[1]))
  })
  
  # UI: map
  output$ui_map <- renderUI({
    sidebarLayout(
      sidebarPanel(dateRangeInput("daterange2","period of interest",
                                  start=input$daterange[1],
                                  end=input$daterange[2],
                                  max = Sys.Date()-1),
                   dateInput("day", "day of interest", value=max(input$daterange), 
                             min = min(input$daterange), max = max(input$daterange)),
                   width=3),
      mainPanel(leafletOutput("Map", width="100%", height="600px"))
    )
  })
  
  observe({
    updateDateRangeInput(session=session, inputId = "daterange",
                         start=input$daterange2[1],
                         end=input$daterange2[2])
    updateDateInput(session=session, inputId = "day", 
                    value=max(min(input$daterange2[2],input$day),input$daterange2[1]))
  })
  
  # load data
  dataOfPeriod <- reactive({
    dd <- as.character(seq.Date(input$daterange[1],input$daterange[2],"1 day"))
    ff <- NULL
    for(d in dd) {
      ff <- c(ff,system(paste0("ls /home/giovanni/R/projects/calicantus/data/obs-data/",
                               format(as.Date(d),"%Y/%m/%d/%Y%m%d_*.rda 2>/dev/null")),
                        intern=TRUE))
    }
    Dat <- NULL
    nf <- length(ff)
    for (i in 1:nf) {
      load(ff[i])
      Dat <- bind_rows(Dat,dat)
    }
    Dat$Value <- round(Dat$Value)
    Dat <- arrange(Dat, desc(Value))
    return(Dat)
  })
  
  # data table
  output$df <- renderDataTable({
    dataOfPeriod()
  }, options = list(pageLength = 20))
  
  # peaks detection
  dataWithPeaks <- reactive({
    dataOfPeriod() %>% mutate(Station=paste0(Name," (",Source,")")) -> Dat
    if(input$emphasis=="none") {
      Dat$Emphasis <- Dat$Peak <- NA
    } else {
      if(input$emphasis=="period max") Dat %>% group_by(Pollutant) -> Dat  
      if(input$emphasis=="daily max") Dat %>% group_by(Pollutant,Day) -> Dat  
      if(input$emphasis=="data source max") Dat %>% group_by(Pollutant,Source) -> Dat  
      Dat %>% mutate(Rank=1+n()-rank(Value,na.last=FALSE,ties.method="max"), Peak=(Rank<=input$howmany)) %>% left_join(.,Dat) -> Dat
      Dat %>% group_by(Pollutant,Name,Source) %>% mutate(Emphasis=max(Peak,na.rm=T)) %>% left_join(.,Dat) -> Dat
    }
    Dat$Emphasis[!Dat$Emphasis] <- NA
    Dat$Peak[!Dat$Peak] <- NA
    Dat
  })
  
  # plot time series
  output$ts <- renderPlot({
    Aes <- aes_string(x="Day", y="Value", group="Day")
    if(input$colorby!="same") Aes <- modifyList(Aes, aes_string(col=input$colorby))
    pl <- ggplot(data = dataWithPeaks(), Aes)
    geom_ <- get(paste0("geom_",input$geom_type))
    pl <- pl + theme_bw() + geom_(width=0.4)
    if(input$splitby!="none") pl <- pl + facet_wrap(input$splitby) 
    if(input$emphasis!="none") pl <- pl +
      geom_line(aes(x=as.numeric(ordered(Day))*Emphasis, y=Value*Emphasis, group=Station)) + 
      geom_point(aes(x=as.numeric(ordered(Day))[Peak], y=Value[Peak]), shape=21, fill="white", size=5) +
      geom_point(aes(x=as.numeric(ordered(Day))[Peak], y=Value[Peak], shape=Station[Peak], group=Station[Peak]), size=2.5) +
      scale_shape_manual(values=c(65:90,97:122))
    if(input$labels) pl <- pl +
      geom_text(aes(x=as.numeric(ordered(Day))[Peak]-0.2, y=Value[Peak], label=Value), hjust=1, vjust=0.3)
    pl <- pl + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    pl
  },height = function(){ifelse(input$splitby=="none",600,900)})

  # data selection for the map (single day)
  dataOfDay <- reactive({
    Dat <- dataOfPeriod()
    Dat[Dat$Day==as.character(input$day),]
  })
  
  # daily map
  output$Map <- renderLeaflet({
    na.omit(dataOfDay()) -> Dat
    lon0 <- (min(Dat$Lon)+max(Dat$Lon))/2
    lat0 <- (min(Dat$Lat)+max(Dat$Lat))/2
    
    leaflet() %>% setView(lon0, lat0, 6) %>% addTiles()  %>% addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(data=Dat
                       ,lng=~Lon, lat=~Lat
                       ,radius=6 
                       ,color=as.character(cut(Dat$Value,
                                               breaks=bb,
                                               labels=cc))
                       ,stroke=FALSE, fillOpacity=0.5
                       ,popup= ~htmlEscape(paste0(Name,": ",Value))
      )
  })
}

shinyApp(ui = ui, server = server)