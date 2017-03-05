
# preliminar --------------------------------------------------------------

#packages
suppressMessages({
  pkgO <- names(sessionInfo()$otherPkgs)
  pkg1 <- c(pkgO)
  pkg2 <- setdiff(pkg1,"shiny")
  if(length(pkg2)>0) lapply(paste0('package:',pkg2), detach, character.only = TRUE, unload = TRUE)
  library(shiny, lib.loc = "/usr/lib/R/library")
  library(base64enc, lib.loc="/home/giovanni/R/x86_64-pc-linux-gnu-library/3.2")
  library(lubridate, lib.loc = "/usr/lib/R/library")
  library(dplyr, lib.loc = "/usr/lib/R/library")
  library(plyr, lib.loc = "/home/giovanni/R/x86_64-pc-linux-gnu-library/3.2")
  library(shinyjs, lib.loc = "/usr/lib/R/library")
  library(leaflet, lib.loc = "/home/giovanni/R/x86_64-pc-linux-gnu-library/3.2")
  library(raster, lib.loc = "/usr/lib/R/library")
  library(data.table, lib.loc = "/home/giovanni/R/x86_64-pc-linux-gnu-library/3.2")
  library(tidyr, lib.loc = "/usr/lib/R/library")
  library(scales, lib.loc = "/home/giovanni/R/x86_64-pc-linux-gnu-library/3.2")
  library(ggplot2, lib.loc = "/home/giovanni/R/x86_64-pc-linux-gnu-library/3.2")
  library(ggrepel, lib.loc = "/home/giovanni/R/x86_64-pc-linux-gnu-library/3.2")
  library(RColorBrewer, lib.loc = "/usr/lib/R/library")
  library(maps, lib.loc = "/usr/lib/R/library")
  library(cluster, lib.loc = "/usr/lib/R/library")
})

# credentials
source("/home/giovanni/R/projects/calicantus/config/ui_credentials.R")

# style
progressBarStyle <- ".progress-striped .bar {
background-color: #149bdf;
background-image: -webkit-gradient(linear, 0 100%, 100% 0, color-stop(0.25, rgba(255, 255, 255, 0.6)), color-stop(0.25, transparent), color-stop(0.5, transparent), color-stop(0.5, rgba(255, 255, 255, 0.6)), color-stop(0.75, rgba(255, 255, 255, 0.6)), color-stop(0.75, transparent), to(transparent));
background-image: -webkit-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
background-image: -moz-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
background-image: -o-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
background-image: linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
-webkit-background-size: 40px 40px;
-moz-background-size: 40px 40px;
-o-background-size: 40px 40px;
background-size: 40px 40px;
}"

# proxy 
source("/home/giovanni/R/projects/calicantus/config/config_proxy.R")
Sys.setenv(http_proxy=paste0("http://",proxy_usr,":",proxy_pwd,"@",proxy_addr,":",proxy_port,"/"))
Sys.setenv(https_proxy=paste0("http://",proxy_usr,":",proxy_pwd,"@",proxy_addr,":",proxy_port,"/"))

# geographic data
MapData <- map_data(map = "world", 
                    region = c("Italy","Slovenia","Croatia","Serbia","San Marino","Vatican",
                               "France","Switzerland","Austria","Germany","Bosnia and Herzegovina",
                               "Tunisia","Malta","Montenegro","Hungary","Albania","Greece","Algeria"))



# ui ----------------------------------------------------------------------


# UI: map
ui_map <- uiOutput("ui_map")

# UI: data table
ui_data <- uiOutput("ui_data")

# UI: time series
ui_ts <- uiOutput("ui_ts")

# UI: exceedances
ui_exc <- uiOutput("ui_exc")

# UI: clustering
ui_clu <- uiOutput("ui_clu")

# UI: models map
ui_modelmap <- uiOutput("ui_modelmap")

# UI: account info
ui_account <- uiOutput("ui_account")

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
  tableOutput("contacts"),
  h3("code"),
  p(code("calicantus"), " is open source and available", a("here", href="https://github.com/jobonaf/calicantus"))
)

# UI: debug
ui_debug <- fluidPage(
  verbatimTextOutput("summary"),
  verbatimTextOutput("sessionInfo"),
  verbatimTextOutput("uiInput")
)
  

# UI: login
Logged = FALSE
ui_login <- function(){
  tagList(
    div(id = "login",
        wellPanel(textInput("Usr", "user:", "user"),
                  passwordInput("Pwd", "password:", "password"),
                  br(),actionButton("Login", "Log in"),
                  conditionalPanel(condition = "input.Login",
                                   code("Wait, please. If dashboard doesn't appear,"),
                                   br(),code("probably user and/or password are wrong.")
                  ))),
    tags$style(type="text/css", "#login {font-size:12px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
  )}

# UI: webpage with tabs
ui_menu <- function(){
  tagList(
    tabPanel("data", ui_data),
    navbarMenu("maps"
               ,tabPanel("observed data", ui_map)
               ,tabPanel("models", ui_modelmap)
    ),
    navbarMenu("analysis"
               ,tabPanel("timeseries",ui_ts)
               ,tabPanel("exceedances",ui_exc)
               ,tabPanel("clustering",ui_clu)
    ),
    navbarMenu("more"
               ,tabPanel("about",ui_about)
               ,tabPanel("account",ui_account)
               ,tabPanel("debug",ui_debug)
    )
  )}
ui <- uiOutput("page")


# server ------------------------------------------------------------------

server <- function(input, output, session) {
  USER <- reactiveValues(Logged = Logged)
  # check credentials
  observe({ 
    if (!USER$Logged && !is.null(input$Login) && input$Login>0) {
          usr <- isolate(input$Usr)
          pwd <- isolate(input$Pwd)
          Id.usr <- which(ui_usr == usr)
          Id.pwd <- which(ui_pwd == pwd)
          if (length(Id.usr)==1 & length(Id.pwd)==1 && Id.usr==Id.pwd) {
            USER$Logged <- TRUE
          } else {
            USER$Logged <- FALSE
          }
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
        div(class="outer",do.call(navbarPage,c(inverse=TRUE,title = "[calicantus]",ui_menu())))
      })
      print(ui)
    }
  })
  
  # available data sources (personalized depending on user)
  availableSources <- reactive({
    source("/home/giovanni/R/projects/calicantus/config/config_users.R",local=TRUE)
    aS <- system("cd /home/giovanni/R/projects/calicantus/data/sites-info/; ls metadata*csv | sed s/metadata.//g | sed s/.csv//g",intern=TRUE)
    aS <- intersect(aS, auth_sources)
    aS
  })
  
  # daily indicator
  dailyInd <- reactive({
    if(input$pollutantPeriod=="PM10") dI <- "daily average"
    dI
  })
  dayInd <- reactive({
    if(input$pollutantDay=="PM10") dI <- "daily average"
    dI
  })
  
  # UI: data table
  output$ui_data <- renderUI({
    sidebarLayout(
      sidebarPanel(helpText("Here you select the period you are interested in."),
                   dateRangeInput("daterange","period of interest",
                                  start=Sys.Date()-10,
                                  end=Sys.Date()-1,
                                  max = Sys.Date()-1),
                   selectInput("pollutantPeriod",label = "pollutant", choices = c("PM10")),
                   selectInput("sources",label = "sources of data", choices = availableSources(), 
                               selected = availableSources(), multiple=TRUE, selectize=FALSE),
                   actionButton("goPeriod", label="load", icon = icon("arrow-circle-right")),
                   conditionalPanel(condition = "input.goPeriod",
                                    helpText("Please note that the table on the right side is interactive:",
                                             "you can sort it by clicking on the header,",
                                             "look for a specific value using the 'search' tool,",
                                             "filter the data by writing in the cells below the table, and so on."),
                                    downloadButton('downloadData', 'download')),
                   hr(),
                   helpText("Data are not verified and may differ from the validated data."),
                   width=3),
      mainPanel(tags$head(tags$style(HTML(progressBarStyle))),
                dataTableOutput("df"))
    )
  })
  
  # load data of period
  dataOfPeriod <- eventReactive(eventExpr = input$goPeriod, 
                                valueExpr = {
                                  dd <- as.character(seq.Date(input$daterange[1],input$daterange[2],"1 day"))
                                  ff <- NULL
                                  for(d in dd) {
                                    ff <- c(ff,system(paste0("ls /home/giovanni/R/projects/calicantus/data/obs-data/",
                                                             format(as.Date(d),"%Y/%m/%d/%Y%m%d_"),input$pollutantPeriod,
                                                             "*.rda 2>/dev/null"),
                                                      intern=TRUE))
                                  }
                                  Dat <- NULL
                                  nf <- length(ff)
                                  for (i in 1:nf) {
                                    load(ff[i])
                                    Dat <- bind_rows(Dat,dat)
                                  }
                                  Dat %>% dplyr::filter(!is.na(Name) & !is.na(Lat) & !is.na(Lon) & 
                                                          Source%in%input$sources & Source%in%availableSources())  %>%
                                    dplyr::mutate(Value=round(Value)) %>% dplyr::arrange(desc(Value)) -> Dat
                                  return(Dat)
                                })
  
  # download data of period
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data_', input$pollutantPeriod, "_",
            paste(format(as.Date(input$daterange),"%Y%m%d"),collapse="-"), '.csv', sep='')
    },
    content = function(con) {
      dataOfPeriod() %>% dplyr::arrange(Day,Source) -> data
      write.csv(data, con, fileEncoding = "latin1")
    }
  )
  
  # UI: map of observed data
  output$ui_map <- renderUI({
    sidebarLayout(
      sidebarPanel(helpText("Here you can select the day to plot in the map."),
                   dateInput("day", "day of interest", value=Sys.Date()-1, 
                             min="2014-10-10", max = Sys.Date()-1,
                             width="50%"),
                   selectInput("pollutantDay",label = "pollutant", choices = c("PM10")),
                   selectInput("scaleDay",label = "color scale", choices = c("classic","extended")),
                   actionButton("goDay", label="plot", icon = icon("arrow-circle-right")),
                   conditionalPanel(condition = "input.goDay",
                                    helpText("Please note that the map on the right side is interactive:",
                                             "you can zoom in and out with the mouse wheel or with the '+' and '-',",
                                             "and click on a marker to get name of the station and observed concentration.")),
                   hr(),
                   helpText("Data are not verified and may differ from the validated data."),
                   width=3),
      mainPanel(leafletOutput("Map", width="100%", height="800px"))
    )
  })
  
  # load observed data of single day
  #   dataOfDay <- eventReactive(eventExpr = input$goDay, 
  changeOfDay <- reactive({
    input$goDay
    input$day
    })
  dataOfDay <- eventReactive(
    eventExpr = changeOfDay(), #ignoreNULL = FALSE,
    valueExpr = {
      if(is.null(input) || is.null(input$day)) {
        d <- as.character(Sys.Date()-1)
      } else {
        d <- as.character(input$day)
      }
      ff <- system(paste0("ls /home/giovanni/R/projects/calicantus/data/obs-data/",
                          format(as.Date(d),"%Y/%m/%d/%Y%m%d_*.rda 2>/dev/null")),
                   intern=TRUE)
      Dat <- NULL
      nf <- length(ff)
      if(nf>0) {
        for (i in 1:nf) {
          load(ff[i])
          Dat <- bind_rows(Dat,dat)
        }
        Dat$Value <- round(Dat$Value)
        Dat <- dplyr::arrange(Dat, Value) %>% 
          dplyr::filter(as.character(Day)==d,
                        Source %in% availableSources()) %>%
          dplyr::filter(!is.na(Value) & !is.na(Lat) & !is.na(Lon)) %>%
          mutate(ValueInterval=cut(Value
                                   ,Breaks()
                                   ,ordered_result=TRUE, include.lowest=TRUE))
      } else {
        Dat <- NULL
      }
      return(Dat)
    })
  
  # UI: timeseries
  output$ui_ts <- renderUI({
    sidebarLayout(
      sidebarPanel(helpText(paste0("Here you can plot aggregated time series.")),
                   conditionalPanel(condition = "! input.goPeriod",
                                    helpText("Please select the period of interest in the ",strong("data")," tab.")),
                   conditionalPanel(condition = "input.goPeriod",
                                    helpText("You can change the period of interest in the ",strong("data")," tab."),
                                    helpText("Choose the time step, both for time series plot and maps."),
                                    selectInput("timestep","time step",c("Day","Weekday","Year_Week","Year_Month")),
                                    hr(),
                                    helpText("The following options affect only the time series plot, not the maps."),
                                    selectInput("colorby","color by",c("data source"="Source","same color"="same")),
                                    selectInput("splitby","split by",c("data source"="Source","don't split"="none")),
                                    selectInput("geom_type","plot type",c("boxplot","violin","jitter","blank")),
                                    hr(),
                                    helpText("You can emphasize one or more peaks, by plotting data measured by some stations."),
                                    selectInput("emphasis","emphasis",c("period max","daily max","data source max","none")),
                                    sliderInput("howmany","how many peaks do you want to emphasize?",1,5,1,1,ticks = FALSE),
                                    checkboxInput("emph_lines","lines for stations' concentration?",TRUE),
                                    checkboxInput("emph_labels","peaks labelled with values",FALSE),
                                    hr(),
                                    checkboxInput("yax_ctrl","change y axis",FALSE),
                                    conditionalPanel(condition = "input.yax_ctrl",
                                                     selectInput("yax_percmin","lower percentile",choices = c(0,0.1,0.5,1)),
                                                     selectInput("yax_percmax","upper percentile",choices = c(100,99.9,99.5,99))
                                    )
                   ),
                   #actionButton("goTs", label="plot", icon = icon("arrow-circle-right")),
                   width=3),
      mainPanel(tabsetPanel(
        tabPanel("plot",  tags$head(tags$style(HTML(progressBarStyle))),
                 plotOutput("ts"))
        ,tabPanel("maps",   tags$head(tags$style(HTML(progressBarStyle))),
                  plotOutput("ts_maps"))
      )
      )
    )
  })
  
  # UI: exceedances
  output$ui_exc <- renderUI({
    sidebarLayout(
      sidebarPanel(helpText("Here you can analize exceedances of a given threshold."),
                   conditionalPanel(condition = "! input.goPeriod",
                                    helpText("Please select the period of interest in the ",strong("data")," tab.")),
                   conditionalPanel(condition = "input.goPeriod",
                                    helpText("You can change the period of interest in the ",strong("data")," tab."),
                                    sliderInput("threshold","threshold",10,100,50,5)
                   ),
                   width=3),
      mainPanel(tabsetPanel(
        tabPanel("plot",  tags$head(tags$style(HTML(progressBarStyle))),
                 plotOutput("exc_plot"))
        ,tabPanel("map",   tags$head(tags$style(HTML(progressBarStyle))),
                  plotOutput("exc_map"))
        ,tabPanel("table", dataTableOutput("exc_table"))
      ))
    )
  })
  
  # UI: clustering
  output$ui_clu <- renderUI({
    sidebarLayout(
      sidebarPanel(helpText("Here you perform cluster analysis of the time series."),
                   conditionalPanel(condition = "! input.goPeriod",
                                    helpText("Please select the period of interest in the ",strong("data")," tab.")),
                   conditionalPanel(condition = "input.goPeriod",
                                    helpText("You can change the period of interest in the ",strong("data")," tab."),
                                    sliderInput("nclu","number of clusters",2,10,5),
                                    sliderInput("clu_req","data needed (%)",50,100,80,5),
                                    checkboxInput("clu_stand","standardize data before clustering?",FALSE),
                                    selectInput("clu_metr",label = "metric",choices = c("manhattan","euclidean")),
                                    hr(),
                                    helpText("Change here some details of the time series plot. Note that 'medoid' of a cluster is its most representative station."),
                                    selectInput("clu_add",label = "line",choices = c("medoid","mean","median","none")),
                                    checkboxInput("clu_box","boxplot",TRUE),
                                    checkboxInput("clu_log","logarithmic scale in y",TRUE)
                   ),
                   width=3),
      mainPanel(tabsetPanel(
        tabPanel("map",   tags$head(tags$style(HTML(progressBarStyle))),
                 plotOutput("clu_map"))
        ,tabPanel("plot",  tags$head(tags$style(HTML(progressBarStyle))),
                  plotOutput("clu_plot"))
        ,tabPanel("table", dataTableOutput("clu_table"))
      ))
    )
  })
  
  # data table
  output$df <- renderDataTable(options = list(pageLength = 10),
                               expr={
                                 withProgress(message = 'Loading...', value = 1, {
                                   dataOfPeriod()
                                 })
                               })
  
  # peaks detection
  dataWithPeaks <- reactive({
    dataOfPeriod() %>% dplyr::mutate(Station=paste0(Name," (",Source,")"),
                                     Weekday=lubridate::wday(Day,label = T),
                                     Year_Week=format(as.Date(Day),"%Y_%U"),
                                     Year_Month=format(as.Date(Day),"%Y_%m")) -> Dat
    if(input$emphasis=="none") {
      Dat$Emphasis <- Dat$Peak <- NA
    } else {
      if(input$emphasis=="period max") Dat %>% group_by(Pollutant) -> Dat  
      if(input$emphasis=="daily max") Dat %>% group_by(Pollutant,Day) -> Dat  
      if(input$emphasis=="data source max") Dat %>% group_by(Pollutant,Source) -> Dat  
      Dat %>% dplyr::mutate(N=n(), 
                            Rank=1+N-rank(Value,na.last=FALSE,ties.method="max"), 
                            Peak=(Rank<=input$howmany)) %>% 
        left_join(.,Dat) -> Dat
      Dat %>% group_by(Pollutant,Name,Source) %>% dplyr::mutate(Emphasis=max(Peak,na.rm=T)) %>% 
        left_join(.,Dat) -> Dat
    }
    Dat$Emphasis[!Dat$Emphasis] <- NA
    Dat$Peak[!Dat$Peak] <- NA
    Dat
  })
  
  # plot time series
  output$ts <- renderPlot({
    Aes <- aes_string(x=input$timestep, y="Value", group=input$timestep)
    if(input$colorby!="same") Aes <- modifyList(Aes, aes_string(col=input$colorby))
    pl <- ggplot(data = dataWithPeaks(), Aes) + theme_bw()
    geom_ <- get(paste0("geom_",input$geom_type))
    pl <- pl + geom_(width=0.4)
    if(input$splitby!="none") pl <- pl + facet_wrap(input$splitby) 
    if(input$emphasis!="none" & input$emph_lines) pl <- pl +
      geom_line(aes(x=as.numeric(ordered(get(input$timestep)))*Emphasis, 
                    y=Value*Emphasis, group=Station))
    if(input$emphasis!="none") pl <- pl +
      geom_point(aes(x=as.numeric(ordered(get(input$timestep)))[Peak], y=Value[Peak]), 
                 shape=21, fill="white", size=5) +
      geom_point(aes(x=as.numeric(ordered(get(input$timestep)))[Peak], y=Value[Peak], 
                     shape=Station[Peak], group=Station[Peak]), size=2.5) +
      scale_shape_manual(values=c(65:90,97:122))
    if(input$emph_labels) pl <- pl +
      geom_label_repel(aes(x=as.numeric(ordered(get(input$timestep)))[Peak], y=Value[Peak], label=Value))
    if(input$yax_ctrl) pl <- pl +
      scale_y_continuous(limits=c(quantile(dataWithPeaks()$Value,as.numeric(input$yax_percmin)*0.01,na.rm=T),
                                  quantile(dataWithPeaks()$Value,as.numeric(input$yax_percmax)*0.01,na.rm=T)))
    sou <- unique(dataOfPeriod()$Source)
    pl <- pl + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(title=paste0(input$pollutantPeriod,": ",dailyInd()),
           subtitle=paste0("period: ",paste(unique(input$daterange),collapse=" to ")),
           caption=paste(collapse="\n",strwrap(paste0("data source","s"[length(sou)>1],": ",
                          paste(sou,collapse=", ")),exdent=5))) +
      ylab(expression("Concentration"~(mu*g/m^3)))
    withProgress(message = 'Making plot...', value = 1, {
      pl
    })
  },height = function() ifelse(input$splitby=="none",600,900)
  )
  
  # time series: maps
  output$ts_maps <- renderPlot({
    dataOfPeriod() %>% 
      dplyr::mutate(Weekday=lubridate::wday(Day,label = T),
                    Year_Week=format(as.Date(Day),"%Y_%U"),
                    Year_Month=format(as.Date(Day),"%Y_%m"))%>% 
      dplyr::filter(!is.na(Value)) %>%
      dplyr::group_by_(.dots=list(input$timestep,"Name")) %>% 
      dplyr::summarize(Value=mean(Value,na.rm=T), 
                       Valid=n(),Lat=first(Lat),Lon=first(Lon)) -> Dat
    xmin<-min(Dat$Lon,na.rm=T)
    xmax<-max(Dat$Lon,na.rm=T)
    ymin<-min(Dat$Lat,na.rm=T)
    ymax<-max(Dat$Lat,na.rm=T)
    dx<-xmax-xmin
    dy<-ymax-ymin
    bb <- unique(round(quantile(Dat$Value,c(0,20,40,60,80,90,95,99,100)/100,na.rm=T)))
    Dat %>% mutate(Value=cut(Value,bb,include.lowest = T))-> Dat
    sou <- unique(dataOfPeriod()$Source)
    pl <- ggplot() + 
      geom_polygon(data=MapData, aes(x=long, y=lat, group = group),
                   colour="grey80", fill="grey30" ) +
      geom_point(data=Dat, aes(x=Lon, y=Lat, color=Value, size=Valid), shape=19) +
      scale_colour_brewer(palette = "Spectral", direction=-1) +
      scale_size_area(max_size=2) +
      coord_map(xlim = c(xmin-dx*0.05,xmax+dx*0.05),
                ylim = c(ymin-dy*0.05,ymax+dy*0.05)) +
      labs(title=bquote(.(input$pollutantPeriod)*": "*.(dailyInd())*" ("*mu*g/m^3*")"),
           subtitle=paste0("period: ",paste(unique(input$daterange),collapse=" to ")),
           caption=paste(collapse="\n",strwrap(paste0("data source","s"[length(sou)>1],": ",
                          paste(sou,collapse=", ")),exdent=5))) +
      facet_wrap(input$timestep, labeller = label_both)
    withProgress(message = 'Making plot...', value = 1, {
      pl
    })
  },height = function() {
    nd <- as.numeric(as.POSIXct(input$daterange[2])-as.POSIXct(input$daterange[1])+1)
    hrow <- 300
    if(input$timestep=="Day") hh <- 200+(nd%/%8+(nd%%8>0))*hrow
    if(input$timestep=="Weekday") hh <- 200+3*hrow
    if(input$timestep=="Year_Week") hh <- max(500,200+(nd/40)*hrow)
    if(input$timestep=="Year_Month") hh <- max(500,200+(nd/150)*hrow)
    hh
  }
  )
  
  # exceedances: barplot
  output$exc_plot <- renderPlot({
    dataOfPeriod() %>% dplyr::mutate(Exc=Value>input$threshold,
                                     notExc=Value<=input$threshold) %>%
      group_by(Day) %>%
      dplyr::summarize(notExceeding=sum(notExc, na.rm=T),
                       Exceeding=sum(Exc, na.rm=T)) %>%
      gather(key=Status,value=Stations,-Day) %>%
      dplyr::mutate(Day=as.Date(as.character(Day)))-> Dat
    sou <- unique(dataOfPeriod()$Source)
    pl <- ggplot(data=Dat, aes(x=Day,y=Stations,fill=Status)) + geom_col() + 
      theme_bw() + scale_fill_manual(values=c("olivedrab","orangered")) +
      labs(title=bquote(.(input$pollutantPeriod)*": number of stations with "*.(dailyInd())*" above "*.(input$threshold)~mu*g/m^3),
           subtitle=paste0("period: ",paste(unique(input$daterange),collapse=" to ")),
           caption=paste(collapse="\n",strwrap(paste0("data source","s"[length(sou)>1],": ",
                          paste(sou,collapse=", ")),exdent=5)))
    withProgress(message = 'Making plot...', value = 1, {
      pl
    })
  })
  
  # exceedances: map
  output$exc_map <- renderPlot({
    dataOfPeriod() %>% filter(!is.na(Value)) %>%
      dplyr::mutate(Exc=Value>input$threshold) %>% 
      group_by(Name,Lat,Lon) %>%
      dplyr::summarize(Exceed=sum(Exc, na.rm=T),
                       Valid=n()) -> Dat
    xmin<-min(Dat$Lon,na.rm=T)
    xmax<-max(Dat$Lon,na.rm=T)
    ymin<-min(Dat$Lat,na.rm=T)
    ymax<-max(Dat$Lat,na.rm=T)
    dx<-xmax-xmin
    dy<-ymax-ymin
    bb <- unique(round(quantile(Dat$Exceed,c(0,20,40,60,80,90,100)/100)))
    Dat %>% mutate(Exceedances=cut(Exceed,bb,include.lowest = T)) %>% dplyr::arrange(desc(Exceed))-> Dat
    sou <- unique(dataOfPeriod()$Source)
    pl <- ggplot() + 
      geom_polygon(data=MapData, aes(x=long, y=lat, group = group),
                   colour="grey80", fill="grey30" ) +
      geom_point(data=Dat, aes(x=Lon, y=Lat, color=Exceedances, size=Valid), shape=19) +
      scale_colour_brewer(palette = "Spectral", direction=-1) +
      scale_size_area() +
      coord_map(xlim = c(xmin-dx*0.05,xmax+dx*0.05),
                ylim = c(ymin-dy*0.05,ymax+dy*0.05)) +
      labs(title=bquote(.(input$pollutantPeriod)*": exceedances of "*.(dailyInd())*" (threshold: "*.(input$threshold)~mu*g/m^3*")"),
           subtitle=paste0("period: ",paste(unique(input$daterange),collapse=" to ")),
           caption=paste(collapse="\n",strwrap(paste0("data source","s"[length(sou)>1],": ",
                          paste(sou,collapse=", ")),exdent=5)))
    withProgress(message = 'Making plot...', value = 1, {
      pl
    })
  },height = 800
  )
  
  # exceedances: table
  output$exc_table <- renderDataTable({
    dataOfPeriod() %>% dplyr::mutate(Exc=Value>input$threshold) %>% group_by(Name,Lat,Lon) %>%
      dplyr::summarize(Exceed=sum(Exc, na.rm=T),
                       Valid=sum(!is.na(Exc))) -> Dat
    bb <- unique(round(quantile(Dat$Exceed,c(0,20,40,60,80,90,100)/100)))
    Dat %>% mutate(Exceedances=cut(Exceed,bb,include.lowest = T)) %>% dplyr::arrange(desc(Exceed))-> Dat
    Dat
  })
  
  # clustering
  dataWithClusters <- reactive({
    ddd <- dataOfPeriod() %>% mutate(Station=paste0(Name," (",Source,")"))
    nd <- length(unique(ddd$Day))
    ss <- unique(ddd$Station)
    ii <- paste0("st",1:length(ss))
    i2s <- function(i) ss[match(i,ii)]
    s2i <- function(s) ii[match(s,ss)]
    ddd %>% mutate(Code=s2i(Station)) -> ddd
    ddd %>% 
      dplyr::select(Day,Code,Value) %>%
      dplyr::arrange(Day) %>% distinct() %>% spread(Code,Value) ->  dat
    dat <- dat[,colSums(!is.na(dat))>nd*input$clu_req/100]
    clu <- clara(t(dat[,-1]),k=input$nclu,stand=input$clu_stand,
                 samples = 50,metric=input$clu_metr)
    Clu <- data.frame(Code=names(clu$clustering), 
                      Cluster=sprintf(clu$clustering,fmt="%02i"), 
                      isMedoid=names(clu$clustering)%in%rownames(clu$medoids))
    Dat <- merge(ddd, Clu, all.x=FALSE)
    Dat
  })
  
  # clustering: table
  output$clu_table <- renderDataTable({
    dataWithClusters() %>% dplyr::select(Name,Source,Cluster,isMedoid) %>% distinct() -> Dat
    Dat
  })
  
  # clustering: plot
  output$clu_plot <- renderPlot({
    Dat <- dataWithClusters() %>% 
      dplyr::mutate(col=c("odd","even")[1 + as.numeric(as.Date(as.character(Day)) - as.Date("1900-01-01")) %% 2])
    ymin <- max(1,min(Dat$Value,na.rm=T))
    ymax <- max(Dat$Value,na.rm=T)*1.1
    ggplot(Dat, aes(x=Day, y=Value, colour=Cluster)) +
      geom_crossbar(aes(x=Day, 
                        y=   ymin, 
                        ymin=ymin,
                        ymax=ymax,
                        fill = col),
                    col="transparent", width=1) +
      theme_linedraw() +
      theme(panel.grid.major.x = element_blank(),
            line = element_line(colour = "gray90")) +
      scale_fill_manual(values=alpha(c("white", "#dddddd"), c(0,.1)), guide =FALSE) -> pl
    if(input$clu_log) {
      pl <- pl + scale_y_log10(limits=c(ymin,ymax), expand = c(0, 0))
    } else {
      pl <- pl + scale_y_continuous(limits=c(ymin,ymax), expand = c(0, 0))
    }
    if(input$clu_box) pl <- pl + geom_boxplot(fill="white")
    if(input$clu_add=="medoid") pl <- pl + 
      scale_linetype_manual(values=c("blank", "solid"), guide=FALSE) +
      geom_line(size=1.5, aes(group=Station, linetype=isMedoid))
    if(input$clu_add %in% c("mean","median","max")) pl <- pl + 
      stat_summary(fun.y = get(input$clu_add), geom="line",
                   size=1.5, aes(group=Cluster), linetype="solid")
    sou <- unique(dataWithClusters()$Source)
    pl <- pl + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(title=bquote("stations clustering based on "*.(input$pollutantPeriod)*" "*.(dailyInd())),
           subtitle=paste0("period: ",paste(unique(input$daterange),collapse=" to "),"\n",
                           "method: clustering around medoids (",input$clu_metr," metric)",
                           " with data standardization"[input$clu_stand],
                           paste0("\nlines: cluster ",input$clu_add)[input$clu_add!="none"]),
           caption=paste(collapse="\n",strwrap(paste0("data source","s"[length(sou)>1],": ",
                          paste(sou,collapse=", ")),exdent=5)))
    withProgress(message = 'Making plot...', value = 1, {
      pl
    })
  })
  
  # clustering: map
  output$clu_map <- renderPlot({
    dataWithClusters() %>% dplyr::select(Name,Source,Lat,Lon,Cluster,isMedoid) %>% distinct() -> Dat
    xmin<-min(Dat$Lon,na.rm=T)
    xmax<-max(Dat$Lon,na.rm=T)
    ymin<-min(Dat$Lat,na.rm=T)
    ymax<-max(Dat$Lat,na.rm=T)
    dx<-xmax-xmin
    dy<-ymax-ymin
    sou <- unique(dataWithClusters()$Source)
    pl <- ggplot() + 
      geom_polygon(data=MapData, aes(x=long, y=lat, group = group),
                   colour="grey80", fill="grey30" ) +
      geom_label_repel(data=Dat, aes(x=Lon, y=Lat, fill=Cluster, label=Cluster), 
                       box.padding = unit(0.08, "lines"),
                       label.padding = unit(0.08, "lines"), 
                       size=2.5, col="black") +
      coord_map(xlim = c(xmin-dx*0.05,xmax+dx*0.05),
                ylim = c(ymin-dy*0.05,ymax+dy*0.05)) +
      labs(title=bquote("stations clustering based on "*.(input$pollutantPeriod)*" "*.(dailyInd())),
           subtitle=paste0("period: ",paste(unique(input$daterange),collapse=" to "),"\n",
                           "method: clustering around medoids (",input$clu_metr," distance)",
                           " with data standardization"[input$clu_stand]),
           caption=paste(collapse="\n",strwrap(paste0("data source","s"[length(sou)>1],": ",
                          paste(sou,collapse=", ")),exdent=5)))
    withProgress(message = 'Making plot...', value = 1, {
      pl
    })
  },height = 800
  )
  
  # breaks for the obs.map
  Breaks <- reactive({
    if(input$scaleDay == "classic") {
      bb <- c(0,25,50,75,100,300)
    }else if(input$scaleDay == "extended") {
      bb <- c(0,25,50,75,100,150,200,250,300,400)
    }
    bb
  })
  
  # units
  unitDay <- reactive({
    if(input$pollutantDay=="PM10") uu <- "ug/m^3"
    uu
  })
  
  # daily map: palette
  colorpal <- reactive({
    if(input$scaleDay == "classic") {
      cc <- c("steelblue","olivedrab","orange","red","purple")
    }else if(input$scaleDay == "extended") {
      cc <- c("white",rev(RColorBrewer::brewer.pal(name="Spectral",n=length(Breaks())-4)),"magenta","black")
    }
    colorFactor(palette = cc, 
                domain = cut(Breaks(),Breaks(),
                             ordered_result=TRUE, include.lowest=TRUE))
  })
  
  # daily obs. map: initialize basemap
  output$Map <- renderLeaflet({
    lon0 <- 11
    lat0 <- 42.5
    leaflet() %>% 
      setView(lon0, lat0, 6) %>% 
      addTiles(group = "classic")  %>% 
      addProviderTiles("CartoDB.Positron", group = "grey minimal") %>%
      addProviderTiles("Stamen.TonerLite", group = "toner lite") %>%
      addProviderTiles("Esri.WorldImagery", group = "satellite") %>%
      addProviderTiles("Thunderforest.Landscape", group = "terrain") %>%

      # Layers control
      addLayersControl(
        baseGroups = c("grey minimal","classic","toner lite","terrain","satellite"),
        #overlayGroups = c("aod"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })

  # daily obs. map: dynamic layer
 # observeEvent(input$goDay,{
  observe({
    Dat <- dataOfDay()
    Pal <- colorpal()
    
    leafletProxy("Map",session) %>% clearMarkers() -> map
    if (!is.null(Dat) && nrow(Dat)>0) {
      ns <- nrow(Dat)
      map %>%
        addCircleMarkers(lng=~Lon, lat=~Lat
                         ,data=Dat
                         ,radius=6 
                         ,fillColor=~Pal(ValueInterval)
                         ,color="black", weight=1
                         ,stroke=T, fillOpacity=0.8
                         ,layerId=1:ns
                         ,popup= ~paste0("station: <b>",Name,"</b><br/>",
                                                    "value: <b>",Value,"</b><br/>",
                                                    "data source: <b>",Source,"</b>")
                         
        ) -> map
    }else{
      map %>% addPopups(lng=11,lat=44,popup="No valid data!") -> map
    }
    map
  })
  
  # daily obs. map: legend
  observe({
    Dat <- dataOfDay()
    Pal <- colorpal()
    proxy <- leafletProxy("Map", data = Dat)
    
    proxy %>% 
      clearControls() %>%
      addLegend(position = "bottomright", pal = Pal, values = ~ValueInterval,
                title = paste0(as.character(input$day),":<br>",
                               input$pollutantDay," ",dayInd(),"<br>(",
                               unitDay(),")")
                )
  })
  
  
  # UI: map of models
  output$ui_modelmap <- renderUI({
    sidebarLayout(
      sidebarPanel(helpText("Here you can select the day to plot in the map."),
                   dateInput("dayMod", "day of interest", value=Sys.Date(), 
                             min=Sys.Date(), max = Sys.Date()+3,
                             width="50%"),
                   selectInput("pollutantDayMod",label = "pollutant",
                               choices = list("PM10 daily average"="PM10_Mean",
                                              "ozone daily maximum"="O3_Max",
                                              "ozone daily maximum of 8h running mean"="O3_MaxAvg8h",
                                              "PM2.5 daily average"="PM25_Mean")),
                   selectInput("scaleDayMod",label = "color scale", choices = c("classic","extended"), selected = "extended"),
                   selectInput("mapMod",label = "model",
                               choices = c("CHIMERE","EMEP","EURAD","LOTOSEUROS","MATCH","MOCAGE","SILAM")),
                   width=3),
      mainPanel(leafletOutput("MapMod", width="100%", height="800px"))
    )
  })

      # load model data of single day
  changeOfDayMod <- reactive({
    input$dayMod;input$mapMod;input$pollutantDayMod
  })
  modelOfDay <- eventReactive(
    eventExpr = changeOfDayMod(), #ignoreNULL = FALSE,
    valueExpr = {
      refDay <- as.Date(format(Sys.time()-8*3600, "%Y-%m-%d"))
      valDay <- try(as.Date(input$dayMod))
      if(class(valDay)[1]=="try-error") valDay <- Sys.Date()
      model <- ifelse(is.null(input) || is.null(input$mapMod), "CHIMERE", input$mapMod)
      poll <- ifelse(is.null(input) || is.null(input$pollutantDayMod), "PM10_Mean", input$pollutantDayMod)
      rm("r")
      File <- paste0("/home/giovanni/R/projects/calicantus/data/mod-data/grid/",
                     format(refDay,"%Y/%m/%d/CAMS50_ref%Y%m%d"),
                     format(valDay,"_val%Y%m%d_"),
                     model,"_",poll,".rda")
      if(file.exists(File)) {
        load(File)
        Dat <- r
      } else {
        Dat <- NULL
      }
      return(Dat)
    })
  
  
  # breaks for the model map
  BreaksMod <- reactive({
    scale <- ifelse(is.null(input) || is.null(input$scaleDayMod), "classic", input$scaleDayMod)
    poll <- ifelse(is.null(input) || is.null(input$pollutantDayMod), "PM10_Mean", input$pollutantDayMod)
    if(scale == "classic") {
      if(poll=="PM10_Mean")   bb <- c(25,50,75,100,150)
      if(poll=="PM25_Mean")   bb <- c(10,25,50,75,100)
      if(poll=="O3_Max")      bb <- c(90,120,180,240,300)
      if(poll=="O3_MaxAvg8h") bb <- c(60,90,120,150,180)
    }else if(scale == "extended") {
      if(poll=="PM10_Mean")   bb <- c(25,50,75,100,150,200,250,300,400)
      if(poll=="PM25_Mean")   bb <- c(10,25,50,75,100,150,200,250,300)
      if(poll=="O3_Max")      bb <- c(90,120,180,210,240,270,300,350,400)
      if(poll=="O3_MaxAvg8h") bb <- c(60,90,120,150,180,210,240,270,300)
    }
    bb
  })
  
  # model units
  unitDayMod <- reactive({
    poll <- ifelse(is.null(input) || is.null(input$pollutantDayMod), "PM10_Mean", input$pollutantDayMod)
    poll <- strsplit(poll,split = "_")[[1]][1]
    if(poll %in% c("PM10","O3","PM25")) uu <- "ug/m^3"
    uu
  })
  
  # model units
  modelAttribution <- reactive({
    model <- ifelse(is.null(input) || is.null(input$mapMod), "CHIMERE", input$mapMod)
    if(model %in% c("CHIMERE","EMEP","EURAD","LOTOSEUROS","MATCH","MOCAGE","SILAM")) {
      att <- paste0("Generated using Copernicus Atmosphere Monitoring Service Information ",
                    format(Sys.Date(),"%Y"))
    }
    att
  })
  
  # daily model map: palette
  colorpalMod <- reactive({
    scale <- ifelse(is.null(input) || is.null(input$scaleDayMod), "classic", input$scaleDayMod)
    if(scale == "classic") {
      cc <- c("olivedrab","orange","red","purple")
    }else if(scale == "extended") {
      cc <- c(rev(RColorBrewer::brewer.pal(name="Spectral",
                                           n=length(BreaksMod())-3)),
              "magenta","black")
    }
    bb <- BreaksMod()
    Dat <- modelOfDay()
    if(!is.null(Dat)) {
      rmax <- round(cellStats(modelOfDay(),"max"))
      if(max(bb)<rmax) bb[length(bb)] <- rmax
    }
    colorBin(palette = cc, 
             domain = range(bb),
             bins = bb,
             na.color = "transparent")
  })
  
  
  # daily models map: initialize basemap
  output$MapMod <- renderLeaflet({
    lon0 <- 11
    lat0 <- 42.5
    leaflet() %>% 
      setView(lon0, lat0, 5) %>% 
      addTiles(group = "classic")  %>% 
      addProviderTiles("CartoDB.Positron", group = "grey minimal") 
  })
  
  # daily models map: dynamic layer
  observe({
    Dat <- modelOfDay()
    Pal <- colorpalMod()
    
    leafletProxy("MapMod",session) %>% clearShapes() %>% clearImages() -> map
    if (!is.null(Dat)) {
      map %>% 
        addRasterImage(Dat, colors = Pal, opacity = 0.5, layerId = "model", 
                       attribution = modelAttribution()) %>%
        addRectangles(lng1 = Dat@extent[1],
                      lng2 = Dat@extent[2],
                      lat1 = Dat@extent[3],
                      lat2 = Dat@extent[4],
                      fill = F, stroke = T, weight = 5, color = "black", 
                      dashArray = "10, 10")-> map
    }else{
      map %>% addPopups(lng=11,lat=42.5,
                        popup="Data not available for this model.<br>Please try another one.") -> map
    }
    map
  })
  
  # daily models map: legend
  observe({
    Dat <- modelOfDay()
    Pal <- colorpalMod()
    proxyMod <- leafletProxy("MapMod", data = Dat)
    
    model <- ifelse(is.null(input) || is.null(input$mapMod), "CHIMERE", input$mapMod)
    poll <- ifelse(is.null(input) || is.null(input$pollutantDayMod), "PM10_Mean", input$pollutantDayMod)
    poll <- switch(poll,
                   "PM10_Mean"="PM10 daily average",
                   "O3_Max"="O3 daily maximum",
                   "O3_MaxAvg8h"="O3 max of 8h running mean",
                   "PM25_Mean"="PM2.5 daily average")
    
    if(!is.null(Dat)) {
      proxyMod %>% 
        clearControls() %>%
        addLegend(position = "bottomright", pal = Pal, values = values(Dat),
                  title = paste0("model ",model,"<br>",
                                 as.character(input$dayMod),":<br>",
                                 poll,"<br>(",
                                 unitDayMod(),")")
        )
    }
  })
  
  
  
  # support for debug
  output$sessionInfo <- renderPrint({sessionInfo()})
  output$uiInput <- renderPrint({input})
  output$summary <- renderPrint({summary(dataOfDay())})
  
  # contacts
  output$contacts <- renderTable({
    read.table("/home/giovanni/R/projects/calicantus/data/data-sources/contacts.csv",
               sep=",",row.names = NULL,header = T, check.names = F)
  })
  
  # account info
  output$ui_account <- renderUI({
    fluidPage(
      p("User: ",code(input$Usr)),
      p("Available data sources: ",code(paste(availableSources(),collapse=", ")))
    )
  })
}

shinyApp(ui = ui, server = server)