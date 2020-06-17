  
# preliminar --------------------------------------------------------------

options(shiny.sanitize.errors = FALSE)

# detach packages
basic.packages <- c("stats","graphics","grDevices", "shiny",
                    "utils","datasets","methods","base",
                    "httpuv")
basic.packages <- paste0("package:", basic.packages)
package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
package.list <- setdiff(package.list,basic.packages)
if (length(package.list)>0)  for (package in package.list) {
  try(detach(package, character.only=TRUE, force=T))
}

# load packages
local_libloc <- "/home/giovanni/R/x86_64-pc-linux-gnu-library/3.5"
.libPaths(unique(local_libloc,.libPaths()))

req.pckgs <- c("Rcpp","digest","R6","cluster","shinyBS","base64enc",
               "plyr","dplyr","lubridate","sp","rgdal",
               "geosphere","shinyjs","raster","data.table","tidyr",
               "scales","ggplot2","ggrepel","stringi","RColorBrewer","maps",
               "cluster","bitops","RCurl","markdown","yaml","tidyselect",
               "promises", "later", "httpuv", "htmltools","htmlwidgets",
               "leaflet","DT")

require_last <- function(package) {
  ip <- as.data.frame(installed.packages(), stringsAsFactors = F)
  ip <- ip[ip$Package==package,]
  ip <- ip[order(ip$Built, decreasing = T), ]
  lib_loc <- ip$LibPath[1]
  ok <- try(require(package, 
                    lib.loc = lib_loc,
                    character.only = T))
  if(class(ok)[1]=="try-error") ok <- -1
  cat(paste0("package ",package,c(" loaded"," not available"," cannot be unloaded")[2-ok]),sep="\n")
}
lapply(req.pckgs, require_last) -> devnull

# scripts
source("/home/giovanni/R/projects/calicantus/config/ui_credentials.R")
source("/home/giovanni/R/projects/calicantus/R/policy.R")
source("/home/giovanni/R/projects/calicantus/shiny/forms/forms.R")

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
if(!is.null(proxy_usr)) {
  Sys.setenv(http_proxy=paste0("http://",proxy_usr,":",proxy_pwd,"@",proxy_addr,":",proxy_port,"/"))
  Sys.setenv(https_proxy=paste0("http://",proxy_usr,":",proxy_pwd,"@",proxy_addr,":",proxy_port,"/"))
}

# geographic data
# MapData <- map_data(map = "world", 
#                     region = c("Italy","Slovenia","Croatia","Serbia","San Marino","Vatican",
#                                "France","Switzerland","Austria","Germany","Bosnia and Herzegovina",
#                                "Tunisia","Malta","Montenegro","Hungary","Albania","Greece","Algeria",
#                                "Czech Republic", "Slovakia", "Liechtenstein","Poland","Kosovo",
#                                "Bulgaria","Romania","Turkey","Macedonia","Ukraine",
#                                "Luxembourg", "Belgium", "Netherlands", "UK", "Ireland",
#                                "Spain", "Portugal", "Andorra", 
#                                "Russia", "Belarus",
#                                "Costa Rica"))
MapData <- map_data(map = "world")



# ui ----------------------------------------------------------------------


ui_home     <- uiOutput("ui_home")
ui_map      <- uiOutput("ui_map")
ui_data     <- uiOutput("ui_data")
ui_ts       <- uiOutput("ui_ts")
ui_tsmod    <- uiOutput("ui_tsmod")
ui_exc      <- uiOutput("ui_exc")
ui_clu      <- uiOutput("ui_clu")
ui_modelmap <- uiOutput("ui_modelmap")
ui_account  <- uiOutput("ui_account")
ui_obsAvail <- uiOutput("ui_obsAvail")

# UI: policy
ui_policy <- fluidPage(
  includeMarkdown("/home/giovanni/R/projects/calicantus/shiny/intro/policy.md"),
  tableOutput("use_policy")
)

# UI: about
ui_about <- fluidPage(
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
  verbatimTextOutput("uiInput"),
  verbatimTextOutput("uiInput")
)
  

# UI: login
Logged = FALSE
ui_login <- function(){
  tagList(
    div(id = "login",
        wellPanel(textInput("Usr", "user:", ""),
                  passwordInput("Pwd", "password:", ""),
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
    tabPanel("home", ui_home),
    navbarMenu("observations"
               ,tabPanel("map", ui_map)
               ,tabPanel("data", ui_data)
               ,tabPanel("availability",ui_obsAvail)
               ,tabPanel("timeseries",ui_ts)
               ,tabPanel("exceedances",ui_exc)
               ,tabPanel("clustering",ui_clu)
    ),
    navbarMenu("forecasts"
               ,tabPanel("map", ui_modelmap)
               ,tabPanel("timeseries",ui_tsmod)
    ),
    navbarMenu("more"
               ,tabPanel("data policy",ui_policy)
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
  observeEvent(input$Login,{ 
    if (!USER$Logged && !is.null(input$Login) && input$Login>0) {
          usr <- isolate(input$Usr)
          pwd <- isolate(input$Pwd)
          Id.usr <- which(ui_usr == usr)
          Id.pwd <- which(ui_pwd == pwd)
          logfile <- paste0("/home/giovanni/R/projects/calicantus/log/web-interface.log")
          if(!file.exists(logfile)) file.create(logfile)
          if (length(Id.usr)==1 & length(Id.pwd)==1 && Id.usr==Id.pwd) {
            rec <- paste0("login success: ",Sys.time()," | usr: ",input$Usr)
            USER$Logged <- TRUE
          } else {
            rec <- paste0("login failure: ",Sys.time()," | usr: ",input$Usr)
            USER$Logged <- FALSE
          }
          write(rec,file=logfile,append=TRUE)
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
    }
  })
  
  # check account expire date
  expAccount <- reactive({
    source("/home/giovanni/R/projects/calicantus/config/config_users.R",local=TRUE)
    expiredAccount(input$Usr)
  })
  expDate <- reactive({
    source("/home/giovanni/R/projects/calicantus/config/config_users.R",local=TRUE)
    expireDate(input$Usr)
  })
  
  # available data sources (personalized depending on user)
  availableSources <- reactive({
    source("/home/giovanni/R/projects/calicantus/config/config_users.R",local=TRUE)
    auth_sources <- authorizedSources(input$Usr)
    aS <- system("cd /home/giovanni/R/projects/calicantus/data/sites-info/; ls metadata*csv | sed s/metadata.//g | sed s/.csv//g",intern=TRUE)
    aS <- intersect(aS, auth_sources)
    aS
  })
  
  # daily indicator
  dailyInd <- reactive({
    if(input$pollutantPeriod %in% c("PM10","PM2.5")) dI <- "daily average"
    if(input$pollutantPeriod %in% c("O3","NO2")) dI <- "daily maximum"
    dI
  })
  dayInd <- reactive({
    if(input$pollutantDay %in% c("PM10","PM2.5")) dI <- "daily average"
    if(input$pollutantDay %in% c("O3","NO2")) dI <- "daily maximum"
    dI
  })
  
  # UI: home------------------------------------------------------------------
  output$ui_home <- renderUI({
    fluidPage(
      column(4,
             wellPanel( 
               helpText("Welcome back to ",em("calicantus"),
                        ". Please contact the ",
                        a("platform manager",href="mailto:giovanni.bonafe@arpa.fvg.it"),
                        " if you find a bug. Thanks for your cooperation!")
             ),
             wellPanel(
               helpText("If you want to participate to ",em("calicantus"),
                        "as data provider, please click ",em("Participate"),
                        a("on this page",
                          href="http://sdati.arpae.it/calicantus-intro",
                          target="_blank"),
                        ".")
             ),
             actionButton(inputId = "logout",label = "Log out",icon = icon("sign-out"), width="120",
                          onclick ="location.href='https://sdati.arpae.it/calicantus-intro';"),
             bsTooltip("logout","Click here to log out")
      ),
      column(4,
             wellPanel(
               helpText("Remember that your account will expire after"),
               textOutput("expDate")
             ),
             bsAlert("homeAlert")
      )
    )
  })
  
  output$expDate <- renderText({
    if(expAccount()) {
      createAlert(session, "homeAlert", "dateAlert",
                  title = "Your account has expired", style="danger",
                  content = "Please contact the platform manager to renew your registration.", 
                  append = FALSE)
      return(expDate())
    } else {
      return(expDate())
    }
  })
    

  # UI: data table------------------------------------------------------------------
  output$ui_data <- renderUI({
    sidebarLayout(
      sidebarPanel(helpText("Here you select the period you are interested in."),
                   dateRangeInput("daterange","period of interest",
                                  start=Sys.Date()-10,
                                  end=Sys.Date()-1,
                                  max = Sys.Date()-1),
                   selectInput("pollutantPeriod",label = "pollutant", 
                               choices = list(`PM10 daily mean`="PM10",
                                              `PM2.5 daily mean`="PM2.5",
                                              `NO2 daily max`="NO2",
                                              `O3 daily max`="O3")),
                   selectInput("sources",label = "sources of data", choices = availableSources(), 
                               selected = availableSources(), multiple=TRUE, selectize=FALSE, size = 15),
                   actionButton("goPeriod", label="load", icon = icon("arrow-circle-right")),
                   conditionalPanel(condition = "input.goPeriod",
                                    helpText("Please note that the table on the right side is interactive:",
                                             "you can sort it by clicking on the header,",
                                             "look for a specific value using the 'search' tool,",
                                             "filter the data by writing in the cells below the table, and so on."),
                                    downloadButton('downloadData', 'download')
                                    ,permissionButton(id="buttonPermis_obsdf", sources=availableSources()),
                                    bsModal("modalPermis_obsdf",title = "Permissions", 
                                            trigger = "buttonPermis_obsdf", 
                                            HTML(policyByPurpose(availableSources())))
                                    ),
                   bsTooltip("downloadData","Data downloaded from the platform cannot be distributed"),
                   hr(),
                   helpText("Data are not verified and may differ from the validated data."),
                   width=3),
      mainPanel(tags$head(tags$style(HTML(progressBarStyle))),
                DT::dataTableOutput("df"))
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
                                                             "_*.rda 2>/dev/null"),
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
      write.table(data, con, fileEncoding = "latin1", quote=T, row.names=F, sep=";")
    }
  )
  
  # UI: map of observed data------------------------------------------------------------------
  output$ui_map <- renderUI({
    sidebarLayout(
      sidebarPanel(helpText("Here you can select the day to plot in the map."),
                   dateInput("day", "day of interest", value=Sys.Date()-1, 
                             min="2014-10-10", max = Sys.Date()-1,
                             width="50%"),
                   selectInput("pollutantDay",label = "pollutant", 
                               choices = list(`PM10 daily mean`="PM10",
                                              `PM2.5 daily mean`="PM2.5",
                                              `NO2 daily max`="NO2",
                                              `O3 daily max`="O3")),
                   selectInput("scaleDay",label = "color scale", choices = c("classic","high values")),
                   actionButton("goDay", label="plot", icon = icon("arrow-circle-right")),
                   conditionalPanel(condition = "input.goDay",
                                    helpText("Please note that the map on the right side is interactive:",
                                             "you can zoom in and out with the mouse wheel or with the '+' and '-',",
                                             "and click on a marker to get name of the station and observed concentration.")),
                   hr(),
                   helpText("Data are not verified and may differ from the validated data.",
                            "Check here below if you need to ask for any permission."),
                   permissionButton(id="buttonPermis_obsmap", sources=availableSources()),
                   bsModal("modalPermis_obsmap",title = "Permissions", trigger = "buttonPermis_obsmap", 
                           HTML(policyByPurpose(availableSources()))),
                   bsButton("buttonCite_obsmap", label = "citations", style="primary"),
                   bsModal("modalCite_obsmap",title = "Citations", trigger = "buttonCite_obsmap", 
                           HTML(citeHtml(pkgs = c("leaflet","base","shiny")))),
                   width=3),
      mainPanel(leafletOutput("Map", width="100%", height="800px"))
    )
  })
  
  # load observed data of single day
  #   dataOfDay <- eventReactive(eventExpr = input$goDay, 
  changeOfDay <- reactive({
    input$goDay
    input$day
    input$pollutantDay
    })
  dataOfDay <- eventReactive(
    eventExpr = changeOfDay(), #ignoreNULL = FALSE,
    valueExpr = {
      if(is.null(input) || is.null(input$day)) {
        d <- as.character(Sys.Date()-1)
      } else {
        d <- as.character(input$day)
      }
      ff <- dir(path=paste0("/home/giovanni/R/projects/calicantus/data/obs-data/",
                            format(as.Date(d),"%Y/%m/%d/")),
                pattern = paste0(format(as.Date(d),"%Y%m%d"),
                                 "_",input$pollutantDay), full.names = T)
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
  
  # UI: timeseries------------------------------------------------------------------
  output$ui_ts <- renderUI({
    sidebarLayout(
      sidebarPanel(
        helpText(paste0("Here you can plot aggregated time series.")),
        conditionalPanel(
          condition = "! input.goPeriod",
          helpText("Please select the period of interest in ",strong("observations > data"),".")),
        conditionalPanel(
          condition = "input.goPeriod",
          helpText("You can change the period of interest in ",strong("observations > data"),"."),
          helpText("Choose the time step, both for time series plot and maps."),
          selectInput("timestep","time step",c("Day","Weekday","Year_Week","Year_Month")),
          hr(),
          bsCollapse(
            bsCollapsePanel(
              title = "layout", style = "default",
              helpText("The following options affect only the time series plot, not the maps."),
              selectInput("colorby","color by",c("data source"="Source","same color"="same")),
              selectInput("splitby","split by",c("data source"="Source","don't split"="none")),
              selectInput("geom_type","plot type",c("boxplot","violin","jitter","blank")),
              hr(),
              checkboxInput("yax_ctrl","change y axis",FALSE),
              conditionalPanel(condition = "input.yax_ctrl",
                               selectInput("yax_percmin","lower percentile",choices = c(0,0.1,0.5,1)),
                               selectInput("yax_percmax","upper percentile",choices = c(100,99.9,99.5,99))
              )
            ),
            bsCollapsePanel(
              title = "emphasis", style = "default",
              helpText("You can emphasize one or more peaks, by plotting data measured by some stations."),
              selectInput("emphasis","emphasis",c("period max","daily max","data source max","none")),
              sliderInput("howmany","how many peaks do you want to emphasize?",1,5,1,1,ticks = FALSE),
              checkboxInput("emph_lines","lines for stations' concentration?",TRUE),
              checkboxInput("emph_labels","peaks labelled with values",FALSE)
            )
          )
          ,permissionButton(id="buttonPermis_obsts", sources=input$sources),
          bsModal("modalPermis_obsts",title = "Permissions", trigger = "buttonPermis_obsts", 
                  HTML(policyByPurpose(input$sources))),
          bsButton("buttonCite_obsts", label = "citations", style="primary"),
          bsModal("modalCite_obsts",title = "Citations", trigger = "buttonCite_obsts", 
                  HTML(citeHtml(pkgs = c("ggplot2","base","shiny"))))
        ),
        #actionButton("goTs", label="plot", icon = icon("arrow-circle-right")),
        width=3),
      mainPanel(tabsetPanel(
        tabPanel("plot",  tags$head(tags$style(HTML(progressBarStyle))),
                 downloadButton("ts_plot_PDF", label = "download (PDF)"),
                 downloadButton("ts_plot_PNG", label = "download (PNG)"),
                 plotOutput("ts_plot"))
        ,tabPanel("maps",   tags$head(tags$style(HTML(progressBarStyle))),
                  downloadButton("ts_maps_PDF", label = "download (PDF)"),
                  downloadButton("ts_maps_PNG", label = "download (PNG)"),
                  plotOutput("ts_maps"))
      )
      )
    )
  })
  
  # default thresholds
  dailyThr <- reactive({
    if(exists("input") && "pollutantPeriod"%in%names(input)) {
      if(input$pollutantPeriod=="PM10")  dT <- 50
      if(input$pollutantPeriod=="PM2.5") dT <- 40
      if(input$pollutantPeriod=="O3")    dT <- 180
      if(input$pollutantPeriod=="NO2")   dT <- 200
    } else {
      dT <- 50
    }
    dT
  })

  # UI: exceedances------------------------------------------------------------------
  output$ui_exc <- renderUI({
    sidebarLayout(
      sidebarPanel(
        helpText("Here you can analize exceedances of a given threshold."),
        conditionalPanel(
          condition = "! input.goPeriod",
          helpText("Please select the period of interest in ",strong("observations > data"),".")),
        conditionalPanel(
          condition = "input.goPeriod",
          helpText("You can change the period of interest in ",strong("observations > data"),"."),
          sliderInput("threshold","threshold",
                      min = round(dailyThr()*0.1,-1),
                      max = dailyThr()*3,
                      value = dailyThr(),
                      step = 10)
          ,permissionButton(id="buttonPermis_obsexc", sources=input$sources),
          bsModal("modalPermis_obsexc",title = "Permissions", trigger = "buttonPermis_obsexc", 
                  HTML(policyByPurpose(input$sources))),
          bsButton("buttonCite_obsexc", label = "citations", style="primary"),
          bsModal("modalCite_obsexc",title = "Citations", trigger = "buttonCite_obsexc", 
                  HTML(citeHtml(pkgs = c("ggplot2","base","shiny","ggmap"))))
        ),
        width=3),
      mainPanel(
        tabsetPanel(
          tabPanel("plot",  tags$head(tags$style(HTML(progressBarStyle))),
                   downloadButton("exc_plot_PDF", label = "download (PDF)"),
                   downloadButton("exc_plot_PNG", label = "download (PNG)"),
                   plotOutput("exc_plot"))
          ,tabPanel("map",   tags$head(tags$style(HTML(progressBarStyle))),
                    downloadButton("exc_map_PDF", label = "download (PDF)"),
                    downloadButton("exc_map_PNG", label = "download (PNG)"),
                    plotOutput("exc_map"))
          ,tabPanel("table", DT::dataTableOutput("exc_table"))
        ))
    )
  })
  
  # UI: clustering------------------------------------------------------------------
  output$ui_clu <- renderUI({
    sidebarLayout(
      sidebarPanel(helpText("Here you perform cluster analysis of the time series."),
                   conditionalPanel(condition = "! input.goPeriod",
                                    helpText("Please select the period of interest in ",strong("observations > data"),".")),
                   conditionalPanel(condition = "input.goPeriod",
                                    helpText("You can change the period of interest in ",strong("observations > data"),"."),
                                    bsCollapse(
                                      bsCollapsePanel(
                                        title = "clustering options", style = "default",
                                        sliderInput("nclu","number of clusters",2,10,5),
                                        sliderInput("clu_req","data needed (%)",50,100,80,5),
                                        checkboxInput("clu_stand","standardize data before clustering?",FALSE),
                                        selectInput("clu_metr",label = "metric",choices = c("manhattan","euclidean"))
                                      ),
                                      bsCollapsePanel(
                                        title = "layout", style = "default",
                                        helpText("Change here some details of the time series plot. Note that 'medoid' of a cluster is its most representative station."),
                                        selectInput("clu_add",label = "line",choices = c("medoid","mean","median","none")),
                                        checkboxInput("clu_box","boxplot",TRUE),
                                        checkboxInput("clu_log","logarithmic scale in y",TRUE)
                                      )
                                    )
                                    ,permissionButton(id="buttonPermis_obsclu", sources=input$sources),
                                    bsModal("modalPermis_obsclu",title = "Permissions", trigger = "buttonPermis_obsclu", 
                                            HTML(policyByPurpose(input$sources))),
                                    bsButton("buttonCite_obsclu", label = "citations", style="primary"),
                                    bsModal("modalCite_obsclu",title = "Citations", trigger = "buttonCite_obsclu", 
                                            HTML(citeHtml(pkgs = c("ggplot2","base","shiny","ggmap","ggrepel","cluster"))))
                   ),
                   width=3),
      mainPanel(tabsetPanel(
        tabPanel("map",   tags$head(tags$style(HTML(progressBarStyle))),
                 downloadButton("clu_map_PDF", label = "download (PDF)"),
                 downloadButton("clu_map_PNG", label = "download (PNG)"),
                 plotOutput("clu_map"))
        ,tabPanel("plot",  tags$head(tags$style(HTML(progressBarStyle))),
                  downloadButton("clu_plot_PDF", label = "download (PDF)"),
                  downloadButton("clu_plot_PNG", label = "download (PNG)"),
                  plotOutput("clu_plot"))
        ,tabPanel("table", DT::dataTableOutput("clu_table"))
      ))
    )
  })
  
  # data table------------------------------------------------------------------
  output$df <- DT::renderDataTable(options = list(pageLength = 10),
                               expr={
                                 withProgress(message = 'Loading...', value = 1, {
                                   dataOfPeriod()
                                 })
                               })
  
  # peaks detection------------------------------------------------------------------
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
  
  # plot time series------------------------------------------------------------------
  tsPlot <- reactive({
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
  })
  # render the plot
  output$ts_plot <- renderPlot({
    print(tsPlot())
  },height = function() ifelse(input$splitby=="none",600,900)
  )
  # save the plot
  output$ts_plot_PDF <- downloadHandler("timeseries_plot.pdf",function(file){ggsave(file,tsPlot(),width=12,height=9)})
  output$ts_plot_PNG <- downloadHandler("timeseries_plot.png",function(file){ggsave(file,tsPlot(),width=12,height=9)})
  
  # time series: maps------------------------------------------------------------------
  tsMaps <- reactive({
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
  })
  # render the plot
  output$ts_maps <- renderPlot({
    print(tsMaps())
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
  # save the plot
  output$ts_maps_PDF <- downloadHandler("timeseries_maps.pdf",function(file){ggsave(file,tsMaps(),width=12,height=12)})
  output$ts_maps_PNG <- downloadHandler("timeseries_maps.png",function(file){ggsave(file,tsMaps(),width=12,height=12)})
  
  # exceedances: barplot------------------------------------------------------------------
  excPlot <- reactive({
    dataOfPeriod() %>% dplyr::mutate(Exc=Value>input$threshold,
                                     notExc=Value<=input$threshold) %>%
      group_by(Day) %>%
      dplyr::summarize(notExceeding=sum(notExc, na.rm=T),
                       Exceeding=sum(Exc, na.rm=T)) %>%
      gather(key=Status,value=Stations,-Day) %>%
      dplyr::mutate(Day=as.Date(as.character(Day)))-> Dat
    sou <- unique(dataOfPeriod()$Source)
    pl <- ggplot(data=Dat, aes(x=Day,y=Stations,fill=Status)) + geom_col() + 
      theme_bw() + scale_fill_manual(values=c(notExceeding="olivedrab",Exceeding="orangered")) +
      labs(title=bquote(.(input$pollutantPeriod)*": number of stations with "*.(dailyInd())*" above "*.(input$threshold)~mu*g/m^3),
           subtitle=paste0("period: ",paste(unique(input$daterange),collapse=" to ")),
           caption=paste(collapse="\n",strwrap(paste0("data source","s"[length(sou)>1],": ",
                          paste(sou,collapse=", ")),exdent=5)))
    withProgress(message = 'Making plot...', value = 1, {
      pl
    })
  })
  # render the plot
  output$exc_plot <- renderPlot({
    print(excPlot())
  })
  # save the plot
  output$exc_plot_PDF <- downloadHandler("exceedances_barplot.pdf",function(file){ggsave(file,excPlot(),width=9,height=5)})
  output$exc_plot_PNG <- downloadHandler("exceedances_barplot.png",function(file){ggsave(file,excPlot(),width=9,height=5)})
  
  # exceedances: map------------------------------------------------------------------
  excMap <- reactive({
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
    Dat %>% mutate(Exceedances=cut(Exceed,bb,include.lowest = T)) %>% dplyr::arrange(Exceed)-> Dat
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
  })
  # render the plot
  output$exc_map <- renderPlot({
    print(excMap())
  },height = 800)
  # save the plot
  output$exc_map_PDF <- downloadHandler("exceedances_map.pdf",function(file){ggsave(file,excMap(),width=10,height=12)})
  output$exc_map_PNG <- downloadHandler("exceedances_map.png",function(file){ggsave(file,excMap(),width=10,height=12)})
  
  # exceedances: table------------------------------------------------------------------
  output$exc_table <- DT::renderDataTable({
    dataOfPeriod() %>% dplyr::mutate(Exc=Value>input$threshold) %>% group_by(Name,Lat,Lon) %>%
      dplyr::summarize(Exceed=sum(Exc, na.rm=T),
                       Valid=sum(!is.na(Exc))) -> Dat
    bb <- unique(round(quantile(Dat$Exceed,c(0,20,40,60,80,90,100)/100)))
    Dat %>% mutate(Exceedances=cut(Exceed,bb,include.lowest = T)) %>% dplyr::arrange(desc(Exceed))-> Dat
    Dat
  })
  
  # clustering------------------------------------------------------------------
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
  output$clu_table <- DT::renderDataTable({
    dataWithClusters() %>% dplyr::select(Name,Source,Cluster,isMedoid) %>% distinct() -> Dat
    Dat
  })
  
  # clustering: plot
  cluPlot <- reactive({
    Dat <- dataWithClusters() %>% 
      dplyr::mutate(col=c("odd","even")[1 + as.numeric(as.Date(as.character(Day)) - as.Date("1900-01-01")) %% 2])
    ymin <- max(1,min(Dat$Value,na.rm=T))
    ymax <- max(Dat$Value,na.rm=T)*1.1
    ggplot(Dat, aes(x=Day, y=Value, colour=Cluster)) +
      geom_crossbar(aes(x=Day, y=ymin, ymin=ymin, ymax=ymax, fill = col),
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
  # render the plot
  output$clu_plot <- renderPlot({
    print(cluPlot())
  })
  # save the plot
  output$clu_plot_PDF <- downloadHandler("cluster_timeseries.pdf",function(file){ggsave(file,cluPlot(),width=12,height=9)})
  output$clu_plot_PNG <- downloadHandler("cluster_timeseries.png",function(file){ggsave(file,cluPlot(),width=12,height=9)})
  
  
  # clustering: map
  cluMap <- reactive({
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
  })
  # render the plot
  output$clu_map <- renderPlot({
    print(cluMap())
  },height = 800)
  # save the plot
  output$clu_map_PDF <- downloadHandler("cluster_map.pdf",function(file){ggsave(file,cluMap(),width=10,height=12)})
  output$clu_map_PNG <- downloadHandler("cluster_map.png",function(file){ggsave(file,cluMap(),width=10,height=12)})
  
  
  # obs.data: daily map -----------------------------------------------------
  
  # breaks for the obs.map
  Breaks <- reactive({
    if(input$scaleDay == "classic") {
      if(input$pollutantDay=="PM10")  bb <- c(0,25,50,75,100,150)
      if(input$pollutantDay=="PM2.5") bb <- c(0,10,25,50,75,100)
      if(input$pollutantDay=="NO2")   bb <- c(0,25,50,75,100,150)
      if(input$pollutantDay=="O3")    bb <- c(0,90,120,180,240,300)
    }else if(input$scaleDay == "high values") {
      if(input$pollutantDay=="PM10")  bb <- c(0,25,50,75,100,150,200,250,300,400)
      if(input$pollutantDay=="PM2.5") bb <- c(0,10,25,50,75,100,150,200,250,300)
      if(input$pollutantDay=="NO2")   bb <- c(0,25,50,75,100,150,200,250,300,400)
      if(input$pollutantDay=="O3")    bb <- c(0,60,90,120,150,180,210,240,270,300)
    }
    bb
  })
  
  # units
  unitDay <- reactive({
    if(input$pollutantDay %in% c("PM10","PM2.5","O3","NO2")) uu <- "ug/m^3"
    uu
  })
  
  # daily map: palette
  colorpal <- reactive({
    if(input$scaleDay == "classic") {
      cc <- c("steelblue","olivedrab","orange","red","purple")
    }else if(input$scaleDay == "high values") {
      cc <- c("white",rev(RColorBrewer::brewer.pal(name="Spectral",n=length(Breaks())-4)),"magenta","black")
    }
    colorFactor(palette = cc, 
                domain = cut(Breaks(),Breaks(),
                             ordered_result=TRUE, include.lowest=TRUE))
  })
  
  # daily obs. map: initialize basemap
  output$Map <- renderLeaflet({
    lon0 <- userLocation()$lon
    lat0 <- userLocation()$lat
    leaflet() %>% 
      setView(lon0, lat0, 7) %>% 
      addTiles(group = "classic")  %>% 
      addProviderTiles("CartoDB.Positron", group = "grey minimal") %>%
      addProviderTiles("Stamen.TonerLite", group = "toner lite") %>%
      addProviderTiles("Esri.WorldImagery", group = "satellite") %>%
      addProviderTiles("Thunderforest.Landscape", group = "terrain") %>%

      # Layers control
      addLayersControl(
        baseGroups = c("grey minimal","classic","toner lite","terrain","satellite"),
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
    
    if (!is.null(Dat) && nrow(Dat)>0) {
      proxy %>% 
        clearControls() %>%
        addLegend(position = "bottomright", pal = Pal, values = ~ValueInterval,
                  title = paste0(as.character(input$day),":<br>",
                                 input$pollutantDay," ",dayInd(),"<br>(",
                                 unitDay(),")")
        )
    }
  })
  
  
  # UI: map of models -----------------------------------------------------
  avail_modeldays <- unique(as.Date(substr(dir(path="/home/giovanni/R/projects/calicantus/data/mod-data/grid/",
                                               pattern="rda",recursive = T),1,10), 
                                    format ="%Y/%m/%d"))
  avail_models <- c(#"CHIMERE","EMEP","EURAD","LOTOSEUROS","MATCH","MOCAGE","SILAM",
                    "FARMEurope","FARMItaly","CAMxItaly","CHIMERENorthernItaly")
  maxscad <- 3
  output$ui_modelmap <- renderUI({
    sidebarLayout(
      sidebarPanel(
        helpText("Here you can select the day to plot in the map."),
        dateInput("dayMod", "day of interest", value=Sys.Date(), 
                  min=min(avail_modeldays), max = max(avail_modeldays)+maxscad,
                  width="50%"),
        selectInput("pollutantDayMod",label = "pollutant",
                    choices = list("PM10 daily average"="PM10_Mean",
                                   "ozone daily maximum"="O3_Max",
                                   "ozone daily maximum of 8h running mean"="O3_MaxAvg8h",
                                   "PM2.5 daily average"="PM25_Mean")),
        selectInput("scaleDayMod",label = "color scale",
                    choices = c("classic","high values","continuous","extended"), 
                    selected = "continuous"),
        selectInput("mapMod",label = "model", choices = avail_models, selected = sample(avail_models, 1)),
        bsButton("buttonLicense_modmap", label = "license", style="primary", icon=icon("external-link"),
                 onclick ="window.open('http://macc-raq.copernicus-atmosphere.eu/doc/CAMS_data_license_final.pdf', '_blank')"),
        bsButton("buttonInfo_modmap", label = "info", style="primary", icon=icon("external-link"),
                 onclick ="window.open('https://atmosphere.copernicus.eu/documentation-regional-systems', '_blank')"),
        bsButton("buttonCite_modmap", label = "citations", style="primary"),
        bsModal("modalCite_modmap",title = "Citations", trigger = "buttonCite_modmap", 
                HTML(citeHtml(files = c("/home/giovanni/R/projects/calicantus/CITATION",
                                        "/home/giovanni/R/projects/calicantus/shiny/citations/citation_cams.R",
                                        "/home/giovanni/R/projects/calicantus/shiny/citations/citation_RSE.R",
                                        "/home/giovanni/R/projects/calicantus/shiny/citations/citation_ARIANET.R"),
                              pkgs = c("raster","base","shiny","leaflet")))),
        width=3),
      mainPanel(leafletOutput("MapMod", width="100%", height="800px"))
    )
  })
  
  
  # UI: timeseries of models forecast -----------------------------------------------------
  rangeLat <- c( 30.05,69.95)
  rangeLon <- c(-24.95,44.95)
  modelTimeseries <- reactive({
    refDays <- as.Date(format(Sys.time()-8*3600-(0:7)*3600*24, "%Y-%m-%d"))
    dat <- NULL
    for (mS in modelSources) {
      Files <- paste0("/home/giovanni/R/projects/calicantus/data/mod-data/timeseries/",
                      format(refDays,paste0("%Y/%m/%d/",mS,"_ref%Y%m%d")),
                      "_timeseries.rda")
      File <- Files[which(file.exists(Files))[1]]
      if(exists("cities_data")) rm("cities_data")
      if(file.exists(File))load(File)
      if(exists("cities_data")) {
        Dat <- cities_data # manage different name
        Dat$Name <- as.character(Dat$Name)
        dat <- bind_rows(dat,
                         Dat %>% 
                           dplyr::mutate(Model= if (exists('Grid', where = Dat)) paste0(Model,Grid) else Model) %>%
                           dplyr::select(Lon,Lat,Name,Conc,Time,Pollutant,Model))
      } 
    }
    return(dat)
  })
  aoiCities <- eventReactive(input$plot_brush$ymin,{
    ee <- try({aoi <- input$plot_brush}, silent = T)
    if(class(ee)=="try-error"||is.null(input$plot_brush)) {
      aoi <- list(xmin=rangeLon[1],
                  xmax=rangeLon[2],
                  ymin=rangeLat[1],
                  ymax=rangeLat[2])
     }
    unique(modelTimeseries()[,c("Lon","Lat","Name")]) %>%
      mutate(inAoi=as.numeric(Lon>=aoi$xmin &
                                Lon<=aoi$xmax &
                                Lat>=aoi$ymin &
                                Lat<=aoi$ymax)) %>%
      arrange(Name) -> dat
    if(max(dat$inAoi)==0) dat$inAoi <- 1
    dat
  },ignoreNULL=F)
  areaOfMap <- reactive({
    cc <- subset(aoiCities(),inAoi==1)
    xlim <- range(cc$Lon); dx <- xlim[2]-xlim[1]; xlim[1] <- xlim[1]-dx*0.3; xlim[2] <- xlim[2]+dx*0.3
    ylim <- range(cc$Lat); dy <- ylim[2]-ylim[1]; ylim[1] <- ylim[1]-dy*0.3; ylim[2] <- ylim[2]+dy*0.3
    list(xlim=xlim,ylim=ylim)
  })
  output$selectOnMap <- renderPlot({
    cc <- aoiCities()
    map('world',xlim=areaOfMap()$xlim,ylim=areaOfMap()$ylim,col="grey60")
    nn <- as.numeric(cc$Name %in% input$tsmod_selCities)
    ii <- cc$inAoi+1+nn
    points(cc[,c("Lon","Lat")],cex=c(0.6,0.6,1)[ii],
           col=c("grey80","olivedrab","orange")[ii],pch=19)
  })
  output$ui_tsmod <- renderUI({
    sidebarLayout(
      sidebarPanel(
        helpText(paste0("Here you can plot selected models forecasts as time series on selected cities.")),
        selectInput("tsmod_selModels",label = "models",
                    choices = avail_models,
                    selected = sample(avail_models, 3),
                    multiple=T, selectize = F, size = 10),
        bsTooltip("tsmod_selModels","Three models have been randomly selected.", placement = "right"),
        bsCollapse(
          bsCollapsePanel(
            title = "cities", style = "default",
            plotOutput("selectOnMap", brush = "plot_brush"),
            bsTooltip("selectOnMap","Click and drag to pre-select cities in a box. Select an empty area to go back.", placement = "right"),
            selectInput("tsmod_selCities",label = "cities",
                        choices = aoiCities()$Name[which(aoiCities()$inAoi==1)],
                        selected = unique(sample(aoiCities()$Name[which(aoiCities()$inAoi==1)],3,replace=T)),
                        multiple=T, selectize = F, size = 6),
            bsTooltip("tsmod_selCities","Select the cities for the plot (Ctrl+click for multiple selection)", placement = "right")
          )
        ),
        bsButton("buttonLicense_modts", label = "license", style="primary", icon=icon("external-link"),
                 onclick ="window.open('http://macc-raq.copernicus-atmosphere.eu/doc/CAMS_data_license_final.pdf', '_blank')"),
        bsButton("buttonInfo_modts", label = "info", style="primary", icon=icon("external-link"),
                 onclick ="window.open('https://atmosphere.copernicus.eu/documentation-regional-systems', '_blank')"),
        bsButton("buttonCite_modts", label = "citations", style="primary"),
        bsModal("modalCite_modts",title = "Citations", trigger = "buttonCite_modts", 
                HTML(citeHtml(files = c("/home/giovanni/R/projects/calicantus/CITATION",
                                        "/home/giovanni/R/projects/calicantus/shiny/citations/citation_cams.R",
                                        "/home/giovanni/R/projects/calicantus/shiny/citations/citation_RSE.R",
                                        "/home/giovanni/R/projects/calicantus/shiny/citations/citation_ARIANET.R"),
                              pkgs = c("ggplot2","base","shiny","dplyr")))),
        width=3),
      mainPanel( 
        tags$head(tags$style(HTML(progressBarStyle))),
        downloadButton("tsmod_PDF", label = "download (PDF)"),
        downloadButton("tsmod_PNG", label = "download (PNG)"),
        plotOutput("tsmod")
        )
    )
  })
  pollName <- function(x) ifelse(x=="PM25","PM2.5",as.character(x))
  tsMod <- reactive({
    dat <- modelTimeseries() %>%
      filter(Name %in% input$tsmod_selCities,
             Model %in% input$tsmod_selModels) %>%
      mutate(Pollutant=pollName(Pollutant))
    p <- ggplot(dat, aes(x=Time,y=Conc,col=Model,group=Model)) +
      geom_line() + geom_point(alpha=0.5) +
      theme_bw() +
      facet_grid(Pollutant~Name,scales = "free_y")
    p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(title=paste0(paste(unique(dat$Pollutant), collapse=", "),": hourly forecast"),
           subtitle=paste0("period: ",paste(range(dat$Time),collapse=" to ")),
           caption=modelTimeseriesAttr()) +
      ylab(expression("Concentration"~(mu*g/m^3))) +
      scale_x_datetime(date_labels = "%Y-%m-%d")
    withProgress(message = 'Making plot...', value = 1, {
      p
    })
  })
  # render the plot
  output$tsmod <- renderPlot({
    print(tsMod())
  },height = 800)
  # save the plot
  output$tsmod_PDF <- downloadHandler("timeseries_models.pdf",function(file){ggsave(file,tsMod(),width=12,height=12)})
  output$tsmod_PNG <- downloadHandler("timeseries_models.png",function(file){ggsave(file,tsMod(),width=12,height=12)})
  
  
# daily models map -----------------------------------------------------
  
  # model source
  modelSources <- c(#"CAMS50", 
    "RSE", "ARIANET", "ARPAE")
  modelSource <- reactive({
    model <- ifelse(is.null(input) || is.null(input$mapMod), "FARMItaly", input$mapMod)
    if(model %in% c("CHIMERE","EMEP","EURAD","LOTOSEUROS","MATCH","MOCAGE","SILAM")) {
      sou <- "CAMS50"
    }
    if(model %in% c("CAMxItaly")) {
      sou <- "RSE"
    }
    if(model %in% c("FARMItaly", "FARMEurope")) {
      sou <- "ARIANET"
    }
    if(model %in% c("CHIMERENorthernItaly")) {
      sou <- "ARPAE"
    }
    sou
  })

    # load model data of single day
  changeOfDayMod <- reactive({
    input$dayMod;input$mapMod;input$pollutantDayMod
  })
  modelOfDay <- eventReactive(
    eventExpr = changeOfDayMod(), #ignoreNULL = FALSE,
    valueExpr = {
      valDay <- try(as.Date(input$dayMod))
      if(class(valDay)[1]=="try-error") valDay <- Sys.Date()
      model <- ifelse(is.null(input) || is.null(input$mapMod), "CHIMERE", input$mapMod)
      poll <- ifelse(is.null(input) || is.null(input$pollutantDayMod), "PM10_Mean", input$pollutantDayMod)
      if(exists("r")) rm("r")
      Files <- system(paste0("ls /home/giovanni/R/projects/calicantus/data/mod-data/grid/????/??/??/",
                             modelSource(),"_ref????????", format(valDay,"_val%Y%m%d_"),
                             model,"_",poll,".rda"), intern=T)
      if(length(Files)>0) {
        File <- Files[length(Files)]
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
    if(scale %in% c("classic")) {
      if(poll=="PM10_Mean")   bb <- c(0,25,50,75,100,150)
      if(poll=="PM25_Mean")   bb <- c(0,10,25,50,75,100)
      if(poll=="O3_Max")      bb <- c(0,90,120,180,240,300)
      if(poll=="O3_MaxAvg8h") bb <- c(0,60,90,120,150,180)
    }else if(scale %in% c("continuous")) {
      if(poll=="PM10_Mean")   bb <- c(0,25,50,75,100,150,200)
      if(poll=="PM25_Mean")   bb <- c(0,10,25,50,75,100,150)
      if(poll=="O3_Max")      bb <- c(60,90,120,150,180,210,240,270)
      if(poll=="O3_MaxAvg8h") bb <- c(60,90,120,150,180,210,240,270)
    }else if(scale %in% c("high values","extended")) {
      if(poll=="PM10_Mean")   bb <- c(0,25,50,75,100,150,200,250,300,400)
      if(poll=="PM25_Mean")   bb <- c(0,10,25,50,75,100,150,200,250,300)
      if(poll=="O3_Max")      bb <- c(0,60,90,120,150,180,210,240,270,300)
      if(poll=="O3_MaxAvg8h") bb <- c(0,60,90,120,150,180,210,240,270,300)
    }
    bb
  })

  # model units
  unitDayMod <- reactive({
    poll <- ifelse(is.null(input) || is.null(input$pollutantDayMod), "PM10_Mean", input$pollutantDayMod)
    poll <- strsplit(poll,split = "_")[[1]][1]
    if(poll %in% c("PM10","O3","PM25")) uu <- '<span class="math"><em>μg/m</em><sup>3</sup></span>'
    uu
  })
  
  # model attribution
  model_attr <- function(models) {
    att <- NULL
    if(any(models %in% c("CHIMERE","EMEP","EURAD","LOTOSEUROS","MATCH","MOCAGE","SILAM"))) {
      att <- c(att,paste0("Generated using Copernicus Atmosphere Monitoring Service Information ", 
                          format(Sys.Date(),"%Y"), " (",
                          paste(intersect(models, c("CHIMERE","EMEP","EURAD","LOTOSEUROS","MATCH","MOCAGE","SILAM")),
                                collapse=", "),")"))
    }
    if(any(models %in% c("CAMxItaly"))) {
      att <- c(att,paste0("Forecast by RSE S.p.A. (Research on Energy Systems) ", 
                          format(Sys.Date(),"%Y"), " (",
                          paste(intersect(models, c("CAMxItaly")),
                                collapse=", "),")"))
    }
    if(any(models %in% c("FARMItaly", "FARMEurope"))) {
      att <- c(att,paste0("Forecast by ARIANET Srl ", 
                          format(Sys.Date(),"%Y"), " (",
                          paste(intersect(models, c("FARMItaly", "FARMEurope")),
                                collapse=", "),")"))
    }
    if(any(models %in% c("CHIMERENorthernItaly"))) {
      att <- c(att,paste0("Forecast by Arpae ", 
                          format(Sys.Date(),"%Y"), " (",
                          paste(intersect(models, c("CHIMERENorthernItaly")),
                                collapse=", "),")"))
    }
    paste(att, collapse="\n")
  }
  modelMapAttr <- reactive({
    model <- ifelse(is.null(input) || is.null(input$mapMod), "CHIMERE", input$mapMod)
    model_attr(model)
  })
  modelTimeseriesAttr <- reactive({
    models <- input$tsmod_selModels
    model_attr(models)
  })
  
  # daily model map: palette
  colorpalMod <- reactive({
    scale <- ifelse(is.null(input) || is.null(input$scaleDayMod), "classic", input$scaleDayMod)
    if(scale == "classic") {
      cc <- c("white", "olivedrab","orange","red","purple")
    }else if(scale %in% c("high values","continuous","extended")) {
      cc <- c("white", rev(RColorBrewer::brewer.pal(name="Spectral", n=length(BreaksMod())-3)), "magenta","black")
    }
    bb <- BreaksMod()
    Dat <- modelOfDay()
    if(!is.null(Dat)) {
      rmax <- ceiling(cellStats(modelOfDay(),"max"))
      rmin <- floor(cellStats(modelOfDay(),"min"))
    } else {
      rmax <- max(bb)
      rmin <- min(bb)
    }
    if(scale == "continuous") {
      colorNumeric(palette = cc, domain = range(bb), na.color = "transparent")
    }else if(scale == "extended") {
      colorNumeric(palette = cc, domain = c(0,rmax), na.color = "transparent")
    }else{
      if(max(bb)<rmax) bb[length(bb)] <- rmax
      colorBin(palette = cc, domain = range(bb), bins = bb, na.color = "transparent")
    }
    })
  
  
  # daily models map: initialize basemap
  output$MapMod <- renderLeaflet({
    lon0 <- userLocation()$lon
    lat0 <- userLocation()$lat
    leaflet() %>% 
      setView(lon0, lat0, 6) %>% 
      addTiles(group = "classic")  %>% 
      addProviderTiles("CartoDB.Positron", group = "grey minimal") 
  })
  
  # daily models map: dynamic layer
  observe({
    Dat <- modelOfDay()
    Pal <- colorpalMod()
    
    leafletProxy("MapMod",session) %>% clearShapes() %>% clearImages() -> map
    if (!is.null(Dat) && length(values(Dat))>0) {
      map %>% 
        addRasterImage(x = Dat, colors = Pal, opacity = 0.5, layerId = "model", 
                       attribution = modelMapAttr()) -> map
    }else{
      showModal(modalDialog(
        title = "No data.", footer = modalButton("Ok"),
        paste0("Forecast of model ",input$mapMod,
               " not (yet) available for day ",input$dayMod,"."),
        br(),"Please change model or date.",
        easyClose = TRUE
      ))
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
    scale <- ifelse(is.null(input) || is.null(input$scaleDayMod), "classic", input$scaleDayMod)
    
    if(!is.null(Dat)) {
      vv <- values(Dat)
      if(scale=="continuous") vv <- BreaksMod() 
      proxyMod %>% 
        clearControls() %>%
        addLegend(position = "bottomright", pal = Pal, values = vv,
                  title = paste0("model ",model,"<br>",
                                 as.character(input$dayMod),":<br>",
                                 poll,"<br>(",
                                 unitDayMod(),")")
        )
    }
  })
  
  
  # more  -----------------------------------------------------
  # data use policy table
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
  
  # support for debug
  output$sessionInfo <- renderPrint({sessionInfo()})
  output$uiInput <- renderPrint({input})
  output$summary <- renderPrint({summary(dataOfDay())})
  output$libPaths <- renderPrint({.libPaths()})
  
  # contacts
  output$contacts <- renderTable({
    read.table("/home/giovanni/R/projects/calicantus/data/data-sources/contacts.csv",
               sep=",",row.names = NULL,header = T, check.names = F)
  })

  # login statistics
  Logins <- read.table("/home/giovanni/R/projects/calicantus/log/web-interface.log", sep="|", 
                       fill=T, stringsAsFactors = F, flush = T)[,1:2]
  colnames(Logins) <- c("time","user")
  Logins %>% tidyr::separate(time, c("kk","time"), ": ") %>% 
    tidyr::separate(user, c("ll","user"), ": ") %>%
    dplyr::filter(kk=="login success", user!="test") %>% 
    dplyr::select(time, user) %>%
    dplyr::mutate(day=as.Date(time)) -> Logins
  
  output$loginStat <- renderPlot({
    p <- ggplot(Logins, aes(x=day, y=user)) +
      geom_count(col="olivedrab", show.legend = T) + 
      theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ggtitle("web interface", subtitle = "number of accesses") + 
      xlim(Sys.Date()-30, Sys.Date())
    print(p)
  })
  
  output$loginTrend <- renderPlot({
    p <- ggplot(Logins %>%
                  dplyr::mutate(week=format(day,"%Y-%V")),
                aes(x=week)) +
      geom_bar(fill="tomato4", show.legend = T) + 
      theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ggtitle("web interface", subtitle = "number of accesses")
    print(p)
  })
  
  # users locations
  userLocation <- reactive({
    uu <- read.table("/home/giovanni/R/projects/calicantus/config/config_users.csv", 
                     stringsAsFactors = F, header = T, quote = '\"', fill=T, sep=",",
                     strip.white = T)
    uu %>% dplyr::filter(as.character(user) == as.character(input$Usr)) %>% dplyr::select(lon, lat)
  })
  output$usersCities <- renderPlot({
    uu <- read.table("/home/giovanni/R/projects/calicantus/config/config_users.csv", 
                     stringsAsFactors = F, header = T, quote = '\"', fill=T, sep=",",
                     strip.white = T)
    uu$isMe <- uu$user == input$Usr
    xmin<-min(uu$lon,na.rm=T)
    xmax<-max(uu$lon,na.rm=T)
    ymin<-min(uu$lat,na.rm=T)
    ymax<-max(uu$lat,na.rm=T)
    dx<-xmax-xmin
    dy<-ymax-ymin
    pl <- ggplot() + 
      geom_polygon(data=MapData, aes(x=long, y=lat, group = group),
                   colour="grey50", fill="white" ) +
      # geom_label_repel(data=uu %>% mutate(user=ifelse(isMe,user,"")),
      #                  aes(x=lon, y=lat, label=user),
      #                  box.padding = unit(0.2, "lines"),
      #                  label.padding = unit(0.2, "lines"),
      #                  size=4, col="steelblue", show.legend=F,
      #                  segment.size=0, max.iter = 5000, alpha = 0.8) +
      geom_point(data=uu, aes(x=lon, y=lat), size=1.8, shape=21, col="red", fill="red") +
      # scale_fill_manual(values=c("red","steelblue")) +
      coord_map(xlim = c(xmin-dx*0.05,xmax+dx*0.05),
                ylim = c(ymin-dy*0.05,ymax+dy*0.05), 
                projection = "lambert",
                lat0=ymin, lat1=ymax) +
      # coord_map(xlim=c(-180,180)) +
      theme_bw()+
      theme(axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank()) +
      ggtitle("web interface", subtitle = "active users")
      
    pl
  })
  
  # data coverage
  Files <- basename(dir(path = "/home/giovanni/R/projects/calicantus/data/obs-data/", 
                        pattern = "rda", full.names = F, recursive = T))
  avai <- as.data.frame(stri_split_fixed(gsub(".rda","",Files),"_",simplify=T))
  colnames(avai) <- c("day","pollutant","data_source")
  avai %>% mutate(day=as.Date(day,format = "%Y%m%d"), YearMonth=format(day,"%Y-%m")) -> avai
  # all data coverage plot
  output$availObs <- renderPlot({
    p <- ggplot(avai, aes(x=YearMonth, y=data_source)) +
      geom_count(col="tomato3", show.legend=F) + 
      theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      facet_grid(pollutant~.) +
      ggtitle("measured data availability", sub = "whole dataset")
    print(p)
  })
  # recent data coverage plot
  nd <- 30
  output$availObsRecent <- renderPlot({
    p <- ggplot(avai, aes(x=day, y=data_source)) +
      xlim(Sys.Date()-nd,Sys.Date()) +
      geom_count(col="steelblue", show.legend=F) + 
      theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      facet_grid(pollutant~.) +
      ggtitle("measured data availability", sub = paste0("last ",nd," days"))
    print(p)
  })
  
  # account info
  output$ui_account <- renderUI({
    fluidPage(
      p("You are logged in as: ",strong(input$Usr),
        ". Expire date: ",expDate()),
      column(6,
             plotOutput("loginStat",width="100%"),
             plotOutput("loginTrend",width="100%")),
      column(6,
             plotOutput("usersCities",width="100%"))
    )
  })
  
  # obs.data availability
  output$ui_obsAvail <- renderUI({
    fluidPage(
      column(8,p("The data sources available for you are: ",
                 strong(paste(availableSources(),collapse=", ")))),
      column(6,plotOutput("availObs",height="800px")),
      column(6,plotOutput("availObsRecent",height="800px"))
    )
  })
}

shinyApp(ui = ui, server = server)