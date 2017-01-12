
# preliminar --------------------------------------------------------------


#packages
suppressMessages({
  pkgO <- names(sessionInfo()$otherPkgs)
  pkg1 <- c(pkgO)
  pkg2 <- setdiff(pkg1,"shiny")
  if(length(pkg2)>0) lapply(paste0('package:',pkg2), detach, character.only = TRUE, unload = TRUE)
 # .libPaths(new=unique(c("/home/giovanni/R/x86_64-pc-linux-gnu-library/3.2", .libPaths())))
  library(shiny, lib.loc = "/usr/lib/R/library")
 # library("dtw", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
  library(lubridate, lib.loc = "/usr/lib/R/library")
  library(dplyr, lib.loc = "/usr/lib/R/library")
  library(plyr, lib.loc = "/home/giovanni/R/x86_64-pc-linux-gnu-library/3.2")
#  library(shinyBS)
  library(shinyjs, lib.loc = "/usr/lib/R/library")
  library(leaflet, lib.loc = "/home/giovanni/R/x86_64-pc-linux-gnu-library/3.2")
 # library(DT, lib.loc = "/home/giovanni/R/x86_64-pc-linux-gnu-library/3.2")
  #library(rgdal, lib.loc = "/home/giovanni/R/x86_64-pc-linux-gnu-library/3.2")
  library(raster, lib.loc = "/usr/lib/R/library")
  library(data.table, lib.loc = "/home/giovanni/R/x86_64-pc-linux-gnu-library/3.2")
  library(tidyr, lib.loc = "/usr/lib/R/library")
  library(scales, lib.loc = "/home/giovanni/R/x86_64-pc-linux-gnu-library/3.2")
  library(ggplot2, lib.loc = "/home/giovanni/R/x86_64-pc-linux-gnu-library/3.2")
  library(ggrepel, lib.loc = "/home/giovanni/R/x86_64-pc-linux-gnu-library/3.2")
  library(RColorBrewer, lib.loc = "/usr/lib/R/library")
  library(maps, lib.loc = "/usr/lib/R/library")
  library(cluster, lib.loc = "/usr/lib/R/library")
  library(htmltools, lib.loc = "/usr/lib/R/library")
})

# credentials
source("/home/giovanni/R/projects/calicantus/config/ui_credentials.R")


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
    tags$li("Tuscany: ", a("Marco Stefanelli,", href="mailto:m.stefanelli@arpat.toscana.it"), 
            a("Bianca Patrizia Andreini", href="mailto:bp.andreini@arpat.toscana.it"), "(ARPAT)")
  ),
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
                  br(),actionButton("Login", "Log in"))),
    tags$style(type="text/css", "#login {font-size:12px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
  )}

# UI: webpage with tabs
ui_menu <- function(){tagList(tabPanel("map", ui_map),
                              tabPanel("data", ui_data),
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
        div(class="outer",do.call(navbarPage,c(inverse=TRUE,title = "[calicantus]",ui_menu())))
      })
      print(ui)
    }
  })
  
  # available data sources (could be personalized depending on user type...)
  availableSources <- reactive({
    aS <- system("cd /home/giovanni/R/projects/calicantus/data/sites-info/; ls metadata*csv | sed s/metadata.//g | sed s/.csv//g",intern=TRUE)
  })
  
  # UI: data table
  output$ui_data <- renderUI({
    sidebarLayout(
      sidebarPanel(helpText("Here you select the period you are interested in."),
                   dateRangeInput("daterange","period of interest",
                                  start=Sys.Date()-10,
                                  end=Sys.Date()-1,
                                  max = Sys.Date()-1),
                   selectInput("pollutant",label = "pollutant", choices = c("PM10")),
                   selectInput("sources",label = "sources of data", choices = availableSources(), 
                               selected = availableSources(), multiple=TRUE, selectize=FALSE),
                   actionButton("goPeriod", label="load", icon = icon("arrow-circle-right")),
                   conditionalPanel(condition = "input.goPeriod",
                                    helpText("Please note that the table on the right side is interactive:",
                                             "you can sort it by clicking on the header,",
                                             "look for a specific value using the 'search' tool,",
                                             "filter the data by writing in the cells below the table, and so on."),
                                    downloadButton('downloadData', 'download')),
                   width=3),
      mainPanel(dataTableOutput("df"))
    )
  })
  
  # load data of period
  dataOfPeriod <- eventReactive(eventExpr = input$goPeriod, 
                                valueExpr = {
                                  dd <- as.character(seq.Date(input$daterange[1],input$daterange[2],"1 day"))
                                  ff <- NULL
                                  for(d in dd) {
                                    ff <- c(ff,system(paste0("ls /home/giovanni/R/projects/calicantus/data/obs-data/",
                                                             format(as.Date(d),"%Y/%m/%d/%Y%m%d_"),input$pollutant,
                                                             "*.rda 2>/dev/null"),
                                                      intern=TRUE))
                                  }
                                  Dat <- NULL
                                  nf <- length(ff)
                                  for (i in 1:nf) {
                                    load(ff[i])
                                    Dat <- bind_rows(Dat,dat)
                                  }
                                  Dat %>% dplyr::filter(!is.na(Name) & !is.na(Lat) & !is.na(Lon) & Source%in%input$sources)  %>%
                                    dplyr::mutate(Value=round(Value)) %>% dplyr::arrange(desc(Value)) -> Dat
                                  return(Dat)
                                })
  
  # download data of period
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data_', input$pollutant, "_",
            paste(format(as.Date(input$daterange),"%Y%m%d"),collapse="-"), '.csv', sep='')
    },
    content = function(con) {
      dataOfPeriod() %>% dplyr::arrange(Day,Source) -> data
      write.csv(data, con, fileEncoding = "latin1")
    }
  )
  
  # load data of single day
  dataOfDay <- eventReactive(eventExpr = input$goDay, 
                             valueExpr = {
                               d <- as.character(input$day)
                               ff <- system(paste0("ls /home/giovanni/R/projects/calicantus/data/obs-data/",
                                                   format(as.Date(d),"%Y/%m/%d/%Y%m%d_*.rda 2>/dev/null")),
                                            intern=TRUE)
                               Dat <- NULL
                               nf <- length(ff)
                               for (i in 1:nf) {
                                 load(ff[i])
                                 Dat <- bind_rows(Dat,dat)
                               }
                               Dat$Value <- round(Dat$Value)
                               Dat <- dplyr::arrange(Dat, Value) %>% 
                                 filter(as.character(Day)==as.character(input$day)) %>%
                                 filter(!is.na(Value) & !is.na(Lat) & !is.na(Lon)) %>%
                                 mutate(ValueInterval=cut(Value
                                                          #,breaks=Breaks()
                                                          ,breaks=c(0,25,50,75,100,300)
                                                          ,ordered_result=TRUE, include.lowest=TRUE))
                               return(Dat)
                             })

  # UI: map
  output$ui_map <- renderUI({
    sidebarLayout(
      sidebarPanel(helpText("Here you can select the day to plot in the map."),
                   dateInput("day", "day of interest", value=Sys.Date()-1, 
                             min="2014-10-10", max = Sys.Date()-1,
                             width="50%"),
                   actionButton("goDay", label="plot", icon = icon("arrow-circle-right")),
                   conditionalPanel(condition = "input.goDay",
                                    helpText("Please note that the map on the right side is interactive:",
                                             "you can zoom in and out with the mouse wheel or with the '+' and '-',",
                                             "and click on a marker to get name of the station and observed concentration.")),
                   width=3),
      mainPanel(leafletOutput("Map", width="100%", height="800px"))
    )
  })
  
  # UI: timeseries
  output$ui_ts <- renderUI({
    sidebarLayout(
      sidebarPanel(helpText(paste0("Here you can plot aggregated timeseries, in the range from ",
                                   min(as.Date(dataOfPeriod()$Day))," to ",max(as.Date(dataOfPeriod()$Day)),
                                   ". To plot a different period of interest, please change it in the 'data' tab.")),
                   selectInput("colorby","color by",c("data source"="Source","same color"="same")),
                   selectInput("splitby","split by",c("data source"="Source","don't split"="none")),
                   selectInput("geom_type","plot type",c("boxplot","violin","jitter","blank")),
                   selectInput("timestep","time step",c("Day","Weekday","Year_Week","Year_Month")),
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
                   ),
                   #actionButton("goTs", label="plot", icon = icon("arrow-circle-right")),
                   width=3),
      mainPanel(plotOutput("ts"))
    )
  })
  
  # UI: exceedances
  output$ui_exc <- renderUI({
    sidebarLayout(
      sidebarPanel(helpText(paste0("Here you can analize exceedances of a given threshold, in the period from ",
                                   min(as.Date(dataOfPeriod()$Day))," to ",max(as.Date(dataOfPeriod()$Day)),
                                   ". To analize a different period of interest, please change it in the 'data' tab.")),
                   sliderInput("threshold","threshold",10,100,50,5),
                   width=3),
      mainPanel(tabsetPanel(
        tabPanel("plot",  plotOutput("exc_plot"))
        ,tabPanel("map",   plotOutput("exc_map"))
        ,tabPanel("table", dataTableOutput("exc_table"))
      ))
    )
  })
  
  # UI: clustering
  output$ui_clu <- renderUI({
    sidebarLayout(
      sidebarPanel(helpText(paste0("Here you perform cluster analysis of the time series, in the period from ",
                                   min(as.Date(dataOfPeriod()$Day))," to ",max(as.Date(dataOfPeriod()$Day)),
                                   ". To analize a different period of interest, please change it in the 'data' tab.")),
                   sliderInput("nclu","number of clusters",2,10,5),
                   sliderInput("clu_req","data needed (%)",50,100,80,5),
                   checkboxInput("clu_stand","standardize data before clustering?",FALSE),
                   selectInput("clu_metr",label = "metric",choices = c("manhattan","euclidean")),
                   hr(),
                   helpText("Change here some details of the time series plot. Note that 'medoid' of a cluster is its most representative station."),
                   selectInput("clu_add",label = "line",choices = c("medoid","mean","median","none")),
                   checkboxInput("clu_box","boxplot",TRUE),
                   checkboxInput("clu_log","logarithmic scale in y",TRUE),
                   width=3),
      mainPanel(tabsetPanel(
        tabPanel("map",   plotOutput("clu_map"))
        ,tabPanel("plot",  plotOutput("clu_plot"))
        ,tabPanel("table", dataTableOutput("clu_table"))
      ))
    )
  })
  
  # data table
  output$df <- renderDataTable(options = list(pageLength = 10),
                               expr={
                                 dataOfPeriod()
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
    pl <- pl + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    pl
  },height = function() ifelse(input$splitby=="none",600,900)
  )
  
  # exceedances: barplot
  output$exc_plot <- renderPlot({
    dataOfPeriod() %>% dplyr::mutate(Exc=Value>input$threshold,
                                     notExc=Value<=input$threshold) %>% group_by(Day) %>%
      dplyr::summarize(notExceeding=sum(notExc, na.rm=T),
                       Exceeding=sum(Exc, na.rm=T)) %>%
      gather(key=Status,value=Stations,-Day) %>%
      dplyr::mutate(Day=as.Date(as.character(Day)))-> Dat
    pl <- ggplot(data=Dat, aes(x=Day,y=Stations,fill=Status)) + geom_col() + 
      theme_bw() + scale_fill_manual(values=c("olivedrab","orangered")) #+
      #theme(axis.text.x = element_text(angle = 90, hjust = 1))
    pl
  })
  
  # exceedances: map
  output$exc_map <- renderPlot({
    dataOfPeriod() %>% dplyr::mutate(Exc=Value>input$threshold) %>% group_by(Name,Lat,Lon) %>%
      dplyr::summarize(Exceed=sum(Exc, na.rm=T),
                       Valid=sum(!is.na(Exc))) -> Dat
    MapData <- map_data("world")
    xmin<-min(Dat$Lon,na.rm=T)
    xmax<-max(Dat$Lon,na.rm=T)
    ymin<-min(Dat$Lat,na.rm=T)
    ymax<-max(Dat$Lat,na.rm=T)
    dx<-xmax-xmin
    dy<-ymax-ymin
    bb <- unique(round(quantile(Dat$Exceed,c(0,20,40,60,80,90,100)/100)))
    Dat %>% mutate(Exceedances=cut(Exceed,bb,include.lowest = T)) %>% dplyr::arrange(desc(Exceed))-> Dat
    pl <- ggplot() + 
      geom_polygon(data=MapData, aes(x=long, y=lat, group = group),
                   colour="grey80", fill="grey30" ) +
      geom_point(data=Dat, aes(x=Lon, y=Lat, color=Exceedances, size=Valid), shape=19) +
      scale_colour_brewer(palette = "Spectral", direction=-1) +
      scale_size_area() +
      coord_map(xlim = c(xmin-dx*0.05,xmax+dx*0.05),
                ylim = c(ymin-dy*0.05,ymax+dy*0.05))
    pl
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
      scale_fill_manual(values=alpha(c("white", "#eeeeee"), c(0,.1)), guide =FALSE) -> pl
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
    pl <- pl + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    pl
  })
  
  # clustering: map
  output$clu_map <- renderPlot({
    dataWithClusters() %>% dplyr::select(Name,Source,Lat,Lon,Cluster,isMedoid) %>% distinct() -> Dat
    MapData <- map_data("world")
    xmin<-min(Dat$Lon,na.rm=T)
    xmax<-max(Dat$Lon,na.rm=T)
    ymin<-min(Dat$Lat,na.rm=T)
    ymax<-max(Dat$Lat,na.rm=T)
    dx<-xmax-xmin
    dy<-ymax-ymin
    pl <- ggplot() + 
      geom_polygon(data=MapData, aes(x=long, y=lat, group = group),
                   colour="grey80", fill="grey30" ) +
      geom_label_repel(data=Dat, aes(x=Lon, y=Lat, fill=Cluster, label=Cluster), 
                       box.padding = unit(0.1, "lines"),
                       label.padding = unit(0.1, "lines"), 
                       size=3, col="black") +
      coord_map(xlim = c(xmin-dx*0.05,xmax+dx*0.05),
                ylim = c(ymin-dy*0.05,ymax+dy*0.05))
    pl
  },height = 800
  )
  
  # breaks for the map
  Breaks <- reactive({
    bb <- c(0,25,50,75,100,300)
    bb
  })
  
  # daily map: palette
  colorpal <- reactive({
    cc <- c("steelblue","olivedrab","orange","red","purple")
    colorFactor(palette = cc, 
                domain = cut(Breaks(),Breaks(),
                             ordered_result=TRUE, include.lowest=TRUE))
  })
  
  # daily map: initialize basemap
  output$Map <- renderLeaflet({
    lon0 <- 11
    lat0 <- 42.5
    leaflet() %>% 
      setView(lon0, lat0, 6) %>% 
      #fitBounds(~min(Lon, na.rm=T), ~min(Lat, na.rm=T), ~max(Lon, na.rm=T), ~max(Lat, na.rm=T)) %>%
      addTiles()  %>% addProviderTiles("CartoDB.Positron")
  })

  # daily map: dynamic layer
  observeEvent(input$goDay,{
    Dat <- dataOfDay()
    Pal <- colorpal()
    
    leafletProxy("Map",session) %>% clearMarkers() -> map
    if (!is.null(Dat) && nrow(Dat)>0) {
      ns <- nrow(Dat)
      map %>%
        addCircleMarkers(lng=~Lon, lat=~Lat
                         ,data=Dat
                         ,radius=6 
                         ,color=~Pal(ValueInterval)
                         #,color="blue"
                         ,stroke=F, fillOpacity=1
                         ,layerId=1:ns
                         ,popup= ~htmlEscape(paste0(Name,": ",Value))
        ) -> map
    }else{
      map %>% addPopups(lng=11,lat=44,popup="No valid data!") -> map
    }
    map
  })
  
  # daily map: legend
  observe({
    Dat <- dataOfDay()
    Pal <- colorpal()
    proxy <- leafletProxy("Map", data = Dat)
    
    proxy %>% 
      clearControls() %>%
      addLegend(position = "bottomright", pal = Pal, values = ~ValueInterval)
  })
  
  # support for debug
  output$sessionInfo <- renderPrint({sessionInfo()})
  output$uiInput <- renderPrint({input})
  output$summary <- renderPrint({summary(dataOfDay())})
  
  # account info
  output$ui_account <- renderUI({
    fluidPage(
      p("You are logged in with the user ",code(input$Usr))
    )
  })
}

shinyApp(ui = ui, server = server)