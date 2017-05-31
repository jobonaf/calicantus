shinyUI(pageWithSidebar(
  headerPanel("calicantus: data upload"),
  sidebarPanel(
    tags$body(
      h5('1) Select the local ',
        code('csv'),
        ' file that you want to upload.')
    ),
    fileInput('file1', '',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    tags$hr(),
    tags$body(
      h5('2) Specify some details about the file.')
    ),
    selectInput(inputId='sep', label='Separator',
                 choices=list(Comma=',',
                   Semicolon=';',
                   Tab='\t',
                   Space=' '),
                 selected=','),
    selectInput(inputId='institution',
                label='Short code for the institution and/or project',
                choices=list("ARPA-Liguria",
                             "test")),
    selectInput(inputId='pollutant',
                label='Pollutant',
                choices=list("PM10 daily mean"="PM10",
                             "PM2.5 daily mean"="PM2.5",
                             "O3 daily maximum"="O3",
                             "NO2 daily maximum"="NO2")),
    dateInput(inputId="date",
              label="Date of sampling",
              value=Sys.Date()-1,
              max=Sys.Date()-1),
    tags$hr(),
    tags$body(
      h5('3) Enter the password and click to upload.')
    ),
    textInput(inputId="password",
              label="Password",
              value=""),
    
    actionButton("upload", "Upload"),
    tags$body(
      h5('Finally, on the right side of this page you should see the data.')
    )
  ),
  
  mainPanel(
    tableOutput('contents')
  )
))

