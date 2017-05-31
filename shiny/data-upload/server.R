read.n.save <- function(filein, header, sep, name, pwd) {
  if(pwd==Password) {
    data <- read.csv(filein, header=header, sep=sep)
    filecsv <- paste("./data/",name,sep="")
    write.table(data,file=filecsv,row.names=FALSE,sep=",",col.names=T) 
    return(data)
  }
}

shinyServer(function(input, output) {
  output$contents <- renderTable({
    
    inFile <- input$file1    
 
    if (is.null(inFile) | input$upload==0) {
      return(NULL)
    } else {
      read.n.save(inFile$datapath,
                  header=TRUE,
                  sep=input$sep,
                  name=paste(input$institution,".",input$pollutant,".",
                             format(input$date,format="%Y%m%d"),
                             ".dat",sep=""),
                  pwd=input$password)      
    }

  })
})
