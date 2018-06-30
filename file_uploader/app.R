#
# This is a Shiny web application to demonstrate file uploading.
#

library(shiny)

ui <- fluidPage(
  
  titlePanel("File Uploader"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput(
        inputId = "file1",
        label = "Choose file 1",
        multiple = FALSE,
        accept = "text/csv",
        placeholder = "e.g. sampledata/file1.csv"
      ),
      fileInput(
        inputId = "file2",
        label = "Choose file 2",
        multiple = FALSE,
        accept = "text/csv",
        placeholder = "e.g. sampledata/file2.csv"
      ),
      uiOutput("downloadButton")
    ),
    
    mainPanel(
      textOutput(outputId = "summary"),
      tableOutput("file3")
    )
  )
)

server <- function(input, output) {
  
  output$summary <- renderText({
    readLines("summary.txt")
  })
  
  getData <- reactive({
    req(input$file1, input$file2)
    
    file1 <- read.csv(input$file1$datapath)
    file2 <- read.csv(input$file2$datapath)
    
    file3 <- cbind(file1, file2)
    file3
  })
  
  output$file3 <- renderTable({
    getData()
  })
  
  output$downloadButton <- renderUI({
    req(input$file1, input$file2)
    downloadButton(
      outputId = "downloadData",
      label = "Download"
    )
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(getData(), file)
    })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)