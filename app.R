#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(writexl)
library(ggplot2)
library(tidyverse)
library(magrittr)
library(tidyr)
library(purrr)
library(stringr)
library(here)
library(janitor)
library(googlesheets)
library(lubridate)


# Define UI for data upload app ----
ui <- fluidPage(

  #makes horizontal rule lines thicker
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  
  # App title ----
  titlePanel("Converting CTSU Files to Summary Reports"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input select CTSU
      selectInput("ctsu", "CTSU:",
                  c("ACD" = "acd",
                    "HVB" = "hbv",
                    "BFP" = "bfp",
                    "ACCST" = "accst",
                    "PED" = "ped",
                    "NSS" = "nss",
                    "ONC" = "onc")),
      tableOutput("data"),
      
      # Horizontal line ----
      tags$hr(), 
      
      # Input select Report Type
      selectInput("report_type", "Report:",
                  c("Pre-Award" = "pre",
                    "Post-Award" = "post",
                    "Amendments" = "amend")),
      tableOutput("data2"),

      # Horizontal line ----
      tags$hr(),
      
      # Input: Select a file ----
      fileInput("file1", 
                "Choose Task Report File",
                multiple = FALSE,
                placeholder = "e.g. data/task_report.csv",
                accept = c(".csv",
                           ".xls",
                           ".xlsx")),
      
      # Horizontal line ----
      tags$hr(), 
      
      # Input: Select a file ----
      fileInput("file2", 
                "Choose Protocol Search File",
                multiple = FALSE,
                placeholder = "e.g. data/protocol_search.csv",
                accept = c(".csv",
                           ".xls",
                           ".xlsx")),
 
      
      # Download button ----
      # downloadButton("downloadData", "Download Weekly CTSU Report")
      uiOutput("downloadButton")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      tableOutput("file3") # not actually showing file 3
      # would like to show first 5 lines of protocol no, PI, title
    )
    
  )
)


# Define server logic required 
server <- function(input, output) {

  # output$summary <- reactive({
  #   req(output$file3)
  #   renderText({
  #     head(file3, N= 5L)
  #   })
  # })
  
  outputData <- reactive({
    req(input$file1, input$file2)
    
    file1 <- read_excel(input$file1$datapath, skip = 3)
    file2 <- read_excel(input$file2$datapath, skip = 2)
    # process code to get to file 3
    file3 <- left_join(file1, file2, by = c("Protocol No" = "Protocol No."))
    file3 # return
  })
  
  output$file3 <- renderTable({
    head(outputData(), n= 5L)
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
      paste("Weekly_", input$data, "_CTSU_Report_Current", ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(outputData(), file)
    })
}
  

# Run the application 
shinyApp(ui = ui, server = server)

