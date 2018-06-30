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
                    "CHILDREN" = "ped",
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
    # data wrangling code to get to file 3
    
    # read in task report, clean names and select
    tr <-
      file1 %>%
      clean_names() %>%
      select(
        task_list,
        task_name,
        na,
        owner_type,
        owner_name,
        target_date,
        days_overdue,
        completed_date,
        protocol_no,
        institution,
        mgmt_group
      )
    
    # clean names and select columns for protocol search
    ps <- clean_names(file2) %>%
      select(protocol_no:additional_sponsor)
    
    # clean up who is owner
    tr_owner <- tr %>% select(protocol_no, owner_name, task_name) %>%
      filter(task_name == 'Created CTRF & \"Notify ORSP\"') %>%
      select(-task_name)
    
    # add owner to ps
    ps <- left_join(ps, tr_owner)
    
    # fix dates for tasks that are not applicable to study
    # give them an artificial date in year 2200
    tr$completed_date[tr$na == "Y"] <- as.Date("2200-01-01")
    
    #select for only Pre-Award tasks
    tr <- tr %>%
      filter(str_detect(task_list, "Pre-"))
    
    # now make columns = tasks
    # using spread
    tr_spread <- tr %>%
      select(protocol_no, task_name, completed_date) %>%
      filter(!is.na(completed_date)) %>%
      unique() %>%
      spread(task_name, completed_date)
    
    # fix one colname
    colnames(tr_spread)[6] <- "Created CTRF & Notified ORSP"
    
    #merge ps and tr, put names in order
    pstr <- left_join(ps, tr_spread, by = "protocol_no")
    pstr <- pstr %>%
      arrange(`UFA Completed`) %>%
      mutate_if(is.POSIXct, as.Date) %>%
      mutate_if(is.Date, funs(format(., format = "%m/%d/%Y"))) %>% 
      select(
        protocol_no,
        additional_protocol_numbers,
        department,
        pi_name,
        principal_sponsor,
        current_status,
        current_status_date,
        owner_name,
        title,
        `Intake Form Completed`,
        `UFA Completed`,
        `Sponsor Reach out`,
        `Feasibility w/Documents received`,
        `Feasibility approved/CRAO Notified`,
        `Planning Meeting Request Sent`,
        `Planning Meeting Completed`,
        `Created CTRF & Notified ORSP`,
        `Billing Calendar Received`,
        `Care Designations completed`,
        `Internal Budget Finished`,
        `PI approved Budget`,
        `Budget Negotiations`,
        `Final Budget`,
        `PAF routed`,
        `IRB Application Submitted`,
        `OnCore Budget Build`,
        `Calendar Released`,
        `PAN Released`,
        `Open to Accrual`
      )
    
    file3 <- pstr
    
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

