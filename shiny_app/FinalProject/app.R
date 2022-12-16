library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)

library(shiny)
library(leaflet)
library(maps)


# Load data
load("co2_agri_clean.RData")

# Define UI
ui <-
  fluidPage(
    titlePanel("CO2 emission and population and GDP"),
    
    # Create a new Row in the UI for select Inputs
    fluidRow(
      column(4,
             selectInput("country",
                         "Country:",
                         c("All",
                           unique(as.character(co2_agri_clean$country))))
      ),
      column(4,
             selectInput("year",
                         "Year:",
                         c("All",
                           unique(as.character(co2_agri_clean$year))))
      ),
      
      # Create a new row for the table.
      DT::dataTableOutput("table")
    ))

# Define server function
server <- 
  function(input, output) {
    
    # Filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
      data <- co2_agri_clean
      if (input$country != "All") {
        data <- data[data$country == input$country,]
      }
      if (input$year != "All") {
        data <- data[data$year == input$year,]
      }
      
      data
    }))
    
  }

# Create Shiny object
shinyApp(ui = ui, server = server)