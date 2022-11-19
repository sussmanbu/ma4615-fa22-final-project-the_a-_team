library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)


# Load data
load("Agri_air_car.RData")

# Define UI
ui <-
  fluidPage(
    titlePanel("Agriculture & Airline & Vehicle"),
    
    # Create a new Row in the UI for select Inputs
    fluidRow(
      column(4,
             selectInput("source_of_emissions",
                         "Source of Emissions:",
                         c("All",
                           unique(as.character(Agri_air_car$source_of_emissions))))
      ),
      column(4,
             selectInput("year",
                         "Year:",
                         c("All",
                           unique(as.character(Agri_air_car$year))))
      ),
      column(4,
             selectInput("type_of_cars",
                         "Type of Cars:",
                         c("All",
                           unique(as.character(Agri_air_car$type_of_cars)))))
    ),
    # Create a new row for the table.
    DT::dataTableOutput("table")
  )

# Define server function
server <- 
  function(input, output) {
    
    # Filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
      data <- Agri_air_car
      if (input$source_of_emissions != "All") {
        data <- data[data$source_of_emissions == input$source_of_emissions,]
      }
      if (input$year != "All") {
        data <- data[data$year == input$year,]
      }
      if (input$type_of_cars != "All") {
        data <- data[data$type_of_cars == input$type_of_cars,]
      }
      data
    }))
    
  }

# Create Shiny object
shinyApp(ui = ui, server = server)