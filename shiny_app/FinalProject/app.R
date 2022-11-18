
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(tidyverse)

load("Agri_air.Rdata")
ui <- fluidPage(
  
  # Application title
  titlePanel("Agriculture & Airline"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "year",
        "Select groups",
        unique(Agri_air$year),
        selected = unique(Agri_air$year)[1]
      )
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("barPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$barPlot <- renderPlot({
    # show data for  input$group from ui.R
    
    Agri_air %>%
      filter(year %in% input$year) %>%
      filter(source_of_emissions == "Emissions on agricultural land" | source_of_emissions == "Farm-gate emissions" | source_of_emissions == "IPCC Agriculture" | 
               source_of_emissions == "AFOLU" | source_of_emissions == "Enteric Fermentation" | source_of_emissions == "Domestic aviation: (A)+(C)" | source_of_emissions == "International aviation (memo item): (D)+(G)") %>%
      ggplot(aes(x = source_of_emissions, y = Emission, fill = year)) +
      geom_col(position = "dodge") + theme(axis.text.x = element_text(size=7, angle=45, vjust=0.8, hjust=0.8))
  })
}


# Run the application 
shinyApp(ui = ui, server = server)