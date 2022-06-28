library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(knitr)
library(kableExtra)
library(magrittr)
library(leaflet)
source("efa_analysis_2020.R")


ui <- fluidPage(
  tabsetPanel(
    tabPanel("Maps", fluid = TRUE,
             tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
             leafletOutput("map")
    ),
    tabPanel("Histograms", fluid = TRUE,
             sidebarLayout(position = "left",
                           sidebarPanel("2020 Percent Distribution Histograms",
                                        checkboxGroupInput(inputId = "Variable",
                                                           label = "Select Variable:",
                                                           choices = c("Poverty","Minority","ZeroCar"))),
                           mainPanel("",
                                     column(6,plotOutput(outputId="plotgraph", width="500px",height="400px")))
               
             ),
             plotOutput("graph")
    )
  )
)


server <- function(input, output) {
  output$map <- renderLeaflet({
    plot_comparison_map() %>% setView(lng = -111.8910, lat = 40.7608, zoom = 9)
  })
  output$graph <- renderPlot({
    createHistogram(efaHisto20, input$Variable, 2020)
  })
}


shinyApp(ui = ui, server = server)

