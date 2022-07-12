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
library(DT)
source("efa_analysis_2020.R")
source("efa_add_factors_2020.R")


ui <- fluidPage(
  tabsetPanel(
    tabPanel("Comparison Map", fluid = TRUE,
             tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
             leafletOutput("map1")
    ),
    tabPanel("Additional Factors Map", fluid = TRUE,
             tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
             leafletOutput("map2")
    ),
    tabPanel("Histograms", fluid = TRUE,
             tags$head(
               tags$style(type="text/css", ".dataTables_filter {display: none;    }")
              ),
             sidebarLayout(position = "left",
                           sidebarPanel("2020 Percent Distribution Histograms",
                                        checkboxGroupInput(inputId = "Variable",
                                                           label = "Select Variable:",
                                                           choices = c("Poverty","Minority","ZeroCar"))),
                           mainPanel("",
                                     column(6,plotOutput(outputId="plotgraph", width="500px",height="400px")))
               
             ),
             fluidRow(
               splitLayout(cellWidths = c("70%","30%"),plotOutput("graph"),DT::dataTableOutput("table"))
             )
    )
  )
)


server <- function(input, output) {
  output$map1 <- renderLeaflet({
    plot_comparison_map() %>% setView(lng = -111.8910, lat = 40.7608, zoom = 9)
  })
  output$map2 <- renderLeaflet({
    plot_add_factors_map() %>% setView(lng = -111.8910, lat = 40.7608, zoom = 9)
  })
  output$graph <- renderPlot({
    createHistogram(efaHisto20, input$Variable, 2020)
  })
  output$table <- renderDataTable({
    percentage_table(efa2020shpb4)
  },
  options=list(
    bLengthChange=0,  # show/hide records per page dropdown
    bFilter=0,  # global search box on/off
    bInfo=0,   # information on/off (how many records filtered, etc)
    bAutoWidth=0,  # automatic column width calculation, disable if passing column width via aoColumnDefs
    scrollX = FALSE,
    scroller = FALSE
  ))
}


shinyApp(ui = ui, server = server)

