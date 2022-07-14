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

factors <- c("Minority", "Poverty", "Zero Car", "Age Under 18", "Age over 65", "Limited English")


ui <- fluidPage(
  tabsetPanel(
    tabPanel("Additional Factors Map", fluid = TRUE,
      sidebarLayout(
        mainPanel(
             tags$style(type = "text/css", "#map2 {height: calc(100vh - 80px) !important;}"),
             leafletOutput("map2")
        ),
        sidebarPanel(style = "position: fixed; height: 90vh; width: 30%; overflow-y: auto;",
             checkboxInput(inputId = "Minority", label = strong("Minority"), value = FALSE),
             tags$head(tags$style(HTML("label {font-weight:normal;}"))),   
                conditionalPanel(condition = "input.Minority == true",
                                 tags$h5("Regional Mean: ", strong("24%")),
                                 tags$h5("Recommended Percentage", em("(1 SD from Mean): "), strong("42%")),
                                 sliderInput("slider1", label = h6("Select Minimum Percentage:"), min = 0, 
                                             max = 100, value = 42),
                                 numericInput("weight1", h6("Select Weight:"), 1, min = 1, max = 5, step = .25),
                                 verbatimTextOutput("weight1"),
                                 checkboxInput(inputId = "histo1", label = h5("Show Density Plot"), value = FALSE),
                                      conditionalPanel(condition = "input.histo1 == true", plotOutput(outputId="histo1graph"))
                                 
                ),
             checkboxInput(inputId = "Poverty", label = strong("Poverty"), value = FALSE),
             tags$head(tags$style(HTML("label {font-weight:normal;}"))),   
             conditionalPanel(condition = "input.Poverty == true",
                              tags$h5("Regional Mean: ", strong("9%")),
                              tags$h5("Recommended Percentage", em("(1 SD from Mean): "), strong("21%")),
                              sliderInput("slider2", label = h6("Select Minimum Percentage:"), min = 0, 
                                          max = 100, value = 21),
                              numericInput("weight2", h6("Select Weight:"), 1, min = 1, max = 5, step = .25),
                              verbatimTextOutput("weight2"),
                              checkboxInput(inputId = "histo2", label = h5("Show Density Plot"), value = FALSE),
                              conditionalPanel(condition = "input.histo2 == true", plotOutput(outputId="histo2graph"))
                              
             ),
             checkboxInput(inputId = "ZeroCar", label = strong("Zero Car"), value = FALSE),
             tags$head(tags$style(HTML("label {font-weight:normal;}"))),   
             conditionalPanel(condition = "input.ZeroCar == true",
                              tags$h5("Regional Mean: ", strong("4%")),
                              tags$h5("Recommended Percentage", em("(1 SD from Mean): "), strong("11%")),
                              sliderInput("slider3", label = h6("Select Minimum Percentage:"), min = 0, 
                                          max = 100, value = 11),
                              numericInput("weight3", h6("Select Weight:"), 1, min = 1, max = 5, step = .25),
                              verbatimTextOutput("weight3"),
                              checkboxInput(inputId = "histo3", label = h5("Show Density Plot"), value = FALSE),
                              conditionalPanel(condition = "input.histo3 == true", plotOutput(outputId="histo3graph"))
                              
             ),
             checkboxInput(inputId = "AgeUnder18", label = strong("Age Under 18"), value = FALSE),
             tags$head(tags$style(HTML("label {font-weight:normal;}"))),   
             conditionalPanel(condition = "input.AgeUnder18 == true",
                              tags$h5("Regional Mean: ", strong("28%")),
                              tags$h5("Recommended Percentage", em("(1 SD from Mean): "), strong("39%")),
                              sliderInput("slider4", label = h6("Select Minimum Percentage:"), min = 0, 
                                          max = 100, value = 39),
                              numericInput("weight4", h6("Select Weight:"), 1, min = 1, max = 5, step = .25),
                              verbatimTextOutput("weight4"),
                              checkboxInput(inputId = "histo4", label = h5("Show Density Plot"), value = FALSE),
                              conditionalPanel(condition = "input.histo4 == true", plotOutput(outputId="histo4graph"))
                              
             ),
             checkboxInput(inputId = "Age65P", label = strong("Age Above 65"), value = FALSE),
             tags$head(tags$style(HTML("label {font-weight:normal;}"))),   
             conditionalPanel(condition = "input.Age65P == true",
                              tags$h5("Regional Mean: ", strong("11%")),
                              tags$h5("Recommended Percentage", em("(1 SD from Mean): "), strong("19%")),
                              sliderInput("slider5", label = h6("Select Minimum Percentage:"), min = 0, 
                                          max = 100, value = 19),
                              numericInput("weight5", h6("Select Weight:"), 1, min = 1, max = 5, step = .25),
                              verbatimTextOutput("weight5"),
                              checkboxInput(inputId = "histo5", label = h5("Show Density Plot"), value = FALSE),
                              conditionalPanel(condition = "input.histo5 == true", plotOutput(outputId="histo5graph"))
                              
             ),
             checkboxInput(inputId = "LimitedEnglish", label = strong("Limited English"), value = FALSE),
             tags$head(tags$style(HTML("label {font-weight:normal;}"))),   
             conditionalPanel(condition = "input.LimitedEnglish == true",
                              tags$h5("Regional Mean: ", strong("2%")),
                              tags$h5("Recommended Percentage", em("(1 SD from Mean): "), strong("7%")),
                              sliderInput("slider6", label = h6("Select Minimum Percentage:"), min = 0, 
                                          max = 100, value = 7),
                              numericInput("weight6", h6("Select Weight:"), 1, min = 1, max = 5, step = .25),
                              verbatimTextOutput("weight6"),
                              checkboxInput(inputId = "histo6", label = h5("Show Density Plot"), value = FALSE),
                              conditionalPanel(condition = "input.histo6 == true", plotOutput(outputId="histo6graph"))
                              
             )
        )
      )
    ),
    
    tabPanel("Comparison Map", fluid = TRUE,
             tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
             leafletOutput("map")
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
  selected_rows <- reactive({
    my_rows <- c()
    if(input$Minority){my_rows <- c(my_rows,"minority")}
    if(input$Poverty){my_rows <- c(my_rows,"poverty")}
    if(input$ZeroCar){my_rows <- c(my_rows,"zeroCar")}
    if(input$AgeUnder18){my_rows <- c(my_rows,"ageUnder18")}
    if(input$Age65P){my_rows <- c(my_rows,"age65P")}
    if(input$LimitedEnglish){my_rows <- c(my_rows,"limitedEnglish")}
    return(my_rows)
  })
  output$map2 <- renderLeaflet({
    plot_add_factors_map(selected_rows(),
                         input$slider1,input$slider2,input$slider3,input$slider4,input$slider5,input$slider6,
                         input$weight1,input$weight2,input$weight3,input$weight4,input$weight5,input$weight6)

  })
  observe({
    isolate({
      new_zoom <- 9
      if(!is.null(input$map2_zoom)) new_zoom <- input$map2_zoom
      leafletProxy('map2') %>%
        setView(lng = -111.8910, lat = 40.7608, zoom = new_zoom)
    })
  }) #https://stackoverflow.com/questions/34985889/how-to-get-the-zoom-level-from-the-leaflet-map-in-r-shiny
  
  output$histo1graph <- renderPlot({createBasicHistogram(add_factors_long,"minority")})
  output$histo2graph <- renderPlot({createBasicHistogram(add_factors_long,"poverty")})
  output$histo3graph <- renderPlot({createBasicHistogram(add_factors_long,"zeroCar")})
  output$histo4graph <- renderPlot({createBasicHistogram(add_factors_long,"ageUnder18")})
  output$histo5graph <- renderPlot({createBasicHistogram(add_factors_long,"age65P")})
  output$histo6graph <- renderPlot({createBasicHistogram(add_factors_long,"limitedEnglish")})
  
  output$map <- renderLeaflet({
    plot_comparison_map() %>% setView(lng = -111.8910, lat = 40.7608, zoom = 9)
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

