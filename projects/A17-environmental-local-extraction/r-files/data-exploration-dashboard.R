# R Shiny EFH Data Exploration Dashboard

#' Allows user to import an RDS file of EFH observations with environmental data and to visualize 
#' the data distributions spatially and by variable.
#' 
#' Has some issues - need to use RDS in current format and data must already be consolidated into a 
#' data frame (even though it says Excel File). 

library(bslib)
library(readxl)
library(shiny)
library(tidyverse)

options(shiny.maxRequestSize = 500 * 1024^2)

ui <- page_sidebar(
  title = "Amendment 17 EFH Data Visualization",
  sidebar = sidebar(
    fileInput("xldata", "Select Excel File:", accept = c(".xlsx")),
    sliderInput("maps-area-x", "Map Region (LON)", value = c(-100, -20), min = -100, max = -20, step = 5),
    sliderInput("data-area-x", "Data Region (LON)", value = c(-100, -20), min = -100, max = -20, step = 5),
    sliderInput("maps-area-y", "Map Region (LAT)", value = c(0, 55), min = 0, max = 55, step = 5),
    sliderInput("data-area-y", "Data Region (LAT)", value = c(0, 55), min = 0, max = 55, step = 5)
  ),
  
  navset_card_underline(
    nav_panel("Map", plotOutput("mapplot")),
    nav_panel("Dat", plotOutput("histgrm"))
  )
  
)

server <- function(input, output, session) {
  loaddata <- reactive({
    print(input$xldata$datapath)
    df <- readRDS(input$xldata$datapath)
    print(df)
    return(df)
  })
  
  output$mapplot <- renderPlot({
    data <- loaddata()
    ggplot() + annotation_map(map_data("world")) +
      coord_fixed(xlim = input$`maps-area-x`, ylim = input$`maps-area-y`) +
      geom_point(data, mapping = aes(LON,LAT)) +
      geom_rect(mapping = aes(xmin = input$`data-area-x`[1], xmax = input$`data-area-x`[2],
                              ymin = input$`data-area-y`[1], ymax = input$`data-area-y`[2]),
                col = "red", fill = NA, lty = "dashed") +
      theme_bw()
  })
  
  output$histgrm <- renderPlot({
    data <- loaddata()
    data %>% 
      rename(SALINITY = SSS, `BOTTOM TEMPERATURE` = BT, `BOTTOM SALINITY` = BS,
             `MIXED LAYER DEPTH` = MLD, `SEA SURFACE HEIGHT` = ZOS, TEMPERATURE = THETAO,
             TURBIDITY = TURB, `CHLOROPHYLL ALPHA` = CHLA) %>%
      pivot_longer(cols = BATHYMETRY:`CHLOROPHYLL ALPHA`, names_to = "Variable", values_to = "Value") %>%
      ggplot(aes(Value)) +
      geom_histogram() +
      facet_wrap(~Variable, scales = "free") +
      theme_bw()
  })
}

shinyApp(ui, server)