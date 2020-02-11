library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(readr)
library(rgdal)
library(dashboardthemes)

## load data
source("01-scraping-food-country-co2-dataset.R")

final_table <- final_table %>%
  mutate(consumption = parse_number(consumption),
         co2_emmission = parse_number(co2_emmission))

# country name cleaning
final_table$country[final_table$country == "USA"] <- "United States"
final_table$country[final_table$country == "Congo"] <- "Dem. Rep. Congo"

##  data matching data

# add consumption & emission data to polygon data
countries <- readOGR("world-shapefiles-simple", "TM_WORLD_BORDERS-0.3")
map <-
  merge(
    countries,
    final_table,
    by.x = "NAME",
    by.y = "country",
    duplicateGeoms = TRUE
  )
map2 <- map[!is.na(map@data$food_category),]

ui <- dashboardPage( 
                    dashboardHeader(),
                    dashboardSidebar(),
                    dashboardBody(
                      
                      shinyDashboardThemes(
                        theme = "grey_light"
                      ),
                      
                      tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                      fluidRow(
                      column(
                        4,
                        radioButtons(
                          inputId = "map_var",
                          label =  "Choose variable to map :",
                          choices = c("CO2 emissions",
                                      "Food Consumption"),
                          inline = TRUE,
                          selected = "CO2 emissions"
                        )
                      ),
                      column(
                        3,
                        selectInput(
                          inputId = "food",
                          label = "Choose Food Type",
                          choices = unique(map2$food_category)
                        )
                      )
                    ),
                    leafletOutput("map")))


server <- server <- function(input, output, session) {
  
  # output$filtered_map <- reactive({
  #   req(input$food)
  #   subset(map2, food_category == input$food)
  # })
  # 
  # output$data_to_map <- reactive({
  #   req(input$map_var)
  # 
  #   switch(input$map_var,
  #          "CO2 emissions" = filtered_map()$co2_emmission,
  #          "Food Consumption" = filtered_map()$consumption)
  # })
  
  output$map <- renderLeaflet({
    req(input$food)
    req(input$map_var)
 
    filtered_map <- subset(map2, food_category == input$food)
    
    # create over label
    filtered_map$labels <- paste0(
      "<strong> Country: </strong> ",
      filtered_map$NAME,
      "<br/> ",
      "<strong> Food type consumption in kg/per capita: </strong> ",
      filtered_map$consumption,
      "<br/> ",
      "<strong> Co2 emissions in ton/per capita: </strong> ",
      filtered_map$co2_emmission,
      "<br/> "
    ) %>%
      lapply(htmltools::HTML)
    
    
    
    pal <- colorBin(palette = "viridis",
                    domain = filtered_map$co2_emmission,
                    bins = 7)
    
    
    leaflet(filtered_map) %>%
      addTiles() %>%
      setView(lng = 0,
              lat = 30,
              zoom = 2) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~ pal(filtered_map$co2_emmission),
        color = "white",
        fillOpacity = 0.7,
        label = ~ labels,
        highlight = highlightOptions(color = "black",
                                     bringToFront = TRUE)
      ) %>%
      leaflet::addLegend(
        pal = pal,
        values = ~ filtered_map$co2_emmission,
        opacity = 0.7,
        title = paste0(input$map_var)
      )
    
  })
  
  
  
}

shinyApp(ui, server)
