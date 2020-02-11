## load libraries 
# library(rnaturalearth)
library(leaflet)
library(dplyr)
library(readr)
library(rgdal)

## load data
source("01-scraping-food-country-co2-dataset.R")

## data cleaning
# parse characters to numerics
final_table <- final_table %>%
  mutate(consumption = parse_number(consumption),
         co2_emmission = parse_number(co2_emmission))

# country name cleaning
final_table$country[final_table$country == "USA"] <- "United States"
final_table$country[final_table$country == "Congo"] <- "Dem. Rep. Congo"

##  data matching data

# import country polygons
map <- rnaturalearth::ne_countries()
names(map)[names(map) == "name"] <- "country"

# add consumption & emission data to polygon data
countries <- readOGR("world-shapefiles-simple","TM_WORLD_BORDERS-0.3")
map <- merge(countries, final_table, by.x = "NAME", by.y = "country", duplicateGeoms = TRUE)
map2 <- map[!is.na(map@data$food_category), ]

## data vis 
food <- map2$food_category[4]
filtered_map <- subset(map2, food_category == food) 


## you can manioulate whether the scale is relative to the food item
# or fixed incl max for the whole dataset
# choose a colour palette 
pal <- colorBin(
  palette = "viridis", domain = filtered_map$co2_emmission,
  bins = 10)


# create over over label
filtered_map$labels <- paste0(
  "<strong> Country: </strong> ",
  filtered_map$NAME, "<br/> ",
  "<strong> Food type consumption in kg/per capita: </strong> ",
  filtered_map$consumption, "<br/> ",
  "<strong> Co2 emissions in ton/per capita: </strong> ",
  filtered_map$co2_emmission, "<br/> "
) %>%
  lapply(htmltools::HTML)


leaflet(filtered_map) %>%
  addTiles() %>%
  setView(lng = 0, lat = 30, zoom = 2) %>%
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
    title = "co2_emissions"
  ) 
