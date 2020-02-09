library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
library(lubridate)
library(leaflet)
library(rnaturalearth)
library(ggplot2)
library(rgdal)
library(sp)
library(maps)
library(maptools)
### raw emissions ####

raw_emissions <-
  read_csv("CO2 emissions per capita per country.csv")

tidy_emissions <- raw_emissions %>%
  clean_names("snake") %>%
  gather('year', 'co2_emissions',-c(country_name, country_code)) %>%
  # rename()
  mutate(year = as.numeric(str_replace(year, 'x', '')))


glimpse(tidy_emissions)



### global land temperature ####

raw_land_temp <-
  read_csv("GlobalLandTemperatures_GlobalLandTemperaturesByCountry.csv")
colnames(raw_land_temp)

tidy_land_temp <- raw_land_temp %>%
  clean_names("snake") %>%
  rename(date = dt, country_name = country) %>%
  mutate(year = year(date))

# TODO: inspect missingness after 1960s

yearly_averages <- tidy_land_temp %>%
  group_by(year, country_name) %>%
  summarise(yearly_average_temperature = mean(average_temperature, na.rm = TRUE))

glimpse(yearly_averages)

poland <- filter(yearly_averages, country_name == 'Poland') %>%
  na.omit()

ggplot(poland, aes(year, yearly_average_temperature)) +
  geom_line() +
  geom_smooth()

## data vis of temp by country over time




### combining the datasets ####


joined_df <- left_join(yearly_averages, tidy_emissions) %>%
  na.omit()

#### spatial data vis ####
### map 1 ####
leaflet(options = leafletOptions(minZoom = 2)) %>%
  addMiniMap()


### map 2 ####
mymap <- leaflet() %>%
  addTiles() %>%
  setView(lng = -0.127949,
          lat = 51.507752,
          zoom = 5) %>%
  addProviderTiles("NASAGIBS.ViirsEarthAtNight2012")

mymap

### map 3 ####
world <- maps::map("world", fill = TRUE, plot = FALSE)
world_map <- map2SpatialPolygons(world, sub(":.*$", "", world$names))
world_map <- SpatialPolygonsDataFrame(world_map,
                                      data.frame(country = names(world_map),
                                                 stringsAsFactors = FALSE),
                                      FALSE)

cnt <-
  c(
    "Russia",
    "Afghanistan",
    "Albania",
    "Algeria",
    "Argentina",
    "Armenia",
    "Azerbaijan",
    "Bangladesh",
    "Belarus"
  )

target <- subset(world_map, country %in% cnt)

leaflet(world_map) %>%
  addTiles() %>%
  addPolygons(weight = 1)


### map4 ####

map <- rnaturalearth::ne_countries()

map$continent
map$name
map$abbrev
map$name_long

names(map)

names(map)[names(map) == "iso_a3"] <- "country_code"
names(map)[names(map) == "name"] <- "country_name"


map$year <-
  joined_df[match(map$country_code, joined_df$country_code), "year"]
map$yearly_average_temperature <-
  joined_df[match(map$country_code, joined_df$country_code), "yearly_average_temperature"]
map$co2_emissions <-
  joined_df[match(map$country_code, joined_df$country_code), "co2_emissions"]
names(map)

map$labels <- paste0(
  "<strong> Country: </strong> ",
  map$country_name, "<br/> ",
  "<strong> CO2 emissions: </strong> ",
  pull(map$co2_emissions), "<br/> "
) %>%
  lapply(htmltools::HTML)

pal <- colorBin(
  palette = "inferno", domain = pull(map$co2_emissions),
  # bins = seq(0, max(map$co2_emissions, na.rm = TRUE), by = 5)
  bins = 10)

leaflet(map) %>%
  addTiles() %>%
  setView(lng = 0, lat = 30, zoom = 2) %>%
  addPolygons(
    fillColor = ~ pal(pull(map$co2_emissions)),
    color = "white",
    fillOpacity = 0.7,
    label = ~ labels,
    highlight = highlightOptions(color = "black",
                                 bringToFront = TRUE)
  ) %>%
  leaflet::addLegend(
    pal = pal,
    values = ~ pull(map$co2_emissions),
    opacity = 0.7,
    title = "co2_emissions"
  )
