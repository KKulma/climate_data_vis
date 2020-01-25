library(rvest)
library(purrr)
library(dplyr)
library(janitor)
library(tidyr)
library(stringr)


url <- "https://www.nu3.de/blogs/nutrition/food-carbon-footprint-index-2018"

url_html <- read_html(url) 

# tbody <- url_html %>% html_node('tbody') 

whole_table <- url_html %>%  
  html_nodes('table') %>% 
  html_table(fill = TRUE) %>% 
  .[[1]]

glimpse(whole_table)
# first_row <- whole_table %>% slice(1) %>% unlist(use.names = FALSE)

# tidy_header1 <- c("Country", first_row[3:length(first_row)])

# tidying the table
table_content <- whole_table %>% 
  select(-X1) %>% 
  filter(!dplyr::row_number() %in% 1:3)
glimpse(table_content)

dim(table_content)

raw_header3 <- url_html %>% 
  html_nodes(".thead-icon") %>% 
  html_attr('title') 

raw_header3

tidy_header3 <- raw_header3[28:length(raw_header3)]
tidy_header3
# colnames(table_content) <- c("Country", first_view[1, 2:length(first_view)])
# filter(first_view, dplyr::row_number() != 1) 

raw_header2 <- raw_header3[17:27]
raw_header2

tidy_header2 <- c(
  rep(raw_header2[1:7], each = 2),
  "animal_total",
  rep(raw_header2[8:length(raw_header2)], each = 2),
  "non_animal_total",
  "country_total")

tidy_header2

combined_colnames <- paste(tidy_header2, tidy_header3, sep = ';')
colnames(table_content) <- c("Country", combined_colnames)
glimpse(table_content)


long_table <- table_content %>% 
  tidyr::pivot_longer(cols = -Country, names_to = "Category", values_to = "Values") %>% 
    separate(col = Category, into = c("Food Category", "Metric"), sep = ';') %>% 
    janitor::clean_names('snake')

long_table

tidy_table <- long_table %>%
  tidyr::pivot_wider(names_from = metric, values_from = values) %>% 
  janitor::clean_names('snake')

final_table <- tidy_table %>% 
  rename(consumption = 3,
          co2_emmission = 4) %>% 
  filter(!stringr::str_detect(food_category, "total"))

final_table
