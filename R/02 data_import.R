############### DATA IMPORT ##################

source("R/01 helper_functions.R")

## import plateau dataset

montreal <- read_sf(dsn = ".", layer = "plateau") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(3347)
View(plateau)

plateau_buff <-
  montreal %>%
  filter (CODEID=="5") %>%
  summarize(geometry = st_union(geometry)) %>% 
  st_buffer(200)

plot (plateau_buff ["geometry"])

## import airbnb listings from 2018

property_file <- read_csv("Data/Montreal_property.csv")




