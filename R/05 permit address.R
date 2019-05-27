##### ADDRESSES PERMITS

## Load helpers

source("R/01_helper_functions.R")


## Import Addresses and Locations from Permit Data

plateau_address <- read_csv("data/Addresses.csv") %>%
  select(c(1,7,12,13))%>%
  set_names(c("ETBL_ID","Address","Latitude", "Longitude"))

establishment_type <- read_csv("data/EstablishmentType.csv") %>%
  select(c(1,3,4))%>%
  set_names(c("ETBL_ID","ETBL_Code", "Establishment_Type"))
plateau_address <- inner_join(plateau_address, establishment_type)
rm(establishment_type)

plateau_address <- inner_join(st_drop_geometry(property), plateau_address) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(26918)

## Number of listings with permits on St Denis and St Laurent

plateau_address %>%
  filter(str_detect(Address, "Saint-Laurent"))

plateau_address %>%
  filter(str_detect(Address, "Saint-Denis"))%>%

## Map with locations of property with permits (red = airbnb aprox locations
## blue = permit data actual locations)

  
  tm_shape(st_buffer(plateau, 200)) +
    tm_borders(lwd = 1) + 
    tm_shape(plateau_streets)+
    tm_lines(col="grey", alpha = 0.5)+
    tm_shape(candidate_streets)+
    tm_lines(col = "grey", alpha = 0.5) +
    tm_shape(plateau) +
    tm_borders(lwd = 2) +
    tm_shape(plateau_address)+
    tm_dots(col = "#72001a") +
    tm_shape(filter(property, Permit==TRUE))+
    tm_dots(size = 0.05, col="red")+
    tm_layout(legend.position = c("left", "bottom"),
              frame = FALSE) +
    tm_compass()
