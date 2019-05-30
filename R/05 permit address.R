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
  st_transform(32618)




