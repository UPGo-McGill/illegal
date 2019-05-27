##### ADDRESSES PERMITS

plateau_address <- read_csv("data/Addresses.csv") %>%
  select(c(1,7,12,13))%>%
  set_names(c("ETBL_ID","Address","Latitude", "Longitude"))

establishment_type <- read_csv("data/EstablishmentType.csv") %>%
  select(c(1,4))%>%
  set_names(c("ETBL_ID","Establishment_Type"))
plateau_address <- inner_join(plateau_address, establishment_type)
rm(establishment_type)

plateau_address <- inner_join(st_drop_geometry(property), plateau_address) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(26918)




tm_shape(plateau_address[])+
  tm_dots(size = 0.05, col="green")+
  tm_shape(permit[])+
  tm_dots(size = 0.05, col="red")
