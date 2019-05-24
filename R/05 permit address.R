plateau_permit <- read_csv("Data/plateau_legal.csv") 
plateau_address <- read_csv("Data/Addresses.csv")
names (plateau_address) <- c("ETBL_ID", "Add_ID", "Add_Type", "Add_FR", "Add_EN", 
                "Add_Princ", "Address", "Municipality", "Province",
                "Country", "Postal_Code", "Latitude", "Longitude")
plateau_address <- plateau_address[,c(1,12,13)]
plateau_address <- st_as_sf(plateau_address, coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(26918)



new <- st_join(permit, plateau_address)
