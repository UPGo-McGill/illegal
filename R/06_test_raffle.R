############### RUN RAFFLE ##################

## Load helpers
source("R/01_helper_functions.R")

## Test Raffle
DA_plateau <- DA[lengths(st_intersects(DA, plateau))>0,]

DA_plateau$GeoUID <- as.numeric(DA_plateau$GeoUID)
  
outliers <- anti_join(st_drop_geometry(property), 
                      DA_plateau, 
                      by = c("winner" = "GeoUID")) %>% 
            left_join(property)    

# Percentage of all listings that were removed using the raffle
nrow(outliers)/nrow(property)

# Percentage of listings within the 200m buffer but not within the plateau boundaries
  # that were removed using the raffle
nrow(outliers)/nrow(anti_join(st_drop_geometry(property), 
                  st_join(property, plateau, join = st_within, left = FALSE), 
                  by = "Property_ID"))

## Random Subset
sample_st_l <- st_laurent_prop[c(254,388,1323,453,830,798,
                                 1208,1324,51,1129,928,981,
                                 1270,1096,344,117,141,150,
                                 1097,1100),]

sampl_st_d <- st_denis_prop[c(704,825,776,525,410,1084,1134,
                              1046,515,747,1123,905,171,1008,
                              1056,522,852,874,179,901),]          

# Check if removing any with permits, and if so, that they are located outside of the plateau
left_join(filter(outliers, Permit == TRUE), 
          st_drop_geometry(plateau_address)) %>% 
          select(1:2, 5, 20)
