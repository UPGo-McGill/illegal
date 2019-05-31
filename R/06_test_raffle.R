############### RUN RAFFLE ##################

## Load helpers
source("R/01_helper_functions.R")

## Determine the dissemination areas within the borough boundaries
DA_plateau <- DA[lengths(st_intersects(DA, plateau))>0,]
DA_plateau$GeoUID <- as.numeric(DA_plateau$GeoUID)

# Find the listings in the property file that are outside of the boundaries
outliers <- anti_join(st_drop_geometry(property), 
                      DA_plateau, 
                      by = c("winner" = "GeoUID")) %>% 
            left_join(property)    

# Percentage of all listings that were removed using the raffle
nrow(outliers)/nrow(property)

# Percentage of listings within the 200m buffer but not within the borough boundaries
  # that were removed using the raffle
nrow(outliers)/nrow(anti_join(st_drop_geometry(property), 
                  st_join(property, plateau, join = st_within, left = FALSE), 
                  by = "Property_ID"))

# Check if removing any with permits, and if so, that they are located outside of the borough
left_join(filter(outliers, Permit == TRUE), 
          st_drop_geometry(plateau_address)) %>% 
          select(1:2, 5, 20)
