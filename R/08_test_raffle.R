############### RUN RAFFLE ##################

## Load helpers
source("R/01_helper_functions.R")

## Test Raffle
DA_plateau <- DA[lengths(st_intersects(DA, plateau))>0,]

  tm_shape(DA_plateau) +
    tm_borders(col="black")+
    tm_shape(plateau)+
    tm_borders(col="red")+
    tm_shape(st_laurent_buff)+
    tm_fill(col="green", alpha = 0.4)+
    tm_shape(st_denis_buff)+
    tm_fill(col="green", alpha = 0.4)

DA_plateau$GeoUID <- as.numeric(DA_plateau$GeoUID)
  
outliers <- anti_join(st_drop_geometry(property), 
                      DA_plateau, 
                      by = c("winner" = "GeoUID")) %>% 
            left_join(property)    

tm_shape(DA_plateau) +
  tm_borders() + 
  tm_shape(outliers$geometry)+
  tm_dots()

# Percentage of all listings that were removed using the raffle
nrow(outliers)/nrow(property)

# Percentage of listings within the 200m buffer but not within the plateau boundaries
  # that were removed using the raffle
nrow(outliers)/nrow(anti_join(st_drop_geometry(property), 
                  st_join(property, plateau, join = st_within, left = FALSE), 
                  by = "Property_ID"))

# Check if removing any with permits, and if so, that they are located outside of the plateau
left_join(filter(outliers, Permit == TRUE), 
          st_drop_geometry(plateau_address)) %>% 
          select(1:2, 5, 20)



