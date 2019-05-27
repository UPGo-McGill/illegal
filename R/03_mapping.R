##### MAPPING ################

source("R/01_helper_functions.R")

palette <- c('#313695','#fee090','#d73027','#72001a')


# All airbnbs within the plateau colour coded by listing type
tm_shape(st_buffer(plateau, 200)) +
  tm_borders(lwd = 1, alpha = 0.5) + 
  tm_shape(plateau) +
  tm_borders(lwd = 2) +
  tm_shape(property)+
  tm_dots(col = "Listing_Type", palette = palette) +
  tm_layout(legend.position = c("left", "bottom"),
            frame = FALSE) +
  tm_compass()

# All illegal listings under current legislation