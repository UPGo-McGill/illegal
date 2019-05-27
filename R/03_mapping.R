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

# All listings under current legislation colour coded by legality
tm_shape(st_buffer(plateau, 200)) +
  tm_borders(lwd = 1, alpha = 0.5) + 
  tm_shape(plateau) +
  tm_borders(lwd = 2) +
  tm_shape(property)+
  tm_dots(col = "Legal", palette = palette) +
  tm_layout(legend.position = c("left", "bottom"),
            frame = FALSE) +
  tm_compass()

# All legal listings under current legislation
tm_shape(st_buffer(plateau, 200)) +
  tm_borders(lwd = 1, alpha = 0.5) + 
  tm_shape(plateau) +
  tm_borders(lwd = 2) +
  tm_shape(filter(property, Legal == TRUE))+
  tm_dots(col = "green") +
  tm_layout(legend.position = c("left", "bottom"),
            frame = FALSE) +
  tm_compass()

# All illegal listings under current legislation
tm_shape(st_buffer(plateau, 200)) +
  tm_borders(lwd = 1, alpha = 0.5) + 
  tm_shape(plateau) +
  tm_borders(lwd = 2) +
  tm_shape(filter(property, Legal == FALSE))+
  tm_dots(col = "#72001a") +
  tm_layout(legend.position = c("left", "bottom"),
            frame = FALSE) +
  tm_compass()

# Housing loss
# Illegal FREH + ghost hotels + legal FREH
tm_shape(st_buffer(plateau, 200)) +
  tm_borders(lwd = 1, alpha = 0.5) + 
  tm_shape(plateau) +
  tm_borders(lwd = 2) +
  tm_shape(filter(property, FREH == TRUE))+
  tm_dots(col = "Legal", palette = c("#72001a", "blue")) +
  tm_layout(legend.position = c("left", "bottom"),
            frame = FALSE) +
  tm_compass()


# analysis - leaving this here until we finalize numbers
property %>%   
sum(FREH)

sum(property$FREH)

property %>% 
  filter(FREH == TRUE & Legal == FALSE)
