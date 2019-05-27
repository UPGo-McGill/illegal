##################### MAPPING ##########################################

source("R/01_helper_functions.R")
source("R/04_osm.R")

palette <- c('#313695','#fee090','#d73027','#72001a')

## Exact location of permitted listings
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
  tm_layout(legend.position = c("left", "bottom"),
            frame = FALSE) +
  tm_compass()

## St-Denis/St-Laurent buffers and listings within buffers
tm_shape(st_buffer(plateau, 200)) +
  tm_borders(lwd = 1) + 
  tm_shape(plateau_streets)+
  tm_lines(col="grey", alpha = 0.5)+
  tm_shape(candidate_streets)+
  tm_lines(col = "grey", alpha = 0.5) +
  tm_shape(plateau) +
  tm_borders(lwd = 2) +
  tm_shape(st_laurent_buff[])+
  tm_fill(col="darkolivegreen3", alpha=.3)+
  tm_shape(st_denis_buff[])+
  tm_fill(col="darkolivegreen3", alpha = .3)+
  tm_shape(st_denis_prop[])+
  tm_dots(size = 0.01, col="black")+
  tm_shape(st_laurent_prop[])+
  tm_dots(size = 0.01, col = "black")+
  #tm_shape(filter(property, Permit == TRUE))+
  #tm_dots(size = 0.05, col="blue")
  tm_compass()

## St-L and St-D, will require permits: All entire homes on St Denis, St Laurent?
tm_shape(st_l_d)+
  tm_fill(col="grey", alpha = .3)+
  tm_shape(filter(st_denis_prop, Listing_Type=="Entire home/apt"))+
  tm_dots(size = 0.01, col="blue")+
#  tm_shape(filter(st_denis_prop, Listing_Type=="Private room"))+
#  tm_dots(size = 0.01, col="green")+
  tm_shape(filter(st_laurent_prop, Listing_Type=="Entire home/apt"))+
  tm_dots(size = 0.01, col="blue")+
#  tm_shape(filter(st_laurent_prop, Listing_Type=="Private room"))+
#  tm_dots(size = 0.01, col="green")+
  tm_shape(plateau_streets)+
  tm_lines(col="grey")+
  tm_compass()


## Listings which will become illegal: All entire homes?
tm_shape(st_buffer(plateau,200))+
  tm_borders("black")+
  #  tm_shape(plateau_streets)+
  #  tm_lines(col="grey")+
  tm_shape(filter(property,  Listing_Type == "Entire home/apt"&
                    Permit == FALSE))+
  tm_dots(size = 0.05, col="blue")


# All airbnbs within the plateau colour coded by listing type
tm_shape(st_buffer(plateau, 200)) +
  tm_borders(lwd = 1) + 
  tm_shape(plateau_streets)+
  tm_lines(col="grey", alpha = 0.5)+
  tm_shape(candidate_streets)+
  tm_lines(col = "grey", alpha = 0.5) +
  tm_shape(plateau) +
  tm_borders(lwd = 2) +
  tm_shape(property)+
  tm_dots(col = "Listing_Type", palette = palette, title = "Listing Type") +
  tm_layout(legend.position = c("left", "bottom"),
            frame = FALSE) +
  tm_compass()

# All listings under current legislation colour coded by legality
tm_shape(st_buffer(plateau, 200)) +
  tm_borders(lwd = 1) + 
  tm_shape(plateau_streets)+
  tm_lines(col="grey", alpha = 0.5)+
  tm_shape(candidate_streets)+
  tm_lines(col = "grey", alpha = 0.5) +
  tm_shape(plateau) +
  tm_borders(lwd = 2) +
  tm_shape(property)+
  tm_dots(col = "Legal", palette =  c("#72001a", "#313695")) +
  tm_layout(legend.position = c("left", "bottom"),
            frame = FALSE) +
  tm_compass()

# All illegal listings under current legislation
tm_shape(st_buffer(plateau, 200)) +
  tm_borders(lwd = 1) + 
  tm_shape(plateau_streets)+
  tm_lines(col="grey", alpha = 0.5)+
  tm_shape(candidate_streets)+
  tm_lines(col = "grey", alpha = 0.5) +
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
  tm_borders(lwd = 1) + 
  tm_shape(plateau_streets)+
  tm_lines(col="grey", alpha = 0.5)+
  tm_shape(candidate_streets)+
  tm_lines(col = "grey", alpha = 0.5) +
  tm_shape(plateau) +
  tm_borders(lwd = 2) +
  tm_shape(filter(property, FREH == TRUE))+
  tm_dots(col = "Legal", palette = c("#72001a", "blue")) +
  tm_layout(legend.position = c("left", "bottom"),
            frame = FALSE) +
  tm_compass()


