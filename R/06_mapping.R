##################### MAPPING ##########################################

source("R/01_helper_functions.R")
source("R/04_osm.R")
source("R/05 permit address.R")

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
  tm_dots(col = "darkblue", size = 0.1, alpha = 0.6, border.col = "black", border.lwd = 0.7) +
  tm_layout(legend.position = c("left", "bottom"),
            frame = FALSE) +
  tm_compass()


## Revenue map 
tm_shape(st_buffer(plateau, 200)) +
  tm_borders(lwd = 1) + 
  tm_shape(plateau_streets)+
  tm_lines(col="grey", alpha = 0.5)+
  tm_shape(candidate_streets)+
  tm_lines(col = "grey", alpha = 0.5) +
  tm_shape(plateau) +
  tm_borders(lwd = 2) +
  tm_shape(property)+
  tm_dots(size = "revenue", col = "black", title.size = "Revenu", alpha = 0.75, scale=1.1)+
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
  tm_fill(col="lightblue", alpha=.5)+
  tm_shape(st_denis_buff[])+
  tm_fill(col="lightblue", alpha = .5)+
  tm_shape(st_denis_prop[])+
  tm_dots(size = 0.01, col="black", alpha = 0.75)+
  tm_shape(st_laurent_prop[])+
  tm_dots(size = 0.01, col = "black", alpha = 0.75)+
  #tm_shape(filter(property, Permit == TRUE))+
  #tm_dots(size = 0.05, col="blue")+ 
  tm_layout(legend.position = c("left", "bottom"),
            frame = FALSE) +
  tm_compass()

## St-L and St-D, will require permits: All entire homes on St Denis, St Laurent?
tm_shape(st_l_d)+
  tm_fill(col="grey", alpha = .3)+
  tm_shape(plateau_streets)+
  tm_lines(col="grey")+
  tm_shape(candidate_streets)+
  tm_lines(col= "grey42")+
  tm_shape(filter(st_denis_prop,Legal==FALSE))+
  tm_dots(size = 0.1, col="darkred", alpha = 0.6)+
  tm_shape(filter(st_laurent_prop, Legal==FALSE))+
  tm_dots(size = 0.1, col="darkred", alpha = 0.6)+
  tm_shape(plateau_address)+
  tm_dots(size = 0.1, col= "darkblue", alpha = 0.6)+
  tm_add_legend(type="symbol",
                col= c("darkred", "darkblue"),
                labels=c("Résidences de tourisme illégales", "Résidences avec une attestation"),
                border.lwd = NA,
                alpha = 0.6,
                title=" ") +
  tm_layout(legend.position = c("left", "bottom"),
            frame = FALSE) +
  tm_compass()


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
  tm_dots(col = "Listing_Type", scale = 4/3, palette = get_brewer_pal("-Dark2", n = 3), 
          alpha = 0.6, legend.show = FALSE, size = "revenue", 
          title.size = "Revenu", size.lim = c(0, 100000))+
tm_add_legend(type="symbol",
              col= get_brewer_pal("-Dark2", n = 3),
              labels=c("Logement entier", "Chambre privée", "Chambre partagée"),
              border.lwd = NA,
              alpha = 0.6,
              title="Type de Logement") +
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
  tm_dots(col = "#72001a", 
         size = "revenue",
         size.lim = c(0, 100000),
         scale = 4/3,
         alpha = 0.6, 
         title.size = "Revenu") +
  tm_layout(legend.position = c("left", "bottom"),
            frame = FALSE) +
  tm_compass()

# Housing loss
# FREH + ghost hotels 
tm_shape(st_buffer(plateau, 200)) +
  tm_borders(lwd = 1) + 
  tm_shape(plateau_streets)+
  tm_lines(col="grey", alpha = 0.5)+
  tm_shape(candidate_streets)+
  tm_lines(col = "grey", alpha = 0.5) +
  tm_shape(plateau) +
  tm_borders(lwd = 2) +
  tm_shape(filter(property, FREH == TRUE | GH == TRUE)) +
  tm_bubbles(
    scale = 4/3,
    style = "fixed",
    size.lim = c(0, 100000),
    size = "revenue", 
    col = "Legal", 
    alpha = 0.6,
    border.lwd = NA,
    palette = c("darkred", "darkblue"),
    title.col = "Legal",
    labels = c("Non", "Oui"),
    title.size = "Revenu")+
  tm_layout(legend.position = c("left", "bottom"),
            frame = FALSE) +
  tm_compass()

