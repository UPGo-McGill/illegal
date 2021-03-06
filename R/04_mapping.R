##################### MAPPING ##########################################

source("R/01_helper_functions.R")
source("R/03_osm.R")

palette <- c('#313695','#fee090','#d73027','#72001a')

basemap <- tm_shape(st_buffer(plateau, 200)) +
  tm_borders(lwd = 1) + 
  tm_shape(plateau_streets)+
  tm_lines(col="grey", alpha = 0.5)+
  tm_shape(candidate_streets)+
  tm_lines(col = "grey", alpha = 0.5) +
  tm_shape(plateau) +
  tm_borders(lwd = 2) +
  tm_layout(legend.position = c("left", "bottom"), frame = FALSE,
            fontfamily = "Futura-Medium",
            title.fontfamily = "Futura-CondensedExtraBold")


# Figure 1. All listings within the Plateau, colour-coded by listing type
figure_1 <- basemap +
  tm_shape(property)+
  tm_dots(col = "Listing_Type",
          scale = 4/3, 
          palette = get_brewer_pal("-Dark2", n = 3), 
          alpha = 0.6, 
          legend.show = FALSE, 
          size = "revenue", 
          title.size = "Revenu", 
          size.lim = c(0, 100000)) +
  tm_add_legend(type="symbol",
                col= get_brewer_pal("-Dark2", n = 3),
                labels=c("Logement entier", "Chambre privée", "Chambre partagée"),
                border.lwd = NA,
                alpha = 0.6,
                title="Type de Logement") +
  tm_layout(legend.format = list(fun = function(x) {
    paste0("$", formatC(x, digits = 0, format = "f", big.mark = ","))}))

tmap_save(figure_1, "output/figure_1.png", width = 2400, height = 2400 )


# Figure 2. All illegal listings under current legislation

figure_2 <- basemap +
  tm_shape(filter(property, Legal == FALSE))+
  tm_dots(col = "darkred", 
          size = "revenue",
          size.lim = c(0, 100000),
          scale = 4/3,
          alpha = 0.6, 
          title.size = "Revenu") +
  tm_layout(legend.format = list(fun = function(x) {
    paste0("$", formatC(x, digits = 0, format = "f", big.mark = ","))}))

tmap_save(figure_2, "output/figure_2.png", width = 2400, height = 2400 )


## Figure 3. Exact location of permitted listings
figure_3 <- basemap +
  tm_shape(plateau_address)+
  tm_dots(col = "darkblue", 
          size = 0.4, 
          alpha = 0.6, 
          border.col = "black",
          border.lwd = 0.7)

tmap_save(figure_3, "output/figure_3.png", width = 2400, height = 2400 )


# Figure 4. Housing loss - FREH + ghost hotels 
figure_4 <- basemap +
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
    title.size = "Revenu") +
  tm_layout(legend.format = list(fun = function(x) {
    paste0("$", formatC(x, digits = 0, format = "f", big.mark = ","))}))

tmap_save(figure_4, "output/figure_4.png", width = 2400, height = 2400 )


## Figure 5. All entire homes on St Denis, St Laurent?

figure_5 <- 
  tm_shape(st_union(st_buffer(st_laurent_seg,200), st_buffer(st_denis_seg,200))) +
  tm_fill(col="grey", alpha = .45)+
  tm_shape(plateau_streets)+
  tm_lines(col="grey")+
  tm_shape(candidate_streets)+
  tm_lines(col= "grey42")+
  tm_shape(filter(st_denis_prop,Legal==FALSE))+
  tm_dots(size = 0.3, col="darkred", alpha = 0.6)+
  tm_shape(filter(st_laurent_prop, Legal==FALSE))+
  tm_dots(size = 0.3, col="darkred", alpha = 0.6)+
  tm_shape(filter(plateau_address, str_detect(Address, "Laurent")|
                    str_detect(Address, "Denis")))+
  tm_dots(size = 0.3, col= "darkblue", alpha = 0.6)+
  tm_add_legend(type="symbol",
                col= c("darkred", "darkblue"),
                labels=c("Résidences de tourisme illégales", "Résidences avec une attestation"),
                border.lwd = NA,
                alpha = 0.6,
                title=" ") +
  tm_layout(legend.position = c("left", "bottom"), legend.width=6,
            frame = FALSE, fontfamily = "Futura-Medium",
            title.fontfamily = "Futura-CondensedExtraBold")

tmap_save(figure_5, "output/figure_5.png", width = 2400, height = 2400 )
