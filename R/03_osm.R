###### CREATE BUFFERS FOR ILLEGAL LISTINGS #####

source("R/01_helper_functions.R")

## Get OSM data

plateau_streets <- 
  getbb("plateau-mont-royal montreal") %>% 
  opq() %>% 
  add_osm_feature(key = "highway") %>% 
  osmdata_sf()

plateau_streets <- 
  rbind(plateau_streets$osm_polygons %>% st_cast("LINESTRING"), plateau_streets$osm_lines) %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_transform(32618) %>%
  select(osm_id, name, geometry)

## STREETS PLATEAU

candidate_streets <- plateau_streets %>%
  filter(str_detect(name, "Saint-Laurent") |
           str_detect(name, "Sherbrooke") |
           str_detect(name, "Gilford") |
           str_detect(name, "Rue Saint-Denis") |
           str_detect(name, "Mont-Royal Est")) %>% 
  select(name)

st_denis <- 
  plateau_streets %>%
  filter(name == "Rue Saint-Denis")

st_laurent <- 
  plateau_streets %>%
  filter(str_detect(name, "Saint-Laurent"))

sherbrooke <- 
  plateau_streets %>%
  filter(str_detect(name, "Sherbrooke"))%>%
  st_union()

montroyal <- 
  plateau_streets %>%
  filter (str_detect(name, "Mont-Royal"))

## SELECT SEGMENTS IN LEGAL AREA
st_denis_seg <- st_denis[c(9,12,13, 14, 15, 16, 22, 27, 28, 31, 32, 33, 39, 40, 41, 42),]%>%
  st_union()
st_laurent_seg <- st_laurent [c(9,11,12),] %>%
  st_union()

## REMOVE EXTRA SEGMENT ST LAURENT
st_laurent_seg <- st_laurent_seg %>%
  st_difference(sherbrooke)%>%
  st_difference(montroyal) %>%
  st_cast("LINESTRING")%>%
  as.matrix()

st_laurent_seg <- st_laurent_seg[c(3,4,5),] %>%
  st_union()

## Properties within St Laurent & St Denis Buffers
st_laurent_prop <- property[lengths(st_within(property, 
                                              st_buffer(st_laurent_seg,200)))>0,]
st_denis_prop <- property[lengths(st_within(property, 
                                            st_buffer(st_denis_seg,200)))>0,]

## Buffer union for mapping
st_l_d <- st_union(st_laurent_buff, st_denis_buff)
