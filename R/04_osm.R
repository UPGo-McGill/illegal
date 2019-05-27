###### CREATE BUFFERS FOR ILLEGAL LISTINGS #####

source("R/01 helper_functions.R")

## Get OSM data

plateau_roads <- 
  getbb("plateau-mont-royal montreal") %>% 
  opq() %>% 
  add_osm_feature(key = "highway") %>% 
  osmdata_sf()

plateau_streets <- 
  rbind(plateau_roads$osm_polygons %>% st_cast("LINESTRING"), plateau_roads$osm_lines) %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_transform(26918) %>%
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
st_laurent_section <- st_laurent_seg %>%
  st_difference(sherbrooke)%>%
  st_difference(montroyal) %>%
  st_cast("LINESTRING")%>%
  as.matrix()

st_laurent_section <- st_laurent_section[c(3,4,5),] %>%
  st_union()

##BUFFERS ST LAURENT, ST DENIS (200m)
st_denis_buff <-  st_denis_seg %>%
  st_buffer(200)

st_laurent_buff <-  st_laurent_section %>%
  st_buffer(200)


## Properties within St Laurent & St Denis Buffers
property_CRS <- 
  property %>%
  st_as_sf() %>% 
  st_transform(26918)

st_laurent_prop <- property[lengths(st_within(property_CRS, st_laurent_buff))>0,]
st_denis_prop <- property[lengths(st_within(property_CRS, st_denis_buff))>0,]


st_l_d <- st_union(st_laurent_buff, st_denis_buff)

## St-Denis/St-Laurent buffers and listings within buffers
tm_shape(st_buffer(plateau,200))+
  tm_borders("black")+
#  tm_shape(plateau_streets)+
#  tm_lines(col="grey")+
  tm_shape(candidate_streets)+
  tm_lines(col = "black") +
  tm_shape(st_laurent_buff[])+
  tm_fill(col="darkolivegreen3", alpha=.3)+
  tm_shape(st_denis_buff[])+
  tm_fill(col="darkolivegreen3", alpha = .3)+
  tm_shape(st_denis_prop[])+
  tm_dots(size = 0.01, col="black")+
  tm_shape(st_laurent_prop[])+
  tm_dots(size = 0.01, col = "black")+
  tm_shape(filter(property, Permit == TRUE))+
  tm_dots(size = 0.05, col="blue")


## St-L and St-D, will require permits
tm_shape(st_l_d)+
  tm_fill(col="grey", alpha = .3)+
  tm_shape(filter(st_denis_prop, Listing_Type=="Entire home/apt"))+
    tm_dots(size = 0.01, col="blue")+
  tm_shape(filter(st_denis_prop, Listing_Type=="Private room"))+
    tm_dots(size = 0.01, col="green")+
  tm_shape(filter(st_laurent_prop, Listing_Type=="Entire home/apt"))+
  tm_dots(size = 0.01, col="blue")+
  tm_shape(filter(st_laurent_prop, Listing_Type=="Private room"))+
  tm_dots(size = 0.01, col="green")+
  tm_layout(legend.position = c("left", "top"),frame = FALSE) +
  tm_compass()
  #tm_shape(plateau_streets)+
  #tm_lines(col="grey")
  #tm_shape(filter(property, Permit == TRUE))+
  #tm_dots(size = 0.05, col="blue")+


## Listings which will become illegal: All entire homes?
tm_shape(st_buffer(plateau,200))+
  tm_borders("black")+
  #  tm_shape(plateau_streets)+
  #  tm_lines(col="grey")+
  tm_shape(filter(property,  Listing_Type == "Entire home/apt"&
          Permit == FALSE))+
  tm_dots(size = 0.05, col="blue")
View(property)
  
  

mapview(permit)
