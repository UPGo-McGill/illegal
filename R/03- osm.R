
library(extrafont)
library(osmdata)
library(sf)
library(smoothr)
library(tidycensus)
library(tidyverse)
library(tigris)
library(tmap)
library(tmaptools)
library(units)
library(dodgr)
library(stplanr)


hf_osm <-
  plateau_listings %>%
#  filter(nbhd == "plateau") %>%
#  st_transform(4326) %>%
  st_bbox() %>%
  bb(ext = 1.2) %>%
  as.vector() %>%
  opq() %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

available_features()

jhf_streets <-
  rbind(jhf_osm$osm_polygons %>% st_cast("LINESTRING"), jhf_osm$osm_lines) %>%
  as_tibble() %>%
  st_as_sf() %>%
  st_transform(26918) %>%
  select(osm_id, name, geometry)

jhf_dodgr <-
  dodgr_streetnet("queens new york city") %>%
  weight_streetnet() %>%
  dodgr_to_sf() %>%
  st_sf() %>%
  as_tibble() %>%
  st_as_sf() %>%
  st_transform(26918)

jhf_dodgr <-
  target_neighbourhoods %>%
  filter(nbhd == "Jackson Heights/Flushing") %>%
  st_buffer(2500) %>%
  st_intersection(jhf_dodgr, .)
