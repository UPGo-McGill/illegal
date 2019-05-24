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

plot(plateau_streets)

candidate_streets <- plateau_streets %>%
  filter(str_detect(name, "Saint-Laurent") |
           str_detect(name, "Sherbrooke") |
           str_detect(name, "Gilford") |
           str_detect(name, "Rue Saint-Denis") |
           str_detect(name, "Mont-Royal Est")) %>% 
  select(name)

plot(candidate_streets)

st_denis <- 
  plateau_streets %>%
  filter(name == "Rue Saint-Denis")

tm_shape(candidate_streets) +
  tm_lines(col = "grey") +
  tm_shape(st_denis[8,]) +
  tm_lines(col = "red")

## segments on st denis: 9, 12,13, 14, 15, 16, 22, 27, 28, 31, 32, 33, 29, 40, 41, 42

st_laurent <- 
  plateau_streets %>%
  filter(name == "Boulevard Saint-Laurent")

tm_shape(candidate_streets) +
  tm_lines(col = "grey") +
  tm_shape(st_laurent[11,]) +
  tm_lines(col = "red")

# segments on st laurent: part of 9, and part of 11, 12

plot(plateau_streets %>% filter(name == "Rue Saint-Denis"))
plateau_streets %>% filter(str_detect(name, "Sherbrooke"))



####

plateau_dodgr <-
  dodgr_streetnet("plateau") %>%
  weight_streetnet() %>%
  dodgr_to_sf() %>%
  st_sf() %>%
  as_tibble() %>%
  st_as_sf() %>%
  st_transform(4326)

jhf_dodgr <-
  target_neighbourhoods %>%
  filter(nbhd == "Jackson Heights/Flushing") %>%
  st_buffer(2500) %>%
  st_intersection(jhf_dodgr, .)
