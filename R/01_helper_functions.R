######### HELPER FUNCTIONS #########################

library(tidyverse)
library(sf)
library(osmdata)
library(cancensus)
library(tmap)
library(tmaptools)
library(units)
library(dodgr)
library(stplanr)
library(mapview)

## Census Mapper - Run Personal API
# options(cancensus.api_key = "CensusMapper_a552c075421feb150db6cbf1d1707230")
# options(cancensus.cache_path = "~/UPGo/illegal")


## multilistings function
strr_multilistings <- function(daily, EH = 2, PR = 3, listing_type, host_ID,
                               date, cores){
  
  listing_type <- enquo(listing_type)
  host_ID <- enquo(host_ID)
  date <- enquo(date)
  
  daily %>%
    group_by(!! listing_type, !! host_ID, !! date)  %>%
    mutate(ML = ifelse(
      n() >= EH & !! listing_type == "Entire home/apt", TRUE,
      ifelse(n() >= PR & !! listing_type == "Private room", TRUE, FALSE))) %>%
    ungroup()
}
