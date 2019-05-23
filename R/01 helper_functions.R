######### HELPER FUNCTIONS #########################

library(tidyverse)
library(sf)
library(dplyr)

## multilistings unction
strr_multilistings <- function(daily, EH = 2, PR = 3, listing_type, host_ID,
                               date, cores){
  
  listing_type <- enquo(listing_type)
  host_ID <- enquo(host_ID)
  date <- enquo(date)
  
  daily %>%
    group_by(!! listing_type, !! host_ID, !! date)  %>%
    mutate(ML = ifelse(
      n() >= 2 & !! listing_type == "Entire home/apt", TRUE,
      ifelse(n() >= 3 & !! listing_type == "Private room", TRUE, FALSE))) %>%
    ungroup()
}
