############### DATA IMPORT ##################

source("R/01 helper_functions.R")

## import plateau dataset
montreal <- read_sf(dsn = ".", layer = "plateau") 

plateau <- filter(plateau, NOM == "Le Plateau-Mont-Royal")
