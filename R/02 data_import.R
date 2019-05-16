############### DATA IMPORT ##################

source("R/01 helper_functions.R")

## import plateau dataset
montreal <- read_sf(dsn = ".", layer = "plateau") 

plateau <- filter(plateau, NOM == "Le Plateau-Mont-Royal")

## import airbnb listings from 2018

property_file <- read_csv("Data/Montreal_property.csv")


