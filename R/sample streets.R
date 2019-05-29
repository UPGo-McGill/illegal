##################### SAMPLING ##########################################

source("R/01_helper_functions.R")
source("R/04_osm.R")

## Sample from St-D and St-L

sample_st_l <- st_laurent_prop[c(sample(1:1822,50)),]
sample_st_d <- st_denis_prop[c(sample(1:1307,50)),]

View(sample_st_d)
