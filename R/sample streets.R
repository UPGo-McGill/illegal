##################### SAMPLING ##########################################

source("R/01_helper_functions.R")
source("R/04_osm.R")

## Sample from St-D and St-L

sample_st_l <- st_laurent_prop %>%
  filter(Legal ==FALSE)
sample_st_l <- sample_st_l[c(sample(1:802,50)),]

sample_st_d <- st_denis_prop %>%
  filter(Legal==FALSE)
sample_st_d <- sample_st_d[c(sample(1:409,50)),]
View(sample_st_d)
