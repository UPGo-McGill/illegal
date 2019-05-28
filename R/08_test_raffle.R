############### RUN RAFFLE ##################

## Load helpers

source("R/01_helper_functions.R")
source("R/07_str_raffle.R")

## Test Raffle

             
DA_plateau <- DA[lengths(st_intersects(DA, plateau))>0,]


  tm_shape(DA_plateau) +
    tm_borders(col="black")+
    tm_shape(plateau)+
    tm_borders(col="red")+
    tm_shape(st_laurent_buff)+
    tm_fill(col="green", alpha = 0.4)+
    tm_shape(st_denis_buff)+
    tm_fill(col="green", alpha = 0.4)
    

