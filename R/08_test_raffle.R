############### RUN RAFFLE ##################

## Load helpers

source("R/01_helper_functions.R")
source("R/07_str_raffle.R")

## Test Raffle

             
tst_DA <- DA[lengths(st_within(DA, plateau))>0,]
plot(tst_DA)
