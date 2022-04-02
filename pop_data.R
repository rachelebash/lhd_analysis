# getting population data



library(tidycensus)
library(tidyverse)
library(tigris)


source("misc/find_google_drive.R")
source("utils/data_utils.R")


pop <- get_pop("block group", "P1_001N", "08", "2020")





