# Rachel Bash 
# getting population data


remove(list = ls())  # clear all workspace variables
library(tidycensus)
library(tidyverse)
library(tigris)

#utility functions
source("utils/find_google_drive.R")
source("utils/data_utils.R")


pop <- get_pop("block group", "P1_001N", "08", "2020")





