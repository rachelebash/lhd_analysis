# Rachel Bash 
# getting population data


remove(list = ls())  # clear all workspace variables
library(tidycensus)
library(tidyverse)
library(tigris)

#utility functions
source("utils/find_google_drive.R")
source("utils/data_utils.R")

# read in LHD data
lhd <- read.csv(paste0(drive_dir, "/data/Low Head Dam Inventory Final CIM 092920 - Inventory.csv"))

#get population data for Colorado by census block
pop <- get_pop("block group", "P1_001N", "08", "2020")

