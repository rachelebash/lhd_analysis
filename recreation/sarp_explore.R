# Rachel Bash 4/14/2022
# sarp inventory data

# set up ------------------
remove(list = ls())  # clear all workspace variables

library(tidyverse)
library(tigris)
library(sf)
library(sp)
library(rgeos)
library(raster)
library(plotly)
library(units)
library(zoo)
library(logger)
library(USAboundaries)
library(mapview)

#utility functions
source("utils/find_google_drive.R")
source("utils/data_utils.R")

#global options
theme_set(theme_classic())
options(scipen = 1000) # get rid of scientific notation

# read in LHD data
lhd <- read.csv(paste0(drive_dir, "/data/Low Head Dam Inventory Final CIM 092920 - Inventory.csv")) %>%
  janitor::clean_names() %>%
  select(-x, -x_1) %>%
  mutate(uid = row_number())

# transform lhd to sf
lhd <- st_as_sf(lhd, coords = c("longitude", "latitude"), crs = 4326)

# read in state border
CO <- USAboundaries::us_states(states = "CO")


# SARP data -----------

sarp <- read.csv(paste0(drive_dir, "/data/aquatic_barrier_ranks_2022-02-28.csv")) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  janitor::clean_names() 

sarp_tidy <- sarp %>%
  select(sarpid, river, basin, subbasin, subwatershed, huc6, huc8, huc12, county, 
         year_removed, purpose, passage_facility, barrier_severity, te_spp, state_sgcn_spp,
         trout, owner_type, protected_land, excluded, has_network, ranked, stream_order, landcover, 
         size_classes
         )

mapview(sarp, col.regions = "red") + lhd






# match sarp lhd (do later) ---------------

# convert atlas points and lhd points to projection w/ imperial units
lhd_mi <- st_transform(lhd, "+proj=utm +zone=13 +datum=NAD83 +units=mi") 
sarp_mi <- st_transform(sarp, "+proj=utm +zone=13 +datum=NAD83 +units=mi")

#there are a handful that are further away from each other than 0.05mi, not sure how to snap it to each other
# without a common identifier
lhd_sarp <- st_snap(sarp_mi, lhd_mi, tolerance = 0.05)

mapview(lhd_sarp, col.regions = "red") +  lhd_mi +
  mapview(sarp_mi, col.regions = "yellow")

lhd_sarp_join <- st_join(lhd_mi, lhd_sarp)

mapview(lhd_sarp_join)
