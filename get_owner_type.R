# Rachel Bash 6/9/2022
# ownership data

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
library(gmapsdistance)
library(USAboundaries)
library(mapview)
library(nhdplusTools)
library(nngeo)
library(viridis)

#utility functions
source("utils/find_google_drive.R")
source("utils/data_utils.R")

#global options
theme_set(theme_classic())
options(scipen = 1000) # get rid of scientific notation

# read in land type data from COMaP 

land <- st_read(paste0(drive_dir, "/data/COMaP_SHP/COMaP_v20211005.shp")) %>%
  janitor::clean_names()

crs(land)


# read in LHD data
lhd_pt <- readr::read_csv("data/lhd/Low Head Dam Inventory Final CIM 092920 - Inventory.csv") %>%
  janitor::clean_names() %>% 
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs    = 4326) %>%
  st_transform(crs(land)) %>%  # match to land type
  rename("lhd_id" = "id") %>%
  mutate(new_id = 1:dplyr::n()) %>%
  relocate(new_id) %>%
  select(-x11,-x12)


mapview(land, zcol = "legend")


#join lhd and land type
lhd_land <- st_join(lhd_pt, land) %>%
  select(new_id, structure_category, name, owner, manager, owner_deta, manager_de, 
         mgmt_descr, easement_h, public_acc, legend)

saveRDS(lhd_land, "data/owner_type/owner_type.rds")

types <- distinct(lhd_land, legend, mgmt_descr) %>%
  arrange(legend)

write.csv(types, "data/owner_type/all_owner_types.csv", row.names = FALSE)
