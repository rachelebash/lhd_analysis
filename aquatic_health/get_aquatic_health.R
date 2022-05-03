# Rachel Bash 4/20/2022
# aquatic habitat data

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
library(readxl)

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
  mutate(uid = row_number())  %>%
  select(uid, everything())

lhd <- st_as_sf(lhd, coords = c("longitude", "latitude"), crs = 4326)

# get nfhp data ------------------------

lhd_comid <- readRDS(paste0(drive_dir, "/data/lhd_comid.rds")) %>%
  rename(uid = u_id)

nfhp <- read.csv(paste0(drive_dir, "/data/nfhp2015_hci_scores_limiting_dist_conterminous_us_v2.0.csv"))

lhd_nfhp <- left_join(lhd_comid, nfhp, by = c("origin_comid" = "comid")) %>%
  select(uid, origin_comid, cumu_hci, cu_hcitext)



# species richness -------------------
tier1_fish <- st_read(paste0(drive_dir, "/data/CPW_PriorityWatershedsAquatic/HUC10_Species_Data.shp")) %>% 
  janitor::clean_names() %>%
  st_as_sf() %>%
  st_transform(., crs(lhd))

# find intersection between huc10 and lhd
tier_lhd <- st_intersection(tier1_fish, lhd)

mapview(tier1_fish, zcol= "TOTAL")


# CO data -----------
dat <- readxl::read_xlsx(paste0(drive_dir, "/data/DATA_RachelBash_Lynker_04132022.xlsx"), sheet = "Data") %>%
  janitor::clean_names() %>%
  mutate(huc10 = substr(HUC12,1,11))
