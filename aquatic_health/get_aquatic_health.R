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
library(nhdplusTools)

#utility functions
source("utils/find_google_drive.R")
source("utils/data_utils.R")

#global options
theme_set(theme_classic())
options(scipen = 1000) # get rid of scientific notation

# read in LHD data
lhd_pt <- readr::read_csv("data/lhd/Low Head Dam Inventory Final CIM 092920 - Inventory.csv") %>%
  janitor::clean_names() %>% 
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs    = 4326) %>%
  st_transform(5070) %>%  # to change to miles
  rename("lhd_id" = "id") %>%
  mutate(new_id = 1:dplyr::n()) %>%
  relocate(new_id) %>%
  select(-x11,-x12)

# get nfhp data ------------------------

lhd_comid <- readRDS(paste0(drive_dir, "/data/lhd_comid.rds")) %>%
  rename(uid = u_id)

nfhp <- read.csv(paste0(drive_dir, "/data/nfhp2015_hci_scores_limiting_dist_conterminous_us_v2.0.csv"))

# get connectivity
connect <- readRDS("data/spatial/networks/connectivity/lhd_network_connectivity.rds")


hci_all <- data.frame()
x <- c("new_id", "us_hci", "ds_hci", "tot_hci")

for (i in 1:length(connect$new_id)) {
  
  logger::log_info("find hci scores for {connect$new_id[i]}")
  id <- as.data.frame(connect$new_id[i])
  
  logger::log_info("list upstream and downstream comids of interest")
  us_tmp <- unlist(connect$us_comid_list[i])
  ds_tmp <- unlist(connect$ds_comid_list[i])
  
  logger::log_info("find average hci scores for upstream, downstream, and both lengths")
  us_nfhp <- nfhp %>% filter(comid %in% us_tmp) %>%
    summarise(us_nfhp = mean(cumu_hci))
  ds_nfhp <- nfhp %>% filter(comid %in% ds_tmp) %>%
    summarise(ds_nfhp = mean(cumu_hci))
  tot_nfhp <- nfhp %>% filter(comid %in% c(us_tmp, ds_tmp)) %>%
    summarise(tot_nfhp = mean(cumu_hci))
  
  logger::log_info("bind scores together")
  hci <- cbind(id, us_nfhp, ds_nfhp, tot_nfhp)
  colnames(hci) <- x
  
  hci_all <- rbind(hci_all, hci)
  
  
}

saveRDS(hci_all, "data/aquatic_health/hci_scores.rds")




# CO data -----------
dat <- readxl::read_xlsx(paste0(drive_dir, "/data/DATA_RachelBash_Lynker_04132022.xlsx"), sheet = "Data") %>%
  janitor::clean_names()


# HUC 12 data ---------

huc12 <- st_read(paste0(drive_dir, "/data/WBDHU12/WBDHU12.shp")) %>%
  st_transform(5070) 

lhd_huc <- st_intersection(lhd_pt, huc12) %>% 
  select(new_id, HUC12, Name) %>% 
  st_drop_geometry() %>%
  mutate(HUC12 = as.numeric(HUC12)) %>%
  janitor::clean_names()

# join huc12 data and dat

species_lhd <- left_join(lhd_huc, dat, by = c("huc12"))

# 280 missing values for huc12 species

