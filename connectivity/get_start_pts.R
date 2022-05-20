# Angus Watters
# Find the lowest downstream point within in HUC4 containing LHD points

rm(list = ls())

# Libaries
library(tidyverse)
library(sf)
library(dataRetrieval)
library(nhdplusTools)
library(mapview)

# Source utility functions
source("utils/data_utils.R")

# *******************************
# ---- Locate network starts ----
# *******************************

fline_connect <- readRDS("data/spatial/points/lhd_flowline_connectivity.rds") %>% 
  sf::st_transform(5070) %>% 
  dplyr::mutate(
    huc4 = substr(REACHCODE, 1, 4)
  )

# Unique HUC4s
unique(fline_connect$huc4)

# Colorado LHD bounding box to subset NHDplus
co_bb <- 
  USAboundaries::us_states() %>% 
  dplyr::filter(stusps == "CO") %>% 
  sf::st_bbox() %>% 
  sf::st_as_sfc() %>%
  sf::st_as_sf() %>% 
  sf::st_transform(5070) 

# down_comid <- 1235725
ds_comids <- 
  fline_connect %>% 
  dplyr::group_by(huc4) %>% 
  dplyr::arrange(Hydroseq, .by_group = T) %>% 
  dplyr::slice(1)

# empty list
start_lst <- list()

# Iterate through each HUC4 in Colorado and find lowest downstream point 
for (i in 1:length(ds_comids)) {
  
  # HUC4
  huc4_txt <- ds_comids$huc4[i]
  
  # Starting comid 
  start    <- as.integer(ds_comids$COMID[i])
  
  logger::log_info("Retrieving COMID further downstream from:\nCOMID: {start}\nHUC4: {huc4_txt}")
  
  # Upstream tributaries
  ds_fline <- nhdplusTools::navigate_network(
    start       = start,
    mode        = "DM", 
    distance_km = 75
  ) %>% 
    sf::st_transform(5070) 
   
  
  # Stop parrellization 
  doParallel::stopImplicitCluster()
  
  # Crop to colorado and HUC4
  ds_fline <-
    ds_fline %>% 
    sf::st_filter(co_bb) %>%
    nhdplusTools::align_nhdplus_names() %>% 
    dplyr::mutate(
      huc4 = substr(REACHCODE, 1, 4)
    ) %>% 
    dplyr::filter(huc4 == huc4_txt) %>% 
    dplyr::arrange(Hydroseq) %>% 
    dplyr::slice(1)
  
  start_lst[[i]] <- ds_fline
  
}

# Starting COMIDs for each HUC4 with LHD dams
huc_starts <- bind_rows(start_lst)

# Save HUC4 starting points 
saveRDS(huc_starts, "data/spatial/lines/start_lines.rds")


