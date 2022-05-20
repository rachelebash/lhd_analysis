# Angus Watters
# Retrieve HUC shapefiles using nhdplusTools

rm(list = ls())

# Libaries
library(tidyverse)
library(sf)
library(dataRetrieval)
library(nhdplusTools)
library(mapview)

# Source utility functions
source("utils/data_utils.R")

# ***************
# ---- HUC4  ---- 
# ***************

# LHD points
fline_connect <- readRDS("data/spatial/points/lhd_flowline_connectivity.rds") %>% 
  sf::st_transform(5070) %>% 
  dplyr::mutate(
    huc4 = substr(REACHCODE, 1, 4)
  )
 

# Unique HUC4s
hucs <- unique(fline_connect$huc4)

# get bounding box of subbasins
bb_sf <-
  fline_connect %>%
  sf::st_bbox() %>% 
  sf::st_as_sfc() %>%
  sf::st_as_sf()

# aggregate HUC8 to HUC4
huc_shp <- nhdplusTools::get_huc8(
  AOI    = bb_sf, 
  buffer = 0
) %>%  
  sf::st_transform(5070) %>% 
  dplyr::mutate(
    huc4 = substr(huc8, 1, 4)
  ) %>% 
  dplyr::group_by(huc4) %>% 
  dplyr::summarise() %>% 
  dplyr::mutate(area = round(as.numeric(sf::st_area(.)), 4)) %>% 
  dplyr::relocate(huc4, area, geometry) %>% 
  dplyr::arrange(area)

# stop external workers
doParallel::stopImplicitCluster()

# Save HUC4 shapes
saveRDS(huc_shp, "data/spatial/shp/huc4.rds")



