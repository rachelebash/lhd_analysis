# Angus Watters
# Download entire NHDPlus network and subset to Colorado and HUCs within colorado 

rm(list = ls())

# Libaries
library(tidyverse)
library(sf)
library(dataRetrieval)
library(nhdplusTools)
library(mapview)
library(tidygraph)
library(sfnetworks)

# Source utility functions
source("utils/data_utils.R")

# ***********************
# ---- Download data ---- 
# ***********************
# Directory to save NHDPlus download
outdir <- "C:/Users/angus/OneDrive/Desktop/nhdplus_dir/nhdplus"

# download NHDplus
download_nhdplusv2(
  outdir,
  url = paste0(
    "https://s3.amazonaws.com/edap-nhdplus/NHDPlusV21/",
    "Data/NationalData/NHDPlusV21_NationalData_Seamless",
    "_Geodatabase_Lower48_07.7z"
  ),
  progress = TRUE
)

# ************************
# ---- Subset NHDplus ---- 
# ************************

# Colorado LHD bounding box to subset NHDplus
bb <- 
  USAboundaries::us_states() %>% 
  dplyr::filter(stusps == "CO") %>% 
  sf::st_bbox()

# mapview(bb)

# Where to save NHDplus subset
output_file <- "C:/Users/angus/OneDrive/Desktop/nhdplus_dir/subset/nhdplus_co_subset.gpkg"

# path to full NHDplus
nhdplus_gdb <- "C:/Users/angus/OneDrive/Desktop/nhdplus_dir/nhdplus/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"

# Subset NHDPlus data and save
subset_nhdplus(
  bbox         = bb,
  output_file  = output_file,
  nhdplus_data = nhdplus_gdb,
  overwrite    = TRUE,
  status       = TRUE
  )

# ***************************************
# ---- Subset NHDplus - South Platte ---- 
# ***************************************

# SP bounding box to subset NHDplus

# Single HUC4 of South Platte
sp_huc    <- readRDS("data/spatial/shp/huc4.rds") %>% 
  dplyr::filter(huc4 == 1019) %>%
  sf::st_transform(5070)

# SP bounding box to subset NHDplus
bb_sp <-
  sp_huc %>%
  sf::st_transform(4326) %>% 
  sf::st_bbox() %>% 
  sf::st_as_sfc() %>%
  sf::st_as_sf() %>%
  sf::st_transform(5070) %>% 
  sf::st_buffer(10000) %>%
  sf::st_transform(4326) %>% 
  sf::st_bbox() 


mapview(bb_sp)

# Where to save NHDplus subset
output_file <- "C:/Users/angus/OneDrive/Desktop/nhdplus_dir/subset/nhdplus_south_platte_subset.gpkg"

# path to full NHDplus
nhdplus_gdb <- "C:/Users/angus/OneDrive/Desktop/nhdplus_dir/nhdplus/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"

# Subset NHDPlus data and save
nhdplusTools::subset_nhdplus(
  bbox         = bb_sp,
  output_file  = output_file,
  nhdplus_data = nhdplus_gdb,
  overwrite    = TRUE,
  status       = TRUE
)

# ***************************************
# ---- Subset NHDplus - Animas River ---- 
# ***************************************

# Single HUC4 of South Platte
animas_huc    <- readRDS("data/spatial/shp/huc4.rds") %>% 
  dplyr::filter(huc4 == 1408) %>%
  sf::st_transform(5070)

# Animas bounding box to subset NHDplus
bb_animas <-
  animas_huc %>%
  sf::st_transform(4326) %>% 
  sf::st_bbox() %>% 
  sf::st_as_sfc() %>%
  sf::st_as_sf() %>%
  sf::st_transform(5070) %>% 
  sf::st_buffer(10000) %>%
  sf::st_transform(4326) %>% 
  sf::st_bbox() 


mapview(bb_animas)

# Where to save NHDplus subset
output_file <- "C:/Users/angus/OneDrive/Desktop/nhdplus_dir/subset/nhdplus_animas_subset.gpkg"

# path to full NHDplus
nhdplus_gdb <- "C:/Users/angus/OneDrive/Desktop/nhdplus_dir/nhdplus/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"

# Subset NHDPlus data and save
nhdplusTools::subset_nhdplus(
  bbox          = bb_animas,
  output_file   = output_file,
  nhdplus_data  = nhdplus_gdb,
  flowline_only = TRUE,
  overwrite     = TRUE,
  status        = TRUE
)

# ****************************************
# ---- Subset NHDplus - Dolores River ---- 
# ****************************************

# Single HUC4 of South Platte
dolores_huc    <- readRDS("data/spatial/shp/huc4.rds") %>% 
  dplyr::filter(huc4 == 1403) %>%
  sf::st_transform(5070)
 

# Dolores bounding box to subset NHDplus
bb_dolores <-
  dolores_huc %>%
  sf::st_transform(4326) %>% 
  sf::st_bbox() %>% 
  sf::st_as_sfc() %>%
  sf::st_as_sf() %>%
  sf::st_transform(5070) %>% 
  sf::st_buffer(10000) %>%
  sf::st_transform(4326) %>% 
  sf::st_bbox() 


mapview(bb_dolores)

# Where to save NHDplus subset
output_file <- "C:/Users/angus/OneDrive/Desktop/nhdplus_dir/subset/nhdplus_dolores_subset.gpkg"

# path to full NHDplus
nhdplus_gdb <- "C:/Users/angus/OneDrive/Desktop/nhdplus_dir/nhdplus/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"

# Subset NHDPlus data and save
nhdplusTools::subset_nhdplus(
  bbox          = bb_dolores,
  output_file   = output_file,
  nhdplus_data  = nhdplus_gdb,
  flowline_only = TRUE,
  overwrite     = TRUE,
  status        = TRUE
)

# **************************************
# ---- Subset NHDplus - Yampa River ---- 
# **************************************

# Single HUC4 of South Platte
yampa_huc    <- readRDS("data/spatial/shp/huc4.rds") %>% 
  dplyr::filter(huc4 == 1405) %>%
  sf::st_transform(5070)

# Yampa bounding box to subset NHDplus
bb_yampa <-
  yampa_huc %>%
  sf::st_transform(4326) %>% 
  sf::st_bbox() %>% 
  sf::st_as_sfc() %>%
  sf::st_as_sf() %>%
  sf::st_transform(5070) %>% 
  sf::st_buffer(10000) %>%
  sf::st_transform(4326) %>% 
  sf::st_bbox() 


mapview(bb_yampa)

# Where to save NHDplus subset
output_file <- "C:/Users/angus/OneDrive/Desktop/nhdplus_dir/subset/nhdplus_yampa_subset.gpkg"

# path to full NHDplus
nhdplus_gdb <- "C:/Users/angus/OneDrive/Desktop/nhdplus_dir/nhdplus/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"

# Subset NHDPlus data and save
nhdplusTools::subset_nhdplus(
  bbox          = bb_yampa,
  output_file   = output_file,
  nhdplus_data  = nhdplus_gdb,
  flowline_only = TRUE,
  overwrite     = TRUE,
  status        = TRUE
)

# **************************************
# ---- Subset NHDplus - Rio Grande River ---- 
# **************************************

# Rio grande bounding box to subset NHDplus

# Single HUC4 of South Platte
rg_huc    <- readRDS("data/spatial/shp/huc4.rds") %>% 
  dplyr::filter(huc4 == 1301) %>%
  sf::st_transform(5070)

# Rio grande bounding box to subset NHDplus
bb_rg <-
  rg_huc %>%
  sf::st_transform(4326) %>% 
  sf::st_bbox() %>% 
  sf::st_as_sfc() %>%
  sf::st_as_sf() %>%
  sf::st_transform(5070) %>% 
  # sf::st_buffer(10000) %>%
  sf::st_transform(4326) %>% 
  sf::st_bbox() 


mapview(bb_rg) + rg_huc

# Where to save NHDplus subset
output_file <- "C:/Users/angus/OneDrive/Desktop/nhdplus_dir/subset/nhdplus_rg_subset.gpkg"

# path to full NHDplus
nhdplus_gdb <- "C:/Users/angus/OneDrive/Desktop/nhdplus_dir/nhdplus/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"

# Subset NHDPlus data and save
nhdplusTools::subset_nhdplus(
  bbox          = bb_rg,
  output_file   = output_file,
  nhdplus_data  = nhdplus_gdb,
  flowline_only = TRUE,
  overwrite     = TRUE,
  status        = TRUE
)


