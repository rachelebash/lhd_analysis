# Angus Watters
# Retrieve NHDplus NHD flowlines & waterbodies data for LHD locations

rm(list = ls())

library(tidyverse)
library(sf)
library(dataRetrieval)
library(nhdplusTools)
library(mapview)

source("utils/data_utils.R")

# *******************
# ---- Load data ---- 
# *******************
 

lhd_path <- "data/lhd/Low Head Dam Inventory Final CIM 092920 - Inventory.csv"

# Read in LHD file
lhd <- readr::read_csv(lhd_path) %>% 
  janitor::clean_names() 

# Convert LHD dataframe to spatial points  using sf package 
lhd_pt <- 
  lhd %>% 
  sf::st_as_sf(
    coords = c("longitude", "latitude"), 
    crs    = 4326
  ) %>% 
  sf::st_transform(5070) %>% 
  dplyr::rename("lhd_id" = "id") %>% 
  dplyr::mutate(new_id = 1:dplyr::n()) %>% 
  dplyr::relocate(new_id) 

# ****************
# ---- COMIDs ---- 
# ****************

# Retrieve COMIDs for each LHD

fline_lst    <- list()
comid_lst    <- list()
fline_pt_lst <- list()
centroid_lst <- list()
i <- 1
for (i in 1:nrow(lhd_pt)) {
  
  # LHD point
  pt  <- 
    lhd_pt[i, ] %>%
    sf::st_transform(4326) %>%  
    sf::st_coordinates()
  
  # LHD id
  new_id <- lhd_pt$new_id[i] 
  
  # GNIS Stream ID in LHD dataset
  stream_id   <- as.character(lhd_pt$assoc_stream_id[i])
  
  # GNIS Stream name in LHD dataset
  stream_name <- as.character(lhd_pt$stream_name[i])
  
  logger::log_info("Retrieving NHD COMID\n- LHD ID: {new_id}\n- Stream ID: {stream_id}\n- Stream name: {stream_name}")

  fline <- nhdplusTools::get_nhdplus(
    comid = dataRetrieval::findNLDI(location =   pt)$comid
  )
  
  fline <-
    fline %>%
    nhdplusTools::align_nhdplus_names() %>% 
    dplyr::mutate(
      new_id         = new_id
    )  %>% 
    dplyr::mutate(dplyr::across(where(is.numeric), as.character)) %>% 
    dplyr::mutate(
      gnis_id = dplyr::case_when(
        grepl("^\\s*$", gnis_id)   ~ stream_id,
        TRUE                       ~ gnis_id
      ),
      gnis_name = case_when(
        grepl("^\\s*$", gnis_name) ~ stream_name,
        TRUE                       ~ gnis_name
      )
    ) 
  
  # Snap LHD point onto line to use as LHD point snapped to grid
  fline_pt <-
    lhd_pt[i, ] %>%
    sf::st_transform(4326) %>% 
    st_snap_points(fline) 
  
  # save original coordinates of LHD before changing geometry to snapped points on flowlines
  new_lhd_pts <- 
    lhd_pt[i, ] %>% 
    sf::st_transform(4326) %>% 
    dplyr::mutate(
      original_lng = st_coordinates(.)[1],
      original_lat = st_coordinates(.)[2]
    ) 
  
  
  sf::st_geometry(new_lhd_pts) <- sf::st_geometry(fline_pt)
  
  # %>%  sf::st_drop_geometry() dplyr::bind_cols(fline_pt) %>% 
  
  new_lhd_pts <- 
    new_lhd_pts %>% 
    sf::st_sf() %>% 
    dplyr::mutate(new_id = as.character(new_id))
  
  new_fline_pt <- 
    fline %>% 
    sf::st_drop_geometry() %>% 
    dplyr::left_join(
      dplyr::select(
        dplyr::mutate(new_lhd_pts, new_id = as.character(new_id)),
        new_id, original_lng, original_lat
      ),
      by = c("new_id") 
    ) %>% 
    sf::st_sf()
  
  # point comid
  origin_df <- tibble::tibble(
    new_id       = new_id,
    origin_comid = as.integer(dataRetrieval::findNLDI(location =   pt)$comid),
    reach_code   = fline$REACHCODE,
    huc8         = substr(fline$REACHCODE, 0 ,8),
    hydroseq     = fline$Hydroseq,
    gnis_id      = fline$gnis_id,
    gnis_name    = fline$gnis_name,
    levelpathi   = fline$LevelPathI
  )
  
  origin_df <- origin_df %>%
    dplyr::mutate(new_id = as.character(new_id))
  
  # add comid to list 
  comid_lst[[i]]     <- origin_df
  fline_lst[[i]]     <- fline
  centroid_lst[[i]]  <- new_lhd_pts
  fline_pt_lst[[i]]  <- new_fline_pt
  
}

# bind rows of dataframe lists
comid_df    <- bind_rows(comid_lst)
fline_df    <- bind_rows(fline_lst)
new_lhd_pts <- bind_rows(centroid_lst)
fline_pts   <- bind_rows(fline_pt_lst)

# mapview(fline_pts, color = "red") + lhd_pt + fline_df

saveRDS(comid_df, "data/lhd/lhd_comid.rds")
saveRDS(fline_df, "data/spatial/lines/lhd_flowline.rds")
saveRDS(new_lhd_pts, "data/spatial/points/lhd_points.rds")
saveRDS(fline_pts, "data/spatial/points/lhd_flowline_points.rds")

