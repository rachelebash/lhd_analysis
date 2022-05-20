# Angus Watters
# Calculate arbolate sum at each LHD site

rm(list = ls())

# Libaries
library(tidyverse)
library(sf)
library(dataRetrieval)
library(nhdplusTools)
library(mapview)
library(tidygraph)
library(sfnetworks)
library(nngeo)

# Source utility functions
source("utils/data_utils.R")

# *************************************
# ---- Upstream tributary networks ----
# *************************************

# **********************
# ---- Connectivity ----
# **********************

# LHD Points w/ NHDplus info, remove some issue LHDs
lhd_pts <- readRDS("data/spatial/points/lhd_flowline_points.rds") %>% 
  sf::st_transform(5070) %>% 
  dplyr::mutate(
    huc4 = substr(REACHCODE, 1, 4)
  ) %>% 
  filter(!COMID %in% c(17876471, 17876487,  17900363, 17900361))

# Path to clean directed stream networks
stream_network_path <- paste0(
  "data/spatial/networks/trim/", 
  list.files("data/spatial/networks/trim/", pattern = "trim")
)
 

# Empty lists to add to 
connect_lst <- list()
edge_lst    <- list()

# Loop through networks and calculate connectivity metrics 
for (i in 1:length(stream_network_path)) {
  
  net_txt <- gsub("data/spatial/networks/trim/", "", stream_network_path[i])
  
  ipct    <- paste0(round(100*(i/length(stream_network_path)), 2))
  
  logger::log_info("{ipct} %")
  logger::log_info("Calculating stream network connectivity\n File name: {net_txt}\n Full path: {stream_network_path[i]}")
  
  # Upstream tributary network
  ut_net <- readRDS(stream_network_path[i])
  
  # Prepare flowlines to use in sfnetwork
  flines <- prep_flines(
    flowlines = ut_net,
    split     = FALSE
    )

  # Prepare points to use in sfnetwork
  pts  <- prep_points(
    flowlines = ut_net,
    points    = lhd_pts,
    wb        = TRUE
    )
  
  net    <- create_network(
    flowlines = flines,
    points    = pts
    )
  
  # Partition directed network edges between LHD points, waterbodies, and headwaters
  group_net <- 
    net %>%
    sfnetworks::activate("edges") %>%
    dplyr::mutate(group = group_custom())
  
  # Plot the results.
  nodes        <-  sf::st_as_sf(group_net, "nodes")
  edges        <-  sf::st_as_sf(group_net, "edges")
  
  # Calculate distances between LHDs, dams, reservoirs, headwaters
  connect <- network_connectivity(
    flines      = edges,
    points      = pts,
    return_wide = FALSE
    ) %>%
    mutate(network = net_txt)
  
  logger::log_info("Connectivity analysis complete: \n{net_txt}")
  
  edge_final <- 
    edges %>% 
    mutate(network = net_txt)
  
  connect_lst[[i]] <- connect
  edge_lst[[i]]    <- edge_final
  
}

# Long datasets with all points and distances (includes Waterbody points)
connect_df <- bind_rows(connect_lst)
edge_df    <- bind_rows(edge_lst)

# Save long dataset
saveRDS(connect_df, "data/spatial/networks/connectivity/network_connectivity.rds")
saveRDS(edge_df, "data/spatial/networks/connectivity/network_connectivity_flowlines.rds")

# Widen datasets and keep only LHDs
lhd_connect <- 
  connect_df %>% 
  dplyr::group_by(new_id) %>% 
  dplyr::filter(!grepl("waterbody_", new_id)) %>% 
  dplyr::mutate(cnt = n()) %>% 
  dplyr::filter(!group %in% c(873) & !new_id %in% c(796, 795)) %>% 
  tidyr::pivot_wider(
    id_cols     = c(new_id, hydroseq, comid, reachcode, 
                    # group_list,
                    geometry),
    names_from  = "position",
    names_glue  = "{position}_{.value}",
    values_from = c(length, comid_list, group)
    # values_from = c(length, min_topo_sort, comid_list)
  ) %>% 
  dplyr::relocate(new_id, comid, hydroseq, us_length, ds_length, 
                  us_comid_list, ds_comid_list, us_group, ds_group, reachcode,
                  # group_list,
                  geometry)

# Save wide dataset
saveRDS(lhd_connect, "data/spatial/networks/connectivity/lhd_network_connectivity.rds")



 