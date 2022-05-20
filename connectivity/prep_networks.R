# Angus Watters
# Retrieve HUC4 stream networks and prep/trim/clean to use as directed graphs 

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

# ********************
# ---- Rio Grande ----
# ********************

# LHD Points snapped to flowlines
lhd_pts <- readRDS("data/spatial/points/lhd_flowline_points.rds") %>% 
  sf::st_transform(5070) %>% 
  dplyr::mutate(
    huc4 = substr(REACHCODE, 1, 4)
  )

# HUC4 shape
huc_shp    <- readRDS("data/spatial/shp/huc4.rds") %>% 
  dplyr::filter(huc4 %in% unique(lhd_pts$huc4))

# Starting flowline for each HUC4
huc_starts <- readRDS("data/spatial/lines/start_lines.rds")

# path to South platte NHDplus dataset
nhdplus_path <- "C:/Users/angus/OneDrive/Desktop/nhdplus_dir/subset/nhdplus_co_subset.gpkg"

# South platte NHDplus subset
nhdplus <- sf::read_sf(nhdplus_path) %>% 
  sf::st_transform(5070) %>% 
  dplyr::mutate(
    huc4 = substr(REACHCODE, 1, 4),
    huc8 = substr(REACHCODE, 1, 8)
  ) 

# Single HUC4 of South Platte
rg_huc <-
  huc_shp %>%
  dplyr::filter(huc4 == 1301) %>%
  sf::st_transform(5070)

plot(huc_shp$geometry)
plot(rg_huc$geometry, col = "red", add = T)

# Rio Grande River Upstream tributary Network
rg_net <-
  nhdplus %>%
  # st_filter(rg_huc)
  dplyr::filter(huc4 == 1301) %>%
  dplyr::select(-huc4) %>%
  dplyr::rename(geometry = geom) 

# lower case names 
names(rg_net) <- tolower(names(rg_net))


mapview(rg_net$geometry) + rg_huc$geometry

# save full network
saveRDS(rg_net, "data/spatial/networks/full/rg_network.rds")

# find individual HUC8 for Animas River
rg_lhd <- 
  lhd_pts %>% 
  dplyr::filter(huc4 == 1301) %>% 
  dplyr::mutate(
    huc8 = substr(REACHCODE, 1, 8)
  )

# Mapview
# mapview(rg_networks, color = "red") + rg_trim 

# Seperate non dendritic branches
fline_groups <- 
  rg_net %>% 
  seperate_trees() 

ggplot() + 
  geom_sf(data = fline_groups, aes(color = section))

fline_groups <- 
  fline_groups %>% 
  dplyr::filter(section == 1)

ggplot() + 
  geom_sf(data = fline_groups, aes(color = section))

mapview(tmp_full, color = "red") + fline_groups + rg_lhd + tmp

 # COMIDs of main network section
main_tree_index <- sf::st_intersection(rg_net, fline_groups)

# Non dendritic flowlinest to keep because several LHDs are on the branch
keep_flines <- filter(rg_net, gnis_name == "North Branch Conejos River")$comid

# Non dendritic flowlines/loops that have to be manually removed in this particular case
rm_comids <- c(17875485, 17875487, 17876489)
# rm_comids <- c(17876473)

# Trim network, removing non dendritic 
rg_trim <-
  rg_net %>%
  dplyr::filter(comid %in% main_tree_index$comid) %>%
  dplyr::filter(streamorde == streamcalc | comid %in% keep_flines) %>% 
  dplyr::filter(comid != rm_comids)

mapview(rg_net) + edg + rt + lhd_pts

plot(seperate_trees(rg_trim))

rg_trim_groups <-
  rg_trim %>%
  seperate_trees() 

plot(seperate_trees(rg_trim))

rg_trim_groups <- rg_trim_groups %>%
  dplyr::filter(section == 1)

plot(seperate_trees(rg_trim_groups))

# COMIDs of main network section
trim_tree_index <- sf::st_intersection(rg_trim, rg_trim_groups)

# Trim network, removing non dendritic and seperated forests/networks
rg_final_trim <-
  rg_trim %>% 
  dplyr::filter(comid %in% trim_tree_index$comid)

# Added extra flowline as LHD is missing flowline
add_comid <- nhdplusTools::get_nhdplus(comid = c(17875585, 17875579))
 

add_comid <- add_comid %>% 
  mutate(across(c(hwnodesqkm, lakefract,surfarea, rareahload), as.numeric))

rg_final_trim <- bind_rows(rg_final_trim, add_comid)

# mapview::mapview(rg_final_trim, color = "red") + rg_lhd + rg_net + tmp

# plot
plot(rg_huc$geometry, add = F)
plot(rg_net$geometry, col = "darkgrey", add = T)
plot(filter(rg_net, !comid %in% rg_final_trim$comid), col = "red", add = T)
plot(filter(lhd_pts, huc4 == 1019)$geometry,
     col = "darkcyan", add = T)

# mapview(rg_final_trim, color = "red") + lhd_pts

# save trimmed network
saveRDS(rg_final_trim, "data/spatial/networks/trim/rg_network_trim.rds")

# ******************
# ---- Colorado ----
# ******************

# LHD Points snapped to flowlines
lhd_pts <- readRDS("data/spatial/points/lhd_flowline_points.rds") %>% 
  sf::st_transform(5070) %>% 
  dplyr::mutate(
    huc4 = substr(REACHCODE, 1, 4)
  )

# HUC4 shape
huc_shp    <- readRDS("data/spatial/shp/huc4.rds") %>% 
  dplyr::filter(huc4 %in% unique(lhd_pts$huc4))

# Starting flowline for each HUC4
huc_starts <- readRDS("data/spatial/lines/start_lines.rds")

# path to CO NHDplus dataset
nhdplus_path <- "C:/Users/angus/OneDrive/Desktop/nhdplus_dir/subset/nhdplus_co_subset.gpkg"

# Full Colorado NHDplus dataset
nhdplus <- sf::read_sf(nhdplus_path) %>% 
  sf::st_transform(5070) %>% 
  dplyr::mutate(
    huc4 = substr(REACHCODE, 1, 4),
    huc8 = substr(REACHCODE, 1, 8)
  ) 

# HUC4 shape
huc_shp    <- readRDS("data/spatial/shp/huc4.rds") %>% 
  dplyr::filter(huc4 %in% unique(lhd_pts$huc4))

# Single HUC4
co_huc <-
  huc_shp %>%
  dplyr::filter(huc4 == 1401) %>%
  st_transform(5070)

plot(huc_shp$geometry)
plot(co_huc$geometry, col = "red", add = T)

# Colorado River Upstream tributary Network
co_net <-
  nhdplus %>%
  dplyr::filter(huc4 == co_huc$huc4[1]) %>%
  dplyr::select(-huc4) %>%
  dplyr::rename(geometry = geom)

names(co_net) <- tolower(names(co_net))

# save full network
saveRDS(co_net, "data/spatial/networks/full/co_network.rds")

# Seperate non dendritic branches
fline_groups <- 
  co_net %>% 
  seperate_trees() %>% 
  dplyr::filter(section == 1)

# mapview(fline_groups)

# COMIDs of main network 
main_tree_index <- sf::st_intersection(co_net, fline_groups)
# main_tree_index <- sf::st_intersection(co_net, fline_groups2)

# Trim network, removing non dendritic 
co_trim <-
  co_net %>% 
  dplyr::filter(comid %in% main_tree_index$comid) %>% 
  dplyr::filter(streamorde == streamcalc)

co_trim_groups <-
  co_trim %>% 
  seperate_trees() %>% 
  dplyr::filter(section == 1)
  # sf::st_intersection(co_trim, .)

trim_tree_index <- sf::st_intersection(co_trim, co_trim_groups)

# Trim network, removing non dendritic 
co_final_trim <-
  co_trim %>% 
  dplyr::filter(comid %in% trim_tree_index$comid)

# mapview(co_final_trim)

# plot
plot(co_huc$geometry, add = F)
plot(co_net$geometry, col = "darkgrey", add = T)
plot(dplyr::filter(co_net, !comid %in% co_final_trim$comid), col = "red", add = T)
plot(dplyr::filter(lhd_pts, huc4 == 1401)$geometry,
     col = "darkcyan", add = T)

# save trimmed network
saveRDS(co_final_trim, "data/spatial/networks/trim/co_network_trim.rds")

# ******************
# ---- Gunnison ----
# ******************

# LHD Points snapped to flowlines
lhd_pts <- readRDS("data/spatial/points/lhd_flowline_points.rds") %>% 
  sf::st_transform(5070) %>% 
  dplyr::mutate(
    huc4 = substr(REACHCODE, 1, 4)
  )

# HUC4 shape
huc_shp    <- readRDS("data/spatial/shp/huc4.rds") %>% 
  dplyr::filter(huc4 %in% unique(lhd_pts$huc4))

# Starting flowline for each HUC4
huc_starts <- readRDS("data/spatial/lines/start_lines.rds")

# Colorado River Upstream tributaries
gunn_net <- nhdplusTools::navigate_network(
  start       = as.integer(huc_starts$COMID[5]), 
  mode        = "UT", 
  distance_km = 400
) %>% 
  st_transform(5070) 

doParallel::stopImplicitCluster()

# Plot HUC4, network lines, and LHD points
# plot(huc_shp$geometry)
# plot(filter(huc_shp, huc4 == huc_starts$huc4[5])$geometry, col = "dodgerblue", add = T)
plot(dplyr::filter(huc_shp, huc4 == huc_starts$huc4[5])$geometry, add = F)
plot(gunn_net$geometry, col = "darkgrey", add = T)
plot(dplyr::filter(lhd_pts, huc4 == huc_starts$huc4[5])$geometry,
     col = "red", add = T)

# save full network
saveRDS(gunn_net, "data/spatial/networks/full/gunnison_network.rds")

# Remove non dendritic
gunn_trim <-
  gunn_net %>%
  dplyr::filter(streamorde == streamcalc | comid == 3231451)

plot(dplyr::filter(huc_shp, huc4 == huc_starts$huc4[5])$geometry, add = F)
plot(gunn_net$geometry, col = "darkgrey", add = T)
plot(dplyr::filter(gunn_net, !comid %in% gunn_trim$comid), col = "red", add = T)
plot(dplyr::filter(lhd_pts, huc4 == huc_starts$huc4[5])$geometry,
     col = "darkcyan", add = T)

# mapview(gunn_net, color = "red") + gunn_trim 

# save trimmed network
saveRDS(gunn_trim, "data/spatial/networks/trim/gunnison_network_trim.rds")
# ***********************************************************************

# *****************
# ---- Dolores ----
# *****************

# LHD Points snapped to flowlines
lhd_pts <- readRDS("data/spatial/points/lhd_flowline_points.rds") %>% 
  sf::st_transform(5070) %>% 
  dplyr::mutate(
    huc4 = substr(REACHCODE, 1, 4)
  )

# HUC4 shape
huc_shp    <- readRDS("data/spatial/shp/huc4.rds") %>% 
  dplyr::filter(huc4 %in% unique(lhd_pts$huc4))

# Starting flowline for each HUC4
huc_starts <- readRDS("data/spatial/lines/start_lines.rds")

# find individual HUC8 for Animas River
dolo_lhd <- 
  lhd_pts %>% 
  dplyr::filter(huc4 == 1403) %>% 
  dplyr::mutate(
    huc8= substr(REACHCODE, 1, 8)
  )

# # HUC4 shape
huc_shp    <- readRDS("data/spatial/shp/huc4.rds") %>%
  dplyr::filter(huc4 %in% unique(lhd_pts$huc4))

plot(huc_shp$geometry)
# plot(filter(huc_shp, huc4 == 1403), col = "red", add = T)
plot(dolo_huc8$geometry, col = "red", add = T)
plot(dolo_lhd$geometry, col = "darkgrey", add = T)

# HUC 8 shapes
dolo_huc8 <- nhdplusTools::get_huc8(
  AOI = dplyr::filter(huc_shp, huc4 == 1403)
) %>% 
  sf::st_filter(dolo_lhd)

# path to CO NHDplus dataset
nhdplus_path <- "C:/Users/angus/OneDrive/Desktop/nhdplus_dir/subset/nhdplus_dolores_subset.gpkg"

# Full Colorado NHDplus dataset
nhdplus <- sf::read_sf(nhdplus_path) %>% 
  sf::st_transform(5070) %>% 
  dplyr::mutate(
    huc4 = substr(REACHCODE, 1, 4),
    huc8 = substr(REACHCODE, 1, 8)
  ) 

dolo_networks <- 
  nhdplus %>% 
  dplyr::filter(huc8 %in% dolo_huc8$huc8) 

unique_huc8 <- unique(dolo_networks$huc8)

plot(huc_shp$geometry)
# plot(filter(huc_shp, huc4 == 1403), col = "red", add = T)
plot(dolo_huc8$geometry, add = T)
plot(dolo_networks$geom, col = "darkgrey", add = T)
plot(dolo_lhd$geometry, col = "red", add = T)

# Loop through HUC8s and clean networks and save
for (i in 1:length(unique_huc8)){
  
  huc8_id <- unique_huc8[i]
  
  logger::log_info("Cleaning up upstream tributary network:\nHUC8: {huc8_id}")
  
  ut_net  <-
    dolo_networks %>% 
    dplyr::filter(huc8 == huc8_id) %>% 
    dplyr::select(-huc4, -huc8) %>%
    dplyr::rename(geometry = geom)
  
  # 760001454
  names(ut_net) <- tolower(names(ut_net))
  
  net_path <- 
    paste0(
      here::here("data/spatial/networks/full/"), 
      paste0('/dolores_network_', huc8_id ,'.rds')
    )
  
  logger::log_info("Saving full network:\nHUC8: {huc8_id}\n{net_path}")
  
  # save full network
  saveRDS(
    ut_net, 
    net_path
  )
  
  # plot(ut_net$geom, col = "black", add = F)
  
  # Seperate non dendritic branches
  fline_groups <-
    ut_net %>%
    seperate_trees() %>%
    dplyr::filter(section == 1)
  
  # find individual HUC8 for Animas River
  dolo_lhd <- 
    lhd_pts %>% 
    dplyr::filter(huc4 == 1403) %>% 
    dplyr::mutate(
      huc8= substr(REACHCODE, 1, 8)
    ) %>% 
    dplyr::filter(huc8 == huc8_id)
  
  plot(fline_groups$geometry)
  plot(dolo_lhd$geometry,col = "red", add = T)
  
  # COMIDs of main network 
  main_tree_index <- sf::st_intersection(ut_net, fline_groups)
  
  # Trim network, removing non dendritic 
  ut_trim <-
    ut_net %>% 
    dplyr::filter(comid %in% main_tree_index$comid) %>% 
    dplyr::filter(streamorde == streamcalc)
  
  trim_path <- 
    paste0(
      here::here("data/spatial/networks/trim/"), 
      paste0('/dolores_network_trim_', huc8_id ,'.rds')
    )
  logger::log_info("Saving trimmed network:\nHUC8: {huc8_id}\n{trim_path}")
  
  
  # save full network
  saveRDS(
    ut_trim, 
    trim_path
  )

}

# ***************
# ---- Yampa ----
# ***************

# LHD Points snapped to flowlines
lhd_pts <- readRDS("data/spatial/points/lhd_flowline_points.rds") %>% 
  sf::st_transform(5070) %>% 
  dplyr::mutate(
    huc4 = substr(REACHCODE, 1, 4)
  )

# HUC4 shape
huc_shp    <- readRDS("data/spatial/shp/huc4.rds") %>% 
  dplyr::filter(huc4 %in% unique(lhd_pts$huc4))

# Starting flowline for each HUC4
huc_starts <- readRDS("data/spatial/lines/start_lines.rds")

# find individual HUC8 for Animas River
yampa_lhd <- 
  lhd_pts %>% 
  dplyr::filter(huc4 == 1405) %>% 
  dplyr::mutate(
    huc8= substr(REACHCODE, 1, 8)
  )

# HUC 8 shapes
yampa_huc8 <- nhdplusTools::get_huc8(
  AOI = dplyr::filter(huc_shp, huc4 == 1405)
) %>% 
  dplyr::filter(huc8 %in% c(14050007, 14050005, 14050002, 14050001))  %>%
  dplyr::mutate(
    huc_group = dplyr::case_when(
      huc8 %in% c(14050002, 14050001) ~ "1", 
      huc8 %in% c(14050007, 14050005) ~ "2" 
      )
    )

# merge 2 main HUC8s
yampa_huc_groups <- 
  yampa_huc8 %>%
  group_by(huc_group) %>% 
  summarize()

plot(huc_shp$geometry)
plot(yampa_huc_groups$geometry, col = "red", add = T)
plot(yampa_lhd$geometry, col = "darkgrey", add = T)

# path to Yampa NHDplus dataset
nhdplus_path <- "C:/Users/angus/OneDrive/Desktop/nhdplus_dir/subset/nhdplus_yampa_subset.gpkg"

# Full Yampa NHDplus dataset
nhdplus <- sf::read_sf(nhdplus_path) %>% 
  sf::st_transform(5070) %>% 
  dplyr::mutate(
    huc4 = substr(REACHCODE, 1, 4),
    huc8 = substr(REACHCODE, 1, 8)
  ) 

# Subset yampa network to just HUC8s of interest
yampa_networks <- 
  nhdplus %>% 
  dplyr::filter(huc8 %in% yampa_huc8$huc8) 

# shapes to iterate over
unique_huc_groups <- unique(yampa_huc_groups$huc_group)
# unique_huc8 <- unique(yampa_networks$huc8)

plot(huc_shp$geometry)
plot(yampa_huc8$geometry, add = T)
plot(yampa_networks$geom, col = "darkgrey", add = T)
plot(yampa_lhd$geometry, col = "red", add = T)

# i <- 1

# Loop through HUC8s and clean networks and save

for (i in 1:length(unique_huc_groups)){
  # for (i in 1:length(unique_huc8)){
  
  huc_id <- unique_huc_groups[i]
  shp <- yampa_huc_groups[i, ]
  logger::log_info("Cleaning up upstream tributary network:\nHUCs: {huc_id}")
  
  
  # huc8_id <- unique_huc8[i]
  # shp <- yampa_huc8 %>% dplyr::filter(huc8 == huc8_id)
  # logger::log_info("Cleaning up upstream tributary network:\nHUC8: {huc8_id}")
  
  ut_net  <-
    yampa_networks %>%
    sf::st_filter(shp) %>%
    dplyr::select(-huc4, -huc8) %>%
    dplyr::rename(geometry = geom)
  
  # clean names
  names(ut_net) <- tolower(names(ut_net))
  
  
  net_path <- 
    paste0(
      here::here("data/spatial/networks/full/"), 
      paste0('/yampa_network_0', huc_id ,'.rds')
    )
  
  logger::log_info("Saving full network:\nHUC8: {huc_id}\n{net_path}")
  
  # save full network
  saveRDS(
    ut_net, 
    net_path
  )
  
  # plot(seperate_trees(ut_net))
  
  # Seperate non dendritic branches
  fline_groups <-
    ut_net %>%
    seperate_trees() %>%
    dplyr::filter(section == 1)
  
  # find individual HUC8 for Animas River
  # yampa_lhd <- lhd_pts %>% 
  #   st_filter(shp) %>% 
  #   # dplyr::filter(huc4 == 1405) %>%
  #   dplyr::mutate( huc8= substr(REACHCODE, 1, 8)) 
  
  # plot(ut_net$geometry)
  # plot(fline_groups$geometry)
  # plot(yampa_lhd$geometry,col = "red", add = T)
  
  # COMIDs of main network 
  main_tree_index <- sf::st_intersection(ut_net, fline_groups)
  
  # Specific COMIDs to remove
  rm_comids <- c(
    3113399, 3113539, 3112611, 3112653,   3112645, 3112667, 3112649, 
    3112677, 3112661, 3112693, 120299632, 3112637, 3112619,
    120299727, 120299629, 1352812, 120299721, 
    1357388,   1357376,   1357374,   120299723,
    1355054,   1355056, 120299630, 1352818, 
    1355090,  3111257,
    # 3113443, 120299726,
    # 3113399
    # yampa part 2 (white river)
    5306170,
    120299731, 1342624, 120299731, 1342832, # Divergence
    5308094, 5308272,# big split
    1343170,
    120299635
    )
  # mapview(flines) + ut_trim

  # Trim network, removing non dendritic 
  ut_trim <-
    ut_net %>% 
    dplyr::filter(comid %in% main_tree_index$comid) %>%
    dplyr::filter(streamorde == streamcalc) %>%
    dplyr::filter(!comid %in% rm_comids)
  
  # plot(seperate_trees(ut_trim))
  
  trim_fline_groups <-
    ut_trim %>%
    seperate_trees() %>% 
    dplyr::filter(section == 1)
  
  # COMIDs of main network 
  trim_tree_index <- sf::st_intersection(ut_trim, trim_fline_groups)

  # Trim network, removing non dendritic 
  final_ut_trim <-
    ut_trim %>% 
    dplyr::filter(comid %in% trim_tree_index$comid)  %>% 
    dplyr::filter(!comid %in% c(3111619))

  trim_path <- 
    paste0(
      here::here("data/spatial/networks/trim/"), 
      paste0('/yampa_network_trim_0', huc_id ,'.rds')
    )
  logger::log_info("Saving trimmed network:\nHUC8: {huc_id}\n{trim_path}")
  
  
  # save full network
  saveRDS(
    final_ut_trim, 
    trim_path
  )
  
}

# ****************
# ---- Animas ----
# ****************

# LHD Points snapped to flowlines
lhd_pts <- readRDS("data/spatial/points/lhd_flowline_points.rds") %>% 
  sf::st_transform(5070) %>% 
  dplyr::mutate(
    huc4 = substr(REACHCODE, 1, 4)
  )

# HUC4 shape
huc_shp    <- readRDS("data/spatial/shp/huc4.rds") %>% 
  dplyr::filter(huc4 %in% unique(lhd_pts$huc4))

# Starting flowline for each HUC4
huc_starts <- readRDS("data/spatial/lines/start_lines.rds")

# find individual HUC8 for Animas River
animas_lhd <- 
  lhd_pts %>% 
  dplyr::filter(huc4 == 1408) %>% 
  dplyr::mutate(
    huc8= substr(REACHCODE, 1, 8)
  )
huc_starts
# path to CO NHDplus dataset
nhdplus_path <- "C:/Users/angus/OneDrive/Desktop/nhdplus_dir/subset/nhdplus_animas_subset.gpkg"

# Full Colorado NHDplus dataset
nhdplus <- sf::read_sf(nhdplus_path) %>% 
  sf::st_transform(5070) %>% 
  dplyr::mutate(
    huc4 = substr(REACHCODE, 1, 4),
    huc8 = substr(REACHCODE, 1, 8)
  ) 

# # HUC4 shape
huc_shp    <- readRDS("data/spatial/shp/huc4.rds") %>%
  dplyr::filter(huc4 %in% unique(lhd_pts$huc4))

# Single HUC4
animas_huc <-
  huc_shp %>%
  dplyr::filter(huc4 == 1408) %>%
  st_transform(5070)

plot(huc_shp$geometry)
plot(animas_huc$geometry, col = "red", add = T)
plot(animas_lhd$geometry, col = "blue", add = T)

# HUC 8 shapes
# animas_huc8 <- nhdplusTools::get_huc8(
#   AOI = dplyr::filter(huc_shp, huc4 == 1408)
# ) %>% 
#   sf::st_filter(animas_lhd)

# SP bounding box to subset NHDplus
bb_animas <-
  animas_huc %>%
  sf::st_transform(4326) %>% 
  sf::st_bbox() %>% 
  sf::st_as_sfc() %>%
  sf::st_as_sf() 

huc8 <- get_huc8(AOI = bb_animas)


animas_net <- 
  nhdplus %>% 
  dplyr::filter(huc8 %in% animas_lhd$huc8 | huc8 == 14080201) %>% 
  dplyr::select(-huc4) %>%
  dplyr::rename(geometry = geom)

mapview(huc8) + animas_net + animas_lhd

# 760001454
names(animas_net) <- tolower(names(animas_net))

# Hydroseq to remove below --> 760001454
fline_groups <- 
  animas_net %>%
  # dplyr::select(comid, hydroseq, streamleve, levelpathi) %>% 
  dplyr::filter(hydroseq > 760001454) %>%
  seperate_trees() 
plot(fline_groups)
fline_groups <- fline_groups %>% 
  dplyr::filter(section == 1)

# ggplot() +
#   geom_sf(data = tmp, aes(color = section))

plot(animas_huc$geometry, add = F)
plot(animas_net$geometry, col = "darkgrey", add = T)
plot(fline_groups$geometry, col = "black", add = T)
# plot(ms$geometry, col = "green", add = T)
plot(animas_lhd$geometry, col = "red", add = T)

mapview(animas_huc$geometry) + fline_groups + animas_lhd

# save full network
saveRDS(animas_net, "data/spatial/networks/full/animas_network.rds")

# COMIDs of main network 
main_tree_index <- sf::st_intersection(animas_net, fline_groups)
# main_tree_index <- sf::st_intersection(co_net, fline_groups2)

# Trim network, removing non dendritic 
animas_trim <-
  animas_net %>% 
  dplyr::filter(comid %in% main_tree_index$comid) %>% 
  dplyr::filter(streamorde == streamcalc)

mapview(animas_net, color = "red") + animas_trim

# save trimmed network
saveRDS(animas_trim, "data/spatial/networks/trim/animas_network_trim.rds")

# Seperate non dendritic branches
# fline_groups <- 
#   tmp %>% 
#   seperate_trees() %>% 
#   dplyr::filter(section == 1)



# **********************************************************************

# find individual HUC8 for Animas River
animas_lhd <- 
  lhd_pts %>% 
  dplyr::filter(huc4 == 1408) %>% 
  dplyr::mutate(
    huc8= substr(REACHCODE, 1, 8)
  )

# # HUC4 shape
huc_shp    <- readRDS("data/spatial/shp/huc4.rds") %>%
  dplyr::filter(huc4 %in% unique(lhd_pts$huc4))

# HUC 8 shapes
animas_huc8 <- nhdplusTools::get_huc8(
  AOI = dplyr::filter(huc_shp, huc4 == 1408)
) %>% 
  sf::st_filter(animas_lhd)

# path to CO NHDplus dataset
nhdplus_path <- "C:/Users/angus/OneDrive/Desktop/nhdplus_dir/subset/nhdplus_animas_subset.gpkg"

# Full Colorado NHDplus dataset
nhdplus <- sf::read_sf(nhdplus_path) %>% 
  sf::st_transform(5070) %>% 
  dplyr::mutate(
    huc4 = substr(REACHCODE, 1, 4),
    huc8 = substr(REACHCODE, 1, 8)
  ) 

animas_networks <- 
  nhdplus %>% 
  dplyr::filter(huc8 %in% animas_huc8$huc8) 

unique_huc8 <- unique(animas_networks$huc8)
i = 4
for (i in 1:length(unique_huc8)){

  
  huc8_id <- unique_huc8[i]
  
  logger::log_info("Cleaning up upstream tributary network:\nHUC8: {huc8_id}")
  
  ut_net  <-
    animas_networks %>% 
    dplyr::filter(huc8 == huc8_id) %>% 
    dplyr::select(-huc4, -huc8) %>%
    dplyr::rename(geometry = geom)
  
  # 760001454
  names(ut_net) <- tolower(names(ut_net))
  
  net_path <- 
    paste0(
      here::here("data/spatial/networks/full/"), 
      paste0('/animas_network_', huc8_id ,'.rds')
      )
  
  logger::log_info("Saving full network:\nHUC8: {huc8_id}\n{net_path}")
  
  # save full network
  saveRDS(
    ut_net, 
    net_path
    )
  
  # plot(ut_net$geom, col = "black", add = F)
  # Seperate non dendritic branches
  fline_groups <-
    ut_net %>%
    seperate_trees() %>%
    dplyr::filter(section == 1)
  
  rm_comids <- c(17003484,17001972,17001964, 17001920, 17003476)
  
  
  # COMIDs of main network 
  main_tree_index <- sf::st_intersection(ut_net, fline_groups)
  # main_tree_index <- sf::st_intersection(co_net, fline_groups2)
  
  # Trim network, removing non dendritic 
  ut_trim <-
    ut_net %>% 
    dplyr::filter(comid %in% main_tree_index$comid) %>% 
    dplyr::filter(streamorde == streamcalc) %>% 
    dplyr::filter(!comid %in% rm_comids) 
  
  # Seperate non dendritic branches
  trim_groups <-
    ut_trim %>%
    seperate_trees() %>%
    dplyr::filter(section == 1)
  
  # COMIDs of main network 
  trim_tree_index <- sf::st_intersection(ut_trim, trim_groups)
  # main_tree_index <- sf::st_intersection(co_net, fline_groups2)
  
  # Trim network, removing non dendritic 
  final_ut_trim <-
    ut_trim %>% 
    dplyr::filter(comid %in% trim_tree_index$comid) 
  
  # mapview(ut_trim, color = "red") + ut_trim2
  # mapview(animas_net, color = "red") + animas_trim
  
  # # save trimmed network
  # saveRDS(ut_trim, "data/spatial/networks/   animas_network_trim.rds")
  
  trim_path <- 
    paste0(
      here::here("data/spatial/networks/trim/"), 
      paste0('/animas_network_trim_', huc8_id ,'.rds')
    )
  logger::log_info("Saving trimmed network:\nHUC8: {huc8_id}\n{trim_path}")
  # save full network
  saveRDS(
    final_ut_trim, 
    trim_path
  )
  # ggplot() +
  #   geom_sf(data = fline_groups, aes(color = section))

  
}
seperate_trees(animas_networks)
ggplot() + 
  geom_sf(data = animas_networks, aes(color = factor(huc8)))

# most downstream LHDs
start_comids <- 
  animas_lhd %>% 
  group_by(huc8) %>% 
  arrange(Hydroseq, .by_group = T) %>% 
  slice(1)

# 1400512
# Colorado River Upstream tributaries
animas_net <- nhdplusTools::navigate_network(
  start       = as.integer(1400512), 
  mode        = "UT", 
  distance_km = 200
) %>% 
  st_transform(5070) 

doParallel::stopImplicitCluster()

# Plot HUC4, network lines, and LHD points
# plot(huc_shp$geometry)
# plot(filter(huc_shp, huc4 == huc_starts$huc4[5])$geometry, col = "dodgerblue", add = T)
plot(dplyr::filter(huc_shp, huc4 == huc_starts$huc4[5])$geometry, add = F)
plot(gunn_net$geometry, col = "darkgrey", add = T)
plot(dplyr::filter(lhd_pts, huc4 == huc_starts$huc4[5])$geometry,
     col = "red", add = T)

start_lst <- list()

for (i in 1:nrow(start_comids)) {
  
  # HUC4
  huc8_txt <- start_comids$huc8[i]
  
  # Starting comid 
  start    <- as.integer(start_comids$COMID[i])
  
  logger::log_info("Retrieving COMID further downstream from:\nCOMID: {start}\nHUC8: {huc8_txt}")
  
  # Upstream tributaries
  ds_fline <- nhdplusTools::navigate_network(
    start       = start,
    # start = 3229991,
    mode        = "DM", 
    distance_km = 1
  ) %>% 
    sf::st_transform(5070)  %>% 
    dplyr::arrange(hydroseq) %>% 
    dplyr::slice(1) %>% 
    dplyr::mutate(across(where(is.numeric), as.character)) 
  
  doParallel::stopImplicitCluster()
  
  start_lst[[i]] <- ds_fline
  
}


start_points <- bind_rows(start_lst)
# i <- 2
rm(i)
# 2
# COMID: 17034989   RC: 14080102008341
# 17040463
animas_net_lst <- list()

for (i in 1:nrow(start_points)) {
  
  # HUC4
  huc8_txt <- substr(start_points$reachcode[i], 1, 8)
  
  # Starting comid 
  start    <- as.integer(start_points$comid[i])
  
  logger::log_info("Retrieving upstream tributaries from:\nCOMID: {start}\nHUC8: {huc8_txt}")
  
  # Upstream tributaries
  ut_net <- nhdplusTools::navigate_network(
    start       = start,
    # start = 3229991,
    mode        = "UT", 
    distance_km = 250
  ) %>% 
    sf::st_transform(5070) 
  
  # save full network
  saveRDS(
    ut_net,
    paste0(
      "data/spatial/networks/full/animas_network_0",
      i,
      ".rds"
    )
  )
  
  # Seperate non dendritic branches
  fline_groups <- 
    ut_net %>% 
    seperate_trees() %>% 
    dplyr::filter(section == 1)
  
  # COMIDs of main network 
  main_tree_index <- sf::st_intersection(ut_net, fline_groups)
  
  # Trim network, removing non dendritic 
  ut_trim <-
    ut_net %>% 
    dplyr::filter(comid %in% main_tree_index$comid)
  
  
  ut_trim <- 
    ut_trim %>% 
    dplyr::filter(streamorde == streamcalc) 
  
  logger::log_info("Saving full & trimmed networks to data/spatial/networks")
  
  # save trimmed network
  saveRDS(
    ut_trim,
    paste0(
      "data/spatial/networks/trim/animas_network_trim_0",
      i,
      ".rds"
    )
  )
  # plot(seperate_trees(ut_trim))
  
  # plot(dplyr::filter(huc_shp, huc4 == huc_starts$huc4[8])$geometry, add = F)
  # plot(animas_huc8$geometry, add = F)
  # plot(ut_net$geometry, col = "darkgrey", add = T)
  # plot(dplyr::filter(lhd_pts, huc4 == huc_starts$huc4[8])$geometry,
  #      col = "darkcyan", add = T)
  # mapview(ut_net2) + ut_net + animas_huc8
  
}

# **********************
# ---- South Platte ----
# **********************

# LHD Points snapped to flowlines
lhd_pts <- readRDS("data/spatial/points/lhd_flowline_points.rds") %>% 
  sf::st_transform(5070) %>% 
  dplyr::mutate(
    huc4 = substr(REACHCODE, 1, 4)
  )

# HUC4 shape
huc_shp    <- readRDS("data/spatial/shp/huc4.rds") %>% 
  dplyr::filter(huc4 %in% unique(lhd_pts$huc4))

# Starting flowline for each HUC4
huc_starts <- readRDS("data/spatial/lines/start_lines.rds")

# path to South platte NHDplus dataset
nhdplus_path <- "C:/Users/angus/OneDrive/Desktop/nhdplus_dir/subset/nhdplus_south_platte_subset.gpkg"

# South platte NHDplus subset
nhdplus <- sf::read_sf(nhdplus_path) %>% 
  sf::st_transform(5070) %>% 
  dplyr::mutate(
    huc4 = substr(REACHCODE, 1, 4),
    huc8 = substr(REACHCODE, 1, 8)
  ) 

# Single HUC4 of South Platte
sp_huc <-
  huc_shp %>%
  dplyr::filter(huc4 == 1019) %>%
  sf::st_transform(5070)

plot(huc_shp$geometry)
plot(sp_huc$geometry, col = "red", add = T)

# South Platte River Upstream tributary Network
sp_net <-
  nhdplus %>%
  # st_filter(sp_huc)
  dplyr::filter(huc4 == sp_huc$huc4[1]) %>%
  dplyr::select(-huc4) %>%
  dplyr::rename(geometry = geom) 

 # lower case names 
names(sp_net) <- tolower(names(sp_net))

# save full network
saveRDS(sp_net, "data/spatial/networks/full/sp_network.rds")

# find individual HUC8 for Animas River
sp_lhd <- 
  lhd_pts %>% 
  dplyr::filter(huc4 == 1019) %>% 
  dplyr::mutate(
    huc8 = substr(REACHCODE, 1, 8)
  )

# mapview(sp_networks, color = "red") + sp_trim 

# Non dendritic flowlines/loops that have to be manually removed in this particular case
rm_comids  <- c(3560282, 3560300, 3560308, 3560322,
                3561470, 3560356,3561746, 3561748, 
                3560352, 3561736, 13212, 3555472)

# Seperate non dendritic branches
fline_groups <- 
  sp_net %>% 
  seperate_trees() 

ggplot() + 
  geom_sf(data = fline_groups, aes(color = section))
# mapview(fline_groups)

fline_groups <- 
  fline_groups %>% 
  dplyr::filter(section == 1)

ggplot() + 
  geom_sf(data = fline_groups, aes(color = section))

# COMIDs of main network section
main_tree_index <- sf::st_intersection(sp_net, fline_groups)

# Trim network, removing non dendritic 
sp_trim <-
  sp_net %>%
  dplyr::filter(comid %in% main_tree_index$comid) %>%
  dplyr::filter(streamorde == streamcalc)

# mapview(sp_net$geometry, color = "red") + sp_trim$geometry
plot(sp_net$geometry)
plot(sp_trim$geometry, color = "red", add = T)

sp_trim_groups <-
  sp_trim %>%
  seperate_trees() %>%
  dplyr::filter(section == 1)
 # sf::st_intersection(co_trim, .)

# COMIDs of main network section
trim_tree_index <- sf::st_intersection(sp_trim, sp_trim_groups)

# Trim network, removing non dendritic and seperated forests/networks
sp_final_trim <-
  sp_trim %>% 
  dplyr::filter(comid %in% trim_tree_index$comid) %>% 
  dplyr::filter(!comid %in% rm_comids)

mapview::mapview(sp_final_trim, color = "red")

# plot
plot(sp_huc$geometry, add = F)
plot(sp_net$geometry, col = "darkgrey", add = T)
plot(filter(sp_net, !comid %in% sp_final_trim$comid), col = "red", add = T)
plot(filter(lhd_pts, huc4 == 1019)$geometry,
     col = "darkcyan", add = T)

# mapview(sp_final_trim, color = "red") + lhd_pts

# save trimmed network
saveRDS(sp_final_trim, "data/spatial/networks/trim/sp_network_trim.rds")

# *************************

# ************************
# ---- Arkansas River ----
# ************************

# LHD points
lhd_pts <- readRDS("data/spatial/points/lhd_flowline_points.rds") %>% 
  sf::st_transform(5070) %>% 
  dplyr::mutate(
    huc4 = substr(REACHCODE, 1, 4)
  )

# Starting flowline for each HUC4
huc_starts <- readRDS("data/spatial/lines/start_lines.rds")

# HUC4 shape
huc_shp    <- readRDS("data/spatial/shp/huc4.rds") %>% 
  dplyr::filter(huc4 %in% unique(lhd_pts$huc4))

# path to CO NHDplus dataset
nhdplus_path <- "C:/Users/angus/OneDrive/Desktop/nhdplus_dir/subset/nhdplus_co_subset.gpkg"

# Full Colorado NHDplus dataset
nhdplus <- sf::read_sf(nhdplus_path) %>% 
  sf::st_transform(5070) %>% 
  dplyr::mutate(
    huc4 = substr(REACHCODE, 1, 4),
    huc8 = substr(REACHCODE, 1, 8)
  ) 


# Single HUC4 of South Platte
ark_huc <-
  huc_shp %>%
  dplyr::filter(huc4 == 1102) %>%
  st_transform(5070)

plot(huc_shp$geometry)
plot(ark_huc$geometry, col = "red", add = T)

# Colorado River Upstream tributary Network
ark_net <-
  nhdplus %>%
  dplyr::filter(huc4 == ark_huc$huc4[1]) %>%
  dplyr::select(-huc4) %>%
  dplyr::rename(geometry = geom)

# lower case names 
names(ark_net) <- tolower(names(ark_net))

# mapview(ark_huc$geometry,  color = "black", col.regions = "white") + ark_net$geometry
# save full network
saveRDS(ark_net, "data/spatial/networks/full/ark_network.rds")

# Seperate non dendritic branches
fline_groups <- 
  ark_net %>% 
  seperate_trees() 

ggplot() + 
  geom_sf(data = fline_groups, aes(color = section))
# mapview(fline_groups)

fline_groups <- 
  fline_groups %>% 
  dplyr::filter(section == 1)

ggplot() + 
  geom_sf(data = fline_groups, aes(color = section))

# COMIDs of main network section
main_tree_index <- sf::st_intersection(ark_net, fline_groups)

# Trim network, removing non dendritic 
ark_trim <-
  ark_net %>% 
  dplyr::filter(comid %in% main_tree_index$comid) %>% 
  dplyr::filter(streamorde == streamcalc)

ark_trim_groups <-
  ark_trim %>% 
  seperate_trees() %>% 
  dplyr::filter(section == 1)
# sf::st_intersection(co_trim, .)

trim_tree_index <- sf::st_intersection(ark_trim, ark_trim_groups)

# Trim network, removing non dendritic 
ark_final_trim <-
  ark_trim %>% 
  dplyr::filter(comid %in% trim_tree_index$comid)

# mapview(ark_trim, color = "red") + ark_trim2

# plot
plot(ark_huc$geometry, add = F)
plot(ark_net$geometry, col = "darkgrey", add = T)
plot(dplyr::filter(ark_net, !comid %in% ark_final_trim$comid), col = "red", add = T)
plot(dplyr::filter(lhd_pts, huc4 == 1102)$geometry,
     col = "darkcyan", add = T)

# save trimmed network
saveRDS(ark_final_trim, "data/spatial/networks/trim/ark_network_trim.rds")




# *********************************************************
# *********************************************************

# ********************
# ---- Extra code ----
# ********************
# ********************
# Rio Grande 
# ********************

# # LHD Points snapped to flowlines
# lhd_pts <- readRDS("data/spatial/points/lhd_flowline_points.rds") %>% 
#   sf::st_transform(5070) %>% 
#   dplyr::mutate(
#     huc4 = substr(REACHCODE, 1, 4)
#   )
# 
# # HUC4 shape
# huc_shp    <- readRDS("data/spatial/shp/huc4.rds") %>% 
#   dplyr::filter(huc4 %in% unique(lhd_pts$huc4))
# 
# # Starting flowline for each HUC4
# huc_starts <- readRDS("data/spatial/lines/start_lines.rds")
# 
# rg_net <- nhdplusTools::navigate_network(
#   start = as.integer(17900441),
#   # start       = as.integer(huc_starts$COMID[3]), 
#   mode        = "UT", 
#   distance_km = 1000
# ) %>% 
#   st_transform(5070) 
# 
# doParallel::stopImplicitCluster()
# 
# # Plot HUC4, network lines, and LHD points
# plot(dplyr::filter(huc_shp, huc4 == huc_starts$huc4[3])$geometry)
# plot(rg_net$geometry, add = T)
# plot(dplyr::filter(lhd_pts, huc4 == huc_starts$huc4[3])$geometry,
#      col = "red", add = T)
# 
# # save full network
# saveRDS(rg_net, "data/spatial/networks/full/rg_network.rds")
# 
# # Non dendritic flowlinest to keep because several LHDs are on the branch
# # keep_flines <- filter(rg_net, gnis_name == "North Branch Conejos River")$comid
# 
# # Non dendritic flowlines/loops that have to be manually removed in this particular case
# # rm_comids <- c(17875485)
# 
# # Trim network, removing non dendritic 
# rg_trim <-
#   rg_net %>%
#   dplyr::filter(streamorde == streamcalc)
# # dplyr::filter(streamorde == streamcalc | comid %in% keep_flines) %>%
# # dplyr::filter(comid != rm_comids)
# 
# 
# # save trimmed network
# saveRDS(rg_trim, "data/spatial/networks/trim/rg_network_trim.rds")