# Rachel Bash 4/12/2022
# recreation data

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
  select(uid, everything()) %>%
  st_as_sf(., coords = c("longitude", "latitude"), crs = 4326)

# Convert LHD dataframe to spatial points  using sf package
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




# read in state border
CO <- USAboundaries::us_states(states = "CO")


# atlas data ----------------

# read in atlas data
atlas <- st_read(paste0(drive_dir, "/data/CO_Fishing_Atlas/AtlasPoints.shp")) %>% 
  st_as_sf() %>%
  st_transform(., crs(lhd)) %>% #match crs to lhd df
  st_zm() %>% # get rid of z dimension
  janitor::clean_names() %>%
  filter(loc_type == "Stream or River") #only view those on streams or rivers, remove ones on waterbodies


# view points in atlas data
p1 <- ggplot() +
  geom_sf(data = atlas, color = "red") +
  geom_sf(data = CO, fill = NA) +
  geom_sf(data = lhd)
p1
ggsave("recreation/plots/fishing_atlas.png")




# convert atlas points and lhd points to projection w/ imperial units
lhd_mi <- st_transform(lhd, "+proj=utm +zone=13 +datum=NAD83 +units=mi") 
atlas_mi <- st_transform(atlas, "+proj=utm +zone=13 +datum=NAD83 +units=mi")

logger::log_info("create buffer with radius = 1 mile")
lhd_buffer <- st_buffer(lhd_mi, 1)

logger::log_info("find intersection of 1 mile buffer and atlas data")
lhd_atlas <- st_intersection(lhd_buffer, atlas_mi)

mapview(atlas_mi, col.regions = "red") + lhd_buffer

n_fishing_spots <- lhd_atlas %>%
  group_by(uid) %>%
  summarise(num_fishing_spots = n_distinct(prop_name)) %>% 
  st_drop_geometry()

lhd_atlas_counts <- lhd_atlas %>%
  group_by(uid) %>%
  summarise_at(c("stocked", "access_eas", "fish_press"), ~paste(.x, collapse ="; ")) %>%
  st_drop_geometry()

lhd_atlas_sum <- full_join(n_fishing_spots, lhd_atlas_counts)

saveRDS(lhd_atlas_sum, "data/recreation/lhd_atlas_sum.rds")


# municipality pop data --------------

muni <- st_read(paste0(drive_dir, "/data/MuniBounds/MuniBounds.shp")) #crs 4326
ggplot(muni) +
  geom_sf()


# check to see if crs are equal
all.equal(st_crs(lhd),st_crs(muni))
sf::sf_use_s2(FALSE) # in order to get next function to work

# calculate distance from every lhd to nearest muni boundary - output is distance is meters to each muni
lhd_muni <- st_distance(lhd, st_union(muni))

# find closest muni to each lhd - output is id of each muni
results <- st_nearest_feature(lhd, muni)

# dataframe of closest muni to every lhd in order of lhd uid
muni_results <- left_join(data.frame(id = results),muni,by = "id") %>% 
  select(-geometry)

# df of every lhd and the distance, id, and name of the muni that is closest
lhd_muni_join <- lhd %>%
  mutate(muni_id = results, muni_dist = lhd_muni, muni_city = muni_results$first_city) %>%
  select(uid, muni_dist, muni_city) %>%
  drop_units()


# plot of lhds and colored by distance to muni
p2 <- ggplot() +
  geom_sf(data = muni) +
  geom_sf(data = lhd_muni_join, aes(color = muni_dist)) + 
  geom_sf(data = CO, fill = NA)
ggsave("recreation/plots/muni_dist_from_lhd.png")
  

# save output
lhd_muni_join %>% st_drop_geometry() %>% saveRDS(., "data/recreation/muni_dist.rds")


# aw reaches -------------

#read in aw reaches
aw <- st_read(paste0(drive_dir, "/data/aw_reach_segments/co_reach_segments.shp")) %>%
  st_transform(5070) %>%
  mutate(lengths = st_length(geometry))

# create buffer around lhds of 500 meters
lhd_buff <- st_buffer(lhd_pt, 500)


# find lhds on aw reaches within 500 meters of lhds
aw_lhd <- st_intersection(lhd_buff, aw)

# map of lhd buffers and aw reaches
mapview(aw_lhd, col.regions = "red", col = "red") + mapview(aw, col.regions = "blue") +
  mapview(lhd_buff, col.regions = "green", col = "green")

# save gold_lhd data
save_aw <- aw_lhd %>% 
  select(new_id) %>%
  mutate(aw = 1) %>%
  st_drop_geometry() %>%
  distinct() %>%
  right_join(., lhd_pt, by = "new_id") %>%
  mutate(aw = if_else(is.na(aw), 0, aw))

saveRDS(save_aw, "data/recreation/aw_reaches.rds")



# gold medal reaches -------------
gold <- st_read(paste0(drive_dir, "/data/CPW_GoldMedalWaters/GoldMedalStreams02142022.shp")) %>%
  st_transform(5070) %>%
  mutate(lengths = st_length(geometry))



# find lhds on gold medal waters within 500 meters of lhds
gold_lhd <- st_intersection(lhd_buff, gold)


# map of lhd buffers and gold medal waters
mapview(gold_lhd, col.regions = "red", col = "red") + mapview(gold, col.regions = "blue") +
  mapview(lhd_buff, col.regions = "green", col = "green")



# save gold_lhd data
save_gold <- gold_lhd %>% 
  select(new_id) %>%
  mutate(gold = 1) %>%
  st_drop_geometry() %>%
  distinct() %>%
  right_join(., lhd_pt, by = "new_id") %>%
  mutate(gold = if_else(is.na(gold), 0, gold))


saveRDS(save_gold, "data/recreation/gold_reaches.rds")

