# Rachel Bash 4/12/2022
# recreation data

# set up ------------------
remove(list = ls())  # clear all workspace variables
library(tidycensus)
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
library(maps)
library(gmapsdistance)
library(lwgeom)
library(ggforce)

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
  mutate(uid = row_number())

# atlas data ----------------

# read in atlas data
atlas <- st_read(paste0(drive_dir, "/data/CO_Fishing_Atlas/AtlasPoints.shp"))

# view points in atlas data
ggplot(atlas) +
  geom_sf()


# municipality pop data --------------

muni <- st_read(paste0(drive_dir, "/data/MuniBounds/MuniBounds.shp")) #crs 4326
ggplot(muni) +
  geom_sf()

# google cloud api key
# set.api.key("AIzaSyArQzKwhvLDFYK0XRL2GiZqw0U4JiLZykY")

# transform lhd to st
lhd <- st_as_sf(lhd, coords = c("longitude", "latitude"), crs = 4326)

# plot lhds and muni boundaries
ggplot() +
  geom_sf(data = muni) + geom_sf(data = lhd)


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
  select(uid, muni_dist, muni_city)



ggplot() +
  geom_sf(data = lhd_muni_join, aes(fill = muni_dist)) +
  ggforce::scale_x_unit()



# calculate google maps driving distance from each lhd to closest muni
lhd_muni_gdist <- gmapsdistance(origin = "lhd", destination = "muni_results", 
                                combinations = "pairwise", mode = "driving", key = get.api.key())
  # did not work
# Currently, in the free version the limits are given by: 
# 1. 2,500 free elements per day, calculated as the sum of client-side and server-side queries. 
# 2. Maximum of 25 origins or 25 destinations per request. 
# 3. 100 elements per request. 
# 4. 100 elements per second, calculated as the sum of client-side and server-side queries.



