# Rachel Bash 4/12/2022
# public health data

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

# population data ----------------------

#get population data for Colorado and surrounding states by census block
pop <- get_pop("block group", "P1_001N", all_states, "2020")

#convert dataframes to sf
pop <- st_as_sf(pop) #check with st_crs(pop)
lhd <- st_as_sf(lhd, coords = c("longitude", "latitude"), crs = 4326) %>% st_transform(crs = 4269) 

#project and transform to imperial units (miles)
lhd_mi <- st_transform(lhd, "+proj=utm +zone=13 +datum=NAD83 +units=mi") 
pop_mi <- st_transform(pop, "+proj=utm +zone=13 +datum=NAD83 +units=mi")

#find area of each census block
pop_mi$pop_area <- st_area(pop_mi)

# plot to visualize
# all census blocks
# ggplot(pop_mi, aes(fill = population, color = population)) +
#   geom_sf()

# all lhds
# ggplot(lhd_mi, aes(color = structure_category)) +
#   geom_sf()


## loop through buffer sizes -------------
buffers <- c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)


for (i in buffers) {
  
  logger::log_info("create buffer with radius = {buffers[i]} mile")
  
  lhd_buffer <- st_buffer(lhd_mi, i)
  
  logger::log_info("find intersection of {buffers[i]} mile buffer and pop data")
  
  lhd_pop <- st_intersection(lhd_buffer, pop_mi)
  
  logger::log_info("calculate % of census block in each buffer zone")
  
  lhd_pop <- lhd_pop %>%
    mutate(buffer_area = st_area(lhd_pop), percent_area = buffer_area/pop_area,
           pop_included = population*percent_area)
  
  logger::log_info("sum total pop for each lhd buffer")
  
  lhd_pop <- lhd_pop %>%
    group_by(uid) %>%
    summarise(total_pop = sum(pop_included), total_pop = as.numeric(total_pop)) %>%
    mutate(buffer = i)
  
  logger::log_info("assign df with name buffer{buffers[i]}")
  assign(paste0("buffer", i), lhd_pop[lhd_pop$buffer==i,])
  
}



## normalize population -------------

#make df with buffer and pop = 0
buffer0 <- buffer1 %>% mutate(total_pop = 0, buffer = 0)

# combine all buffer dfs
lhd_pop_summary <- rbind(buffer0, buffer1, buffer5, buffer10, buffer15, buffer20,
                         buffer25, buffer30, buffer35, buffer40, buffer45, buffer50)

#normalize function
norm <- function(x){(x-min(x))/(max(x)-min(x))}

#normalize data (grouped by id & structure because there are still duplicates)
lhd_pop_summary <- lhd_pop_summary %>% 
  group_by(uid) %>%
  mutate(total_pop_norm = norm(total_pop)) %>%
  ungroup()

# visualize normalized population increase for each lhd buffer
p1 <- ggplot(lhd_pop_summary) +
  geom_line(mapping = aes(x = buffer, y = total_pop, group = factor(uid), col = uid), 
            color = "grey", size = 0.3) +
#  geom_line(subset(lhd_pop_summary, uid %in% c("343")), 
#            mapping = aes(x = buffer, y = total_pop, group = factor(uid), col = uid), 
#            color = "pink", size = 0.6) +
  labs(x = "Buffer radius (mi)", y = "Total Population")

ggsave("public_health/plots/radius_v_pop.png")

p2 <- ggplot(lhd_pop_summary) +
  geom_line(mapping = aes(x = buffer, y = total_pop_norm, group = factor(uid), col = uid), 
            color = "grey", size = 0.3) +
  #  geom_line(subset(lhd_pop_summary, uid %in% c("343")), 
  #            mapping = aes(x = buffer, y = total_pop_norm, group = factor(uid), col = uid), 
  #            color = "pink", size = 0.6) +
  labs(x = "Buffer radius (mi)", y = "Total Population Normalized")

ggsave("public_health/plots/radius_v_pop_norm.png")

 
# interpolate normalized data
buffer_interp <- lhd_pop_summary %>%
  st_drop_geometry() %>%
  group_by(uid) %>%
  summarise(buffer_interp = list(approx(total_pop_norm, buffer, xout = 0.5))) %>%
  ungroup() %>%
  mutate(p_prime = purrr::map_dbl(buffer_interp, 'x'),
         radius_0.5 = purrr::map_dbl(buffer_interp, 'y')) %>%
  select(-buffer_interp)


# join interp data with lhd_pop_summary data
lhd_pop_interp <- merge(lhd_pop_summary, buffer_interp)

saveRDS(lhd_pop_interp, "data/public_health/lhd_pop_summary.rds")


# weight radius at p'0.5 and then multiply by 1/2 of total population
weighted_pop <- lhd_pop_interp %>%
  st_drop_geometry() %>%
  filter(buffer == 50) %>%
  summarise(uid = uid, total_pop = total_pop, 
            radius_0.5 = radius_0.5, wpop = radius_0.5*total_pop/2)


# plot histogram showing weighted pop distribution
p3 <- ggplot(weighted_pop) +
  geom_histogram(aes(x = wpop), binwidth = 1000000) +
  labs(x = "Weighted pop = Total pop x radius at 0.5 pop", y = "# LHDs") +
  xlim(0,70000000)

ggsave("public_health/plots/weighted_pop_hist2.png")


# summary weighted pop
weighted_pop_sum <- weighted_pop %>%
  mutate(pct = ntile(wpop,10), pct = paste0(pct,"0%"),
         rank = rank(-wpop))

saveRDS(weighted_pop_sum, "data/public_health/weighted_pop_rank.rds")




# accident data -----------

accidents <- read.csv(paste0(drive_dir, "/data/BYU_AW_accident_data.csv")) %>%
  group_by(uid) %>%
  summarise(num_fatalities = sum(num_fatalities))

saveRDS(accidents, "data/public_health/fatalities.rds")


# fishing atlas data ----------------

# read in atlas data
atlas <- st_read(paste0(drive_dir, "/data/CO_Fishing_Atlas/AtlasPoints.shp")) %>% 
  st_as_sf() %>%
  st_transform(., crs(lhd)) %>% #match crs to lhd df
  st_zm() %>% # get rid of z dimension
  janitor::clean_names() %>%
  filter(loc_type == "Stream or River") #only view those on streams or rivers, remove ones on waterbodies


# view points in atlas data
# mapview(atlas_mi, col.regions = "red") + lhd_buffer


# convert atlas points and lhd points to projection w/ imperial units
lhd_mi <- st_transform(lhd, "+proj=utm +zone=13 +datum=NAD83 +units=mi") 
atlas_mi <- st_transform(atlas, "+proj=utm +zone=13 +datum=NAD83 +units=mi")

logger::log_info("create buffer with radius = 1 mile")
lhd_buffer <- st_buffer(lhd_mi, 1)

logger::log_info("find intersection of 1 mile buffer and atlas data")
lhd_atlas <- st_intersection(lhd_buffer, atlas_mi)

n_fishing_spots <- lhd_atlas %>%
  group_by(uid) %>%
  summarise(num_fishing_spots = n_distinct(prop_name)) %>% 
  st_drop_geometry()

lhd_atlas_counts <- lhd_atlas %>%
  group_by(uid) %>%
  summarise_at(c("stocked", "access_eas", "fish_press"), ~paste(.x, collapse ="; ")) %>%
  st_drop_geometry()

lhd_atlas_sum <- full_join(n_fishing_spots, lhd_atlas_counts)

saveRDS(lhd_atlas_sum, "data/public_health/lhd_atlas_sum.rds")
