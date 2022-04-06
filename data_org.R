# Rachel Bash 
# getting population data


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
library(maps)

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
  mutate(uid = seq.int(nrow(lhd)))

#get population data for Colorado by census block; AZ, CO, KS, NE, NM, OK, UT, WY
all_states <- c("04", "08", "20", "31", "35", "40", "49", "56")
pop <- get_pop("block group", "P1_001N", all_states, "2020")

#convert dataframes to sf
pop <- st_as_sf(pop) #check with st_crs(pop)
lhd <- st_as_sf(lhd, coords = c("longitude", "latitude"), crs = 4326) %>% st_transform(crs = 4269) 

#project and transform to imperial units (miles)
lhd_mi <- st_transform(lhd, "+proj=utm +zone=13 +datum=NAD83 +units=mi") 
pop_mi <- st_transform(pop, "+proj=utm +zone=13 +datum=NAD83 +units=mi")

#find area of each census block
pop_mi$pop_area <- st_area(pop_mi)

# #create buffer
# lhd_buffer1 <- st_buffer(lhd_mi, 1)
# lhd_buffer5 <- st_buffer(lhd_mi, 5)
# 
# #find intersection
# lhd_pop1<- st_intersection(lhd_buffer1, pop_mi) # 1 mile?
# lhd_pop5<- st_intersection(lhd_buffer5, pop_mi) #5 mile
# 
# # calculate % of census block in each buffer zone
# lhd_pop1 <- lhd_pop1 %>%
#   mutate(buffer_area = st_area(lhd_pop1), percent_area = buffer_area/pop_area,
#          pop_included = population*percent_area)
# lhd_pop5 <- lhd_pop5 %>%
#   mutate(buffer_area = st_area(lhd_pop5), percent_area = buffer_area/pop_area,
#          pop_included = population*percent_area)
# 
# lhd_pop_summary <- lhd_pop1 %>%
#   group_by(id, structure_category) %>%
#   summarise(total_pop = sum(pop_included), total_pop = as.numeric(total_pop)) %>%
#   mutate(buffer = 1)

#plot to visualize
#all census blocks
ggplot(pop_mi, aes(fill = population, color = population)) +
  geom_sf()

# all lhds
ggplot(lhd_mi, aes(color = structure_category)) +
  geom_sf()




# loop through buffer sizes ----------------------
buffers <- c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)


for (i in buffers) {
  print(i)
  
  #create buffer
  print("create buffer")
  lhd_buffer <- st_buffer(lhd_mi, i)
  
  #find intersection
  print("find intersection")
  lhd_pop <- st_intersection(lhd_buffer, pop_mi)
  
  # calculate % of census block in each buffer zone
  print("calculate % of census block in each buffer zone")
  lhd_pop <- lhd_pop %>%
    mutate(buffer_area = st_area(lhd_pop), percent_area = buffer_area/pop_area,
           pop_included = population*percent_area)
  
  # sum total pop for each lhd
  print("sum total pop for each lhd")
  lhd_pop <- lhd_pop %>%
    group_by(uid) %>%
    summarise(total_pop = sum(pop_included), total_pop = as.numeric(total_pop)) %>%
    mutate(buffer = i)
  
  # combine dataframes
  print("combine dataframes")
  assign(paste0("buffer", i), lhd_pop[lhd_pop$buffer==i,])
  
}


# test visually - total pop at each lhd by buffer size

ggplot() +
  geom_sf(buffer20, mapping = aes(fill = total_pop, color = total_pop)) +
  geom_sf(lhd_mi, mapping = aes())


# normalize population -------------

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
ggplotly(ggplot(lhd_pop_summary) +
  geom_line(mapping = aes(x = buffer, y = total_pop_norm, group = factor(uid), col = uid), 
            color = "grey", size = 0.3) + #switch to y = total_pop to see non-normalized
  geom_line(subset(lhd_pop_summary, uid %in% c("343")), 
            mapping = aes(x = buffer, y = total_pop_norm, group = factor(uid), col = uid), 
            color = "pink", size = 0.6) +
  labs(x = "Buffer radius (mi)", y = "Total Population Normalized"))
 
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

# interpolate to population
pop_interp <- lhd_pop_interp %>%
  st_drop_geometry() %>%
  group_by(uid) %>%
  summarise(pop_interp = list(approx(buffer, total_pop, xout = radius_0.5))) %>%
  ungroup() %>%
  mutate(radius_0.5 = purrr::map_dbl(pop_interp, 'x'),
         total_pop_0.5 = purrr::map_dbl(pop_interp, 'y')) %>%
  select(-pop_interp)










# still to do
# 1. fix pop function to include state as a column
# 2. interpolate
# 3. weight by # pop within radius
# 4. histogram of final value for each lhd

# choose only first one of the list in pop_interp since there are currently 12
