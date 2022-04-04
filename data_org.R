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

#utility functions
source("utils/find_google_drive.R")
source("utils/data_utils.R")

# get rid of scientific notation
options(scipen = 1000)

# read in LHD data
lhd <- read.csv(paste0(drive_dir, "/data/Low Head Dam Inventory Final CIM 092920 - Inventory.csv"))

#get population data for Colorado by census block
pop <- get_pop("block group", "P1_001N", "08", "2020")

#convert dataframes to sf
pop <- st_as_sf(pop) #check with st_crs(pop)
lhd <- st_as_sf(lhd, coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>% st_transform(crs = 4269) 

#project and transform to imperial units (miles)
lhd_mi <- st_transform(lhd, "+proj=utm +zone=13 +datum=NAD83 +units=mi") # I don't think this worked how I wanted it to
pop_mi <- st_transform(pop, "+proj=utm +zone=13 +datum=NAD83 +units=mi")

#find area of each census block
pop_mi$pop_area <- st_area(pop_mi)

#create buffer
lhd_buffer1 <- st_buffer(lhd_mi, 1)
lhd_buffer5 <- st_buffer(lhd_mi, 5)

#find intersection
lhd_pop1<- st_intersection(lhd_buffer1, pop_mi) # 1 mile?
lhd_pop5<- st_intersection(lhd_buffer5, pop_mi) #5 mile

# calculate % of census block in each buffer zone
lhd_pop1 <- lhd_pop1 %>%
  mutate(buffer_area = st_area(lhd_pop1), percent_area = buffer_area/pop_area,
         pop_included = population*percent_area)

lhd_pop_summary <- lhd_pop1 %>%
  group_by(ID, STRUCTURE.CATEGORY) %>%
  summarise(total_pop = sum(pop_included)) %>%
  mutate(buffer = 1)

#plot to visualize
#all census blocks
ggplot(pop_mi, aes(fill = population, color = population)) +
  geom_sf()

# all lhds
ggplot(lhd_mi, aes(color = STRUCTURE.CATEGORY)) +
  geom_sf()

#intersection of lhd buffer and population
ggplot(lhd_pop1, aes(color = population, fill = population)) +
  geom_sf() 

# next steps 
# 1. make into loop to include multiple buffer sizes
# 2. clean up column names
