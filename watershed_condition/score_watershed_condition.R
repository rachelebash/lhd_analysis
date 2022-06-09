# scoring impact categories - watershed condition

# Rachel Bash 6/1/2022

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

# get watershed condition datasets
connect <- readRDS("data/spatial/networks/connectivity/lhd_network_connectivity.rds")
network_channel_alt_pct <- readRDS("data/channel_alterations/network_channel_alt_pct.rds")
catchment_landcover <- readRDS("data/landcover/catchment_landcover.rds")
network_complexity <- readRDS("data/network_complexity/network_complexity.rds")

#combine all

all_watershed_cond <- lhd_pt %>%
  mutate(new_id = as.character(new_id)) %>%
  left_join(catchment_landcover, by = "new_id") %>%
  left_join(network_channel_alt_pct, by = "new_id") %>%
  left_join(network_complexity, by = "new_id") %>%
  left_join(connect, by = "new_id") %>%
  filter(structure_category != "Recreation")


#normalize function
norm <- function(x){(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))}

# create scoring

score <- all_watershed_cond %>%
  mutate(lc_rank = dense_rank(desc(total_nat_lc_pct)),
         complex_rank = dense_rank(desc(us_size_class)),
         alt_rank = dense_rank(desc(us_channel_pct_unaltered)),
         connect_rank = dense_rank(desc(pmin(us_length, ds_length))),
         lc_score = norm(total_nat_lc_pct),
         complex_score = norm(us_size_class),
         alt_score = norm(us_channel_pct_unaltered),
         connect_score = norm(pmin(us_length, ds_length)),
         wc_tot_score = lc_score + complex_score + alt_score + connect_score,
         wc_tot_score = round(wc_tot_score, 3)) %>%
  select(new_id, lc_rank, lc_score, complex_rank, complex_score, alt_rank, alt_score, connect_rank, 
         connect_score, wc_tot_score) %>%
  st_drop_geometry()


score_top10 <- score %>%
  arrange(desc(wc_tot_score)) %>%
  slice(1:10) %>%
  select(new_id, connect_score, complex_score, alt_score, lc_score, wc_tot_score)

write.csv(score_top10, "data/scores/watershed_condition_score_top10.csv", row.names = FALSE)

# visualise scoring
summary(score)
ggplot(data = score, aes(wc_tot_score)) +
  geom_histogram(binwidth = 0.1) +
  theme(text = element_text(size = 16)) +
  labs(x = "Total Score", y = "# LHDs")

ggsave("watershed_condition/plots/score_hist.png", height = 6, width = 6)

saveRDS(score, "data/scores/watershed_condition_score.rds")

