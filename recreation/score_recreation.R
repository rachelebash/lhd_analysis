# scoring impact categories - recreation

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


# read in all data for watershed condition
paths <- list.files(path = here::here("data/recreation/"), pattern = "\\.rds", full.names = T)

for(i in 1:length(paths)) {
  names <- gsub(pattern = "\\.rds$", replacement = "", x = basename(paths[i]))
  print(names)
  assign(names, readRDS(paths[i]))
}

connect <- readRDS("data/spatial/networks/connectivity/lhd_network_connectivity.rds") %>%
  mutate(new_id = as.integer(new_id))

#combine all

all_recreation <- lhd_pt %>%
  left_join(aw_reaches, by = "new_id") %>%
  left_join(gold_reaches, by = "new_id") %>%
  left_join(muni_dist, by = c("new_id" = "uid")) %>%
  left_join(lhd_atlas_sum, by = c("new_id" = "uid")) %>%
  left_join(connect, by = "new_id") %>%
  filter(structure_category != "Recreation")

#normalize function
norm <- function(x){(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))}

# create scoring
score <- all_recreation %>%
  mutate(connect_score = norm(pmin(us_length, ds_length)),
         connect_rank = dense_rank(desc(pmin(us_length, ds_length))),
         muni_score = ifelse(muni_dist == 0,1,1-norm(muni_dist)),
         aw_score = aw,
         gold_score = gold,
         rec_tot_score = connect_score + muni_score + aw_score + gold_score) %>%
  select(new_id, connect_score, muni_score, aw_score, gold_score, rec_tot_score) %>%
  st_drop_geometry() %>%
  janitor::clean_names()

# top 10
score_top10 <- score %>%
  arrange(desc(rec_tot_score)) %>%
  slice(1:10)

write.csv(score_top10, "data/scores/recreation_score_top10.csv", row.names = FALSE)

# visualise scoring
summary(score)
ggplot(data = score, aes(rec_tot_score)) +
  geom_histogram(binwidth = 0.1) +
  theme(text = element_text(size = 16)) +
  labs(x = "Total Score", y = "# LHDs")

ggsave("recreation/plots/score_hist.png", height = 6, width = 6)

saveRDS(score, "data/scores/recreation_score.rds")
