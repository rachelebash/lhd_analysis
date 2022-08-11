# collate all data and deliver to AW


# 6/8/2022

# set up ------------------
remove(list = ls())  # clear all workspace variables

library(tidyverse)
library(tigris)
library(sf)
library(sp)
library(rgeos)
library(raster)
library(plotly)=
library(units)
library(zoo)
library(logger)
library(gmapsdistance)
library(USAboundaries)
library(mapview)
library(nhdplusTools)
library(nngeo)
library(viridis)

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
  mutate(new_id = 1:dplyr::n()) %>%
  relocate(new_id)

# read in all scores
paths <- list.files(path = here::here("data/scores/"), pattern = "\\.rds", full.names = T)

for(i in 1:length(paths)) {
  names <- gsub(pattern = "\\.rds$", replacement = "", x = basename(paths[i]))
  print(names)
  assign(names, readRDS(paths[i]))
}

recreation_score <- recreation_score %>%
  mutate(new_id = as.character(new_id))


public_health_score <- public_health_score %>%
  mutate(new_id = as.character(new_id))

#normalize function
norm <- function(x){(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))}

#combine all

all_scores <- lhd %>%
  mutate(new_id = as.character(new_id)) %>%
  left_join(aquatic_health_score, by = "new_id") %>%
  left_join(public_health_score, by = "new_id") %>%
  left_join(recreation_score, by = "new_id") %>%
  left_join(watershed_condition_score, by = "new_id") %>%
  mutate(ah_tot_score = norm(ah_tot_score),
         ph_tot_score = norm(ph_tot_score),
         rec_tot_score = norm(rec_tot_score),
         wc_tot_score = norm(wc_tot_score)) %>%
  select(new_id:longitude, ah_tot_score, ph_tot_score, rec_tot_score, wc_tot_score)

#write.csv(all_scores, "data/scores/scores_for_spencer.csv", row.names = FALSE)

write.csv(all_scores, "data/scores/lhd_scores_8.9.22.csv")

