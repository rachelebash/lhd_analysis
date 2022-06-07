# scoring impact categories - aquatic health

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
paths <- list.files(path = here::here("data/aquatic_health/"), pattern = "\\.rds", full.names = T)

for(i in 1:length(paths)) {
  names <- gsub(pattern = "\\.rds$", replacement = "", x = basename(paths[i]))
  print(names)
  assign(names, readRDS(paths[i]))
}

#combine all

all_aquatic_health <- lhd_pt %>%
  mutate(new_id = as.character(new_id)) %>%
  left_join(hci_scores, by = "new_id") %>%
  mutate(tot_hci = as.numeric(tot_hci)) %>%
  filter(structure_category != "Recreation")

#normalize function
norm <- function(x){(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))}

# create scoring
score <- all_aquatic_health %>%
  select(new_id, tot_hci) %>%
  rename(ah_tot_score = tot_hci) %>%
  st_drop_geometry() %>%
  drop_na()

# visualise scoring
summary(score)
ggplot(data = score, aes(ah_tot_score)) +
  geom_histogram(binwidth = 0.1) +
  theme(text = element_text(size = 16)) +
  labs(x = "Total Score", y = "# LHDs")

ggsave("aquatic_health/plots/score_hist.png", height = 6, width = 6)

saveRDS(score, "data/scores/aquatic_health_score.rds")

# see na scores
score_na_ah <- all_aquatic_health %>%
  select(new_id, tot_hci) %>%
  filter(is.na(tot_hci))
