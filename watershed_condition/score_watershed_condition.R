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


# read in all data for watershed condition
paths <- list.files(path = here::here("data/watershed_condition/"), pattern = "\\.rds", full.names = T)

for(i in 1:length(paths)) {
  names <- gsub(pattern = "\\.rds$", replacement = "", x = basename(paths[i]))
  print(names)
  assign(names, readRDS(paths[i]))
}

#combine all

all_watershed_cond <- lhd_pt %>%
  mutate(new_id = as.character(new_id)) %>%
  left_join(catchment_landcover, by = "new_id") %>%
  left_join(network_channel_alt_pct, by = "new_id") %>%
  left_join(network_complexity, by = "new_id") %>%
  filter(structure_category != "Recreation")

#normalize function
norm <- function(x){(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))}

# create scoring

score <- all_watershed_cond %>%
  mutate(lc_rank = dense_rank(desc(total_nat_lc_pct)),
         complex_rank = dense_rank(desc(total_size_class)),
         alt_rank = dense_rank(desc(us_channel_pct_unaltered)),
         lc_score = norm(total_nat_lc_pct),
         complex_score = norm(total_size_class),
         alt_score = norm(us_channel_pct_unaltered),
         wc_tot_score = lc_score + complex_score + alt_score) %>%
  select(new_id, lc_rank, lc_score, complex_rank, complex_score, alt_rank, alt_score, wc_tot_score) %>%
  st_drop_geometry()


score_na <- score %>%
  filter(is.na(lc_score))

# visualise scoring
summary(score)
ggplot(data = score, aes(wc_tot_score)) +
  geom_histogram(binwidth = 0.1) +
  theme(text = element_text(size = 16)) +
  labs(x = "Total Score", y = "# LHDs")

ggsave("watershed_condition/plots/score_hist.png", height = 6, width = 6)

saveRDS(score, "data/scores/watershed_condition_score.rds")
# save this once Angus pushes his stuff

test <- network_complexity %>%
  filter(!new_id %in% score_na$new_id)

test1 <- network_complexity %>%
  filter(new_id %in% test$new_id)
