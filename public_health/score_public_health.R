# scoring impact categories

# Rachel Bash 4/20/22

# set up ------------------
remove(list = ls())  # clear all workspace variables

library(tidyverse)
library(sf)
library(sp)
library(rgeos)
library(raster)
library(plotly)
library(zoo)
library(logger)
library(USAboundaries)
library(mapview)


#utility functions
source("utils/find_google_drive.R")
source("utils/data_utils.R")

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

# read in all data for public health
paths <- list.files(path = here::here("data/public_health/"), pattern = "\\.rds", full.names = T)

for(i in 1:length(paths)) {
  names <- gsub(pattern = "\\.rds$", replacement = "", x = basename(paths[i]))
  print(names)
  assign(names, readRDS(paths[i]))
}

# join all public health categories to lhd list
all_public_health <- lhd_pt %>% 
  left_join(fatalities, by = c("new_id" = "uid")) %>%
  left_join(lhd_atlas_sum, by = c("new_id" = "uid")) %>%
  left_join(weighted_pop_rank, by = c("new_id" = "uid")) %>%
  filter(structure_category != "Recreation") # take out recreation dams

#normalize function
norm <- function(x){(x-min(x))/(max(x)-min(x))}

# create score

score_all <- all_public_health %>%
  mutate(fatal_score = ifelse(is.na(num_fatalities),0,num_fatalities),
         fishing_score = ifelse(is.na(num_fishing_spots),0,1),
         pop_score = 1-norm(rank),
         ph_tot_score = fatal_score + fishing_score + pop_score)

score <- score_all %>%
  select(new_id, ph_tot_score)

score_top10 <- score_all %>%
  arrange(desc(ph_tot_score)) %>%
  slice(1:10) %>%
  select(new_id, num_fatalities, structure_category, rank, num_fishing_spots, ph_tot_score)

write.csv(score_top10, "data/scores/public_health_score_top10.csv", row.names = FALSE)
  

# visualise scoring
summary(score)
ggplot(data = score, aes(ph_tot_score)) +
  geom_histogram(binwidth = 0.1) +
  scale_x_continuous(breaks = seq(0,15,1)) +
  theme(text = element_text(size = 16)) +
  labs(x = "Total Score", y = "# LHDs")
ggsave("public_health/plots/score_hist.png", height = 6, width = 6)

ggplot(data = score_all, aes(x = structure_category, y = ph_tot_score)) +
  geom_jitter(aes(x = structure_category), alpha = 0.2, position = position_jitter(0.2), size = 1.8) +
  scale_y_continuous(breaks = seq(0,15,1))  +
  theme(text = element_text(size = 16)) +
  labs(x = "Structure Category", y = "Total Score")
  
ggsave("public_health/plots/score_boxplot.png", height = 6, width = 6)
  

  
saveRDS(score, "data/scores/public_health_score.rds")


