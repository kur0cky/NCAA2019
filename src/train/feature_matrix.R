# feature_matrix
# create train data

library(tidyverse)

seed_fe <- read_csv("data/features/seed_fe.csv")
ranking_fe <- read.csv("data/features/ranking_fe.csv", stringsAsFactors = FALSE)
reg_season_fe <- read.csv("data/features/reg_season_fe.csv", stringsAsFactors = FALSE)
RPI_fe <- read.csv("data/features/RPI_fe.csv", stringsAsFactors = FALSE)
Pyth_ratio_fe <- read.csv("data/features/Pyth_ratio_fe.csv",stringsAsFactors = FALSE)

target <- read_csv("data/processed/target.csv")


target %>% 
  # inner_join(seed_fe, by = "ID") %>%
  # inner_join(ranking_fe, by = "ID") %>%
  # inner_join(reg_season_fe, by = "ID") %>%
  inner_join(Pyth_ratio_fe, by = "ID") %>% 
  inner_join(RPI_fe, by = "ID") %>% 
  write_csv("data/train/features.csv")

  