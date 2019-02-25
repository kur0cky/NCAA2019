# feature_matrix
# create train data

library(tidyverse)

seed_fe <- read_csv("data/features/seed_fe.csv")
target <- read_csv("data/processed/target.csv")

target %>% 
  inner_join(seed_fe, by = "ID") %>% 
  write_csv("data/train/features.csv")
