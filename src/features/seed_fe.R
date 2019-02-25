# feature engineering 
# seed
library(tidyverse)
seed_tmp <- read_csv("data/processed/seed.csv")
target <- read_csv("data/processed/target.csv")


seed_fe <- target %>% 
  select(-target) %>% 
  mutate(Season = as.integer(str_sub(ID, 1, 4)),
         team1 = as.integer(str_sub(ID, 6, 9)),
         team2 = as.integer(str_sub(ID, 11, 14))) %>% 
  left_join(seed_tmp, by = c("Season", "team1" = "TeamID")) %>% 
  left_join(seed_tmp, by = c("Season", "team2" = "TeamID")) %>% 
  select(-team1, -team2, -Season) %>% 
  mutate(seed_diff = seed.x - seed.y,
         seed_ratio = seed.x / seed.y)

seed_fe %>% 
  write_csv("data/features/seed_fe.csv")

rm(seed_tmp, seed_fe); gc()
