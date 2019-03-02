library(tidyverse)

Pyth_ratio <- read_csv("data/processed/Pyth_ratio.csv")
target <- read_csv("data/processed/target.csv")

Pyth_ratio_fe <- target %>% 
  select(-target) %>% 
  mutate(Season = as.integer(str_sub(ID, 1, 4)),
         team1 = as.integer(str_sub(ID, 6, 9)),
         team2 = as.integer(str_sub(ID, 11, 14))) %>% 
  left_join(Pyth_ratio, by = c("Season", "team1" = "TeamID")) %>% 
  left_join(Pyth_ratio, by = c("Season", "team2" = "TeamID")) %>% 
  select(-Season, -team1, -team2) %>% 
  mutate(Pyth_ratio_rate = Pyth_ratio.x / Pyth_ratio.y)

Pyth_ratio_fe %>% 
  write_csv("data/features/Pyth_ratio_fe.csv")
rm(Pyth_ratio_fe, Pyth_ratio); gc()
