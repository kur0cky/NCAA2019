library(tidyverse)

tmp <- read_csv("data/processed/elo.csv")
target <- read_csv("data/processed/target.csv")

fe <- target %>% 
  select(-target) %>% 
  mutate(Season = as.integer(str_sub(ID, 1, 4)),
         team1 = as.integer(str_sub(ID, 6, 9)),
         team2 = as.integer(str_sub(ID, 11, 14))) %>% 
  left_join(tmp, by = c("Season", "team1" = "TeamID")) %>% 
  left_join(tmp, by = c("Season", "team2" = "TeamID")) %>% 
  select(-Season, -team1, -team2) %>% 
  transmute(ID,
            elo_r_diff = elo_r.x - elo_r.y,
            elo_all_diff = elo_all.x - elo_all.y)

write_csv(fe, "data/features/elo_fe.csv")
rm(tmp, fe);gc()
