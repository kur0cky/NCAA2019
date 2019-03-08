# Pythagorean expectation
library(tidyverse)

Pyth_ratio <- read_csv("data/processed/Pyth_ratio.csv")
target <- read_csv("data/processed/target.csv")

sample <- read_csv("data/SampleSubmissionStage1.csv")

fe <- target %>% 
  bind_rows(sample) %>% 
  distinct(ID) %>% 
  mutate(Season = as.integer(str_sub(ID, 1, 4)),
         team1 = as.integer(str_sub(ID, 6, 9)),
         team2 = as.integer(str_sub(ID, 11, 14))) %>% 
  left_join(Pyth_ratio, by = c("Season", "team1" = "TeamID")) %>% 
  left_join(Pyth_ratio, by = c("Season", "team2" = "TeamID")) %>% 
  select(-Season, -team1, -team2) %>% 
  transmute(ID,
            # Pyth_ratio_rate = Pyth_ratio.x / Pyth_ratio.y,
            Pyth_ratio_diff = Pyth_ratio.x - Pyth_ratio.y)

fe %>% 
  write_csv("data/features/Pyth_ratio_fe.csv")
rm(fe, Pyth_ratio); gc()
