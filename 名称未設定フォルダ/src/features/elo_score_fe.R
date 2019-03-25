library(tidyverse)

tmp <- read_csv("data/processed/elo_score.csv")
target <- read_csv("data/processed/target.csv")
sample <- read_csv("data/SampleSubmissionStage2.csv")

fe <- target %>% 
  bind_rows(sample) %>% 
  distinct(ID) %>% 
  mutate(Season = as.integer(str_sub(ID, 1, 4)),
         team1 = as.integer(str_sub(ID, 6, 9)),
         team2 = as.integer(str_sub(ID, 11, 14))) %>% 
  left_join(tmp, by = c("Season", "team1" = "TeamID")) %>% 
  left_join(tmp, by = c("Season", "team2" = "TeamID")) %>% 
  select(-Season, -team1, -team2) %>% 
  transmute(ID,
            elo_score_diff = elo_score.x - elo_score.y,
            elo_score_all_diff = elo_score_all.x - elo_score_all.y)

write_csv(fe, "data/features/elo_score_fe.csv")
rm(tmp, fe);gc()
