# feature engineering 
# seed
library(tidyverse)
seed_tmp <- read_csv("data/processed/seed.csv")
target <- read_csv("data/processed/target.csv")
sample <- read_csv("data/SampleSubmissionStage1.csv")

fe <- target %>% 
  bind_rows(sample) %>% 
  distinct(ID) %>% 
  mutate(Season = as.integer(str_sub(ID, 1, 4)),
         team1 = as.integer(str_sub(ID, 6, 9)),
         team2 = as.integer(str_sub(ID, 11, 14))) %>% 
  left_join(seed_tmp, by = c("Season", "team1" = "TeamID")) %>% 
  left_join(seed_tmp, by = c("Season", "team2" = "TeamID")) %>% 
  select(-team1, -team2, -Season) %>% 
  transmute(ID,
            seed_diff = seed.x - seed.y,
            log_seed_diff = log(seed.x) - log(seed.y))

fe %>% 
  write_csv("data/features/seed_fe.csv")

rm(seed_tmp, fe); gc()
