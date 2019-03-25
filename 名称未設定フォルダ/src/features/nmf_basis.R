library(tidyverse)

tmp <- read_csv("data/processed/basis_nmf.csv")
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
  replace_na(list(rate_nmf.x = 0, rate_nmf.y = 0)) %>% 
  select(-Season, -team1, -team2) %>% 
  transmute(ID,
            basis_nmf_diff = basis.x - basis.y) 

write_csv(fe, "data/features/basis_nmf_fe.csv")
rm(tmp, fe);gc()