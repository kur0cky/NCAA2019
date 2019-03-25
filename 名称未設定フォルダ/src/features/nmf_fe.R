library(tidyverse)

tmp <- read_csv("data/processed/nmf.csv")
target <- read_csv("data/processed/target.csv")
sample <- read_csv("data/SampleSubmissionStage2.csv")

fe <- target %>% 
  bind_rows(sample) %>% 
  distinct(ID) %>% 
  mutate(Season = as.integer(str_sub(ID, 1, 4)),
         team1 = as.integer(str_sub(ID, 6, 9)),
         team2 = as.integer(str_sub(ID, 11, 14))) %>% 
  left_join(tmp, by = c("Season", "team1" = "team1", "team2" = "team2")) %>% 
  left_join(tmp, by = c("Season", "team1" = "team2", "team2" = "team1")) %>% 
  select(-Season, -team1, -team2) %>% 
  transmute(ID,
            nmf = rate.x - rate.y
            # massey_o_diff = massey_o.x - massey_o.y,
            # massey_d_diff = massey_d.x - massey_d.y,
  ) %>% 
  replace_na(list(nmf = 0))

write_csv(fe, "data/features/nmf_fe.csv")
rm(tmp, fe);gc()
