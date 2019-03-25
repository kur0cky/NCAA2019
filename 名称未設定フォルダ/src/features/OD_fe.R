library(tidyverse)

tmp<- read_csv("data/processed/od.csv")
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
            # o_diff = o.x - o.y,
            # d_diff = d.x - d.y,
            od_diff = od_rate.x - od_rate.y
            )

write_csv(fe, "data/features/od_fe.csv")
rm(tmp, fe);gc()
