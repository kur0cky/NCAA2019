library(tidyverse)

RPI <- read_csv("data/processed/RPI.csv")
target <- read_csv("data/processed/target.csv")
sample <- read_csv("data/SampleSubmissionStage2.csv")

fe <- target %>% 
  bind_rows(sample) %>% 
  distinct(ID) %>% 
  mutate(Season = as.integer(str_sub(ID, 1, 4)),
         team1 = as.integer(str_sub(ID, 6, 9)),
         team2 = as.integer(str_sub(ID, 11, 14))) %>% 
  left_join(RPI, by = c("Season", "team1" = "TeamID")) %>% 
  left_join(RPI, by = c("Season", "team2" = "TeamID")) %>% 
  select(-Season, -team1, -team2) %>% 
  transmute(ID, 
            # WP_diff = WP.x - WP.y,
            # OWP_diff = OWP.x - OWP.y,
            # OOWP_diff = OOWP.x - OOWP.y,
            RPI_diff = RPI.x - RPI.y)


fe %>% 
  write_csv("data/features/RPI_fe.csv")
rm(RPI, fe);gc()
