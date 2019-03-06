library(tidyverse)

tmp <- read_csv("data/processed/massey_conf.csv")
teamconf <-read_csv("data/datafiles/TeamConferences.csv")
target <- read_csv("data/processed/target.csv")

fe <- target %>% 
  select(-target) %>% 
  mutate(Season = as.integer(str_sub(ID, 1, 4)),
         team1 = as.integer(str_sub(ID, 6, 9)),
         team2 = as.integer(str_sub(ID, 11, 14))) %>% 
  left_join(teamconf, by = c("Season", "team1" = "TeamID")) %>% 
  left_join(teamconf, by = c("Season", "team2" = "TeamID")) %>% 
  left_join(tmp, by = c("Season", "ConfAbbrev.x" = "ConfAbbrev")) %>% 
  left_join(tmp, by = c("Season", "ConfAbbrev.y" = "ConfAbbrev")) %>% 
  select(-Season, -team1, -team2) %>% 
  transmute(ID,
            massey_conf_r_diff = massey_conf_r.x - massey_conf_r.y)

write_csv(fe, "data/features/massey_conf_fe.csv")
rm(tmp, fe);gc()
