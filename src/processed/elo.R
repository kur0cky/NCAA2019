
library(tidyverse)
library(elo)

reg_stats_compact <- read_csv("data/DataFiles/RegularSeasonCompactResults.csv")

tmp <- reg_stats_compact %>% 
  select(Season, DayNum, WTeamID, LTeamID) %>% 
  arrange(Season, DayNum, WTeamID, LTeamID) %>% 
  mutate(win_flg = TRUE,
         WTeamID = as.character(WTeamID),
         LTeamID = as.character(LTeamID)) %>% 
  group_by(Season) %>% 
  nest() %>% 
  mutate(elo = map(data, ~ 
                     elo.run(win_flg ~ WTeamID + LTeamID,
                             data = .x,
                             k = 32) %>% 
                     as.matrix() %>% 
                     as_tibble() %>% 
                     tail(1)))

tmp2 <- reg_stats_compact %>% 
  select(Season, DayNum, WTeamID, LTeamID) %>% 
  arrange(Season, DayNum, WTeamID, LTeamID) %>% 
  mutate(win_flg = TRUE,
         WTeamID = as.character(WTeamID),
         LTeamID = as.character(LTeamID)) %>% 
  elo.run(win_flg ~ WTeamID + LTeamID, data = ., k = 10) %>% 
  as.matrix() %>% 
  as_tibble() %>% 
  bind_cols(reg_stats_compact %>% select(Season)) %>% 
  group_by(Season) %>% 
  dplyr::slice(n()) %>% 
  gather(TeamID, elo_all, -Season) %>% 
  ungroup() 

tmp %>% 
  select(Season, elo) %>% 
  unnest(elo) %>% 
  gather(TeamID, elo_r, -Season) %>% 
  drop_na() %>% 
  left_join(tmp2, by = c("Season", "TeamID")) %>% 
  write_csv("data/processed/elo.csv")
