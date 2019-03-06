
library(tidyverse)
library(elo)

reg_stats_compact <- read_csv("data/DataFiles/RegularSeasonCompactResults.csv")
tourney_stats_compact <- read_csv("data/datafiles/NCAATourneyCompactResults.csv")
second_stats_compact <- read_csv("data/datafiles/SecondaryTourneyCompactResults.csv")

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

tmp2 <- bind_rows(reg_stats_compact,
                  tourney_stats_compact,
                  second_stats_compact) %>% 
  select(Season, DayNum, WTeamID, LTeamID) %>% 
  arrange(Season, DayNum, WTeamID, LTeamID) %>% 
  mutate(win_flg = TRUE,
         WTeamID = as.character(WTeamID),
         LTeamID = as.character(LTeamID)) 
tmp3 <- tmp2 %>% 
  elo.run(win_flg ~ WTeamID + LTeamID, data = ., k = 10) %>% 
  as.matrix() %>% 
  as_tibble() %>% 
  bind_cols(tmp2 %>% select(Season, DayNum)) %>% 
  filter(DayNum < 134) %>% 
  group_by(Season) %>% 
  dplyr::slice(n()) %>% 
  gather(TeamID, elo_all, -Season) %>% 
  ungroup() 

tmp %>% 
  select(Season, elo) %>% 
  unnest(elo) %>% 
  gather(TeamID, elo_r, -Season) %>% 
  drop_na() %>% 
  left_join(tmp3, by = c("Season", "TeamID")) %>% 
  write_csv("data/processed/elo.csv")

rm(tmp, tmp2, tmp3);gc()

