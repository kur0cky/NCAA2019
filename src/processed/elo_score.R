
library(tidyverse)
library(elo)

reg_stats_compact <- read_csv("data/DataFiles/RegularSeasonCompactResults.csv")


tmp <- reg_stats_compact %>% 
  mutate(Score = WScore - LScore) %>% 
  select(Season, DayNum, WTeamID, LTeamID, Score) %>% 
  arrange(Season, DayNum, WTeamID, LTeamID) %>% 
  mutate(win_flg = TRUE,
         WTeamID = as.character(WTeamID),
         LTeamID = as.character(LTeamID)) %>% 
  group_by(Season) %>% 
  nest() %>% 
  mutate(elo = map(data, ~ 
                     elo.run(win_flg ~ WTeamID + LTeamID + k(32*log(abs(Score) + 1)), data = .x) %>% 
                     as.matrix() %>% 
                     as_tibble() %>% 
                     tail(1)))

tmp2 <- reg_stats_compact %>% 
  select(Season, DayNum, WTeamID, LTeamID, WScore, LScore) %>% 
  mutate(Score = WScore - LScore) %>% 
  arrange(Season, DayNum, WTeamID, LTeamID) %>% 
  mutate(win_flg = TRUE,
         WTeamID = as.character(WTeamID),
         LTeamID = as.character(LTeamID)) %>% 
  elo.run(win_flg ~ WTeamID + LTeamID + k(10*log(abs(Score) + 1)), data = .) %>% 
  as.matrix() %>% 
  as_tibble() %>% 
  bind_cols(reg_stats_compact %>% select(Season)) %>% 
  group_by(Season) %>% 
  dplyr::slice(n()) %>% 
  gather(TeamID, elo_score_all, -Season) %>% 
  ungroup() 

tmp %>% 
  select(Season, elo) %>% 
  unnest(elo) %>% 
  gather(TeamID, elo_score, -Season) %>% 
  drop_na() %>% 
  left_join(tmp2, by = c("Season", "TeamID")) %>% 
  write_csv("data/processed/elo_score.csv")

rm(tmp, tmp2);gc()