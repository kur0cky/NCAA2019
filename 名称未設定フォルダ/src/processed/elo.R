
library(tidyverse)
library(elo)
set.seed(1)

reg_stats_compact <- read_csv("data/Stage2DataFiles/RegularSeasonCompactResults.csv")

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

tmp2 <- bind_rows(reg_stats_compact) %>% 
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
tmp4 <- reg_stats_compact %>% 
  select(Season, DayNum, WTeamID, LTeamID) %>% 
  arrange(Season, DayNum, WTeamID, LTeamID) %>% 
  mutate(win_flg = TRUE,
         WTeamID = as.character(WTeamID),
         LTeamID = as.character(LTeamID)) %>% 
  group_by(Season) %>% 
  nest() %>% 
  mutate(lag = lag(data, 1),
         lag2 = lag(data, 2),
         data = map2(data, lag, ~ rbind(.y, .x)),
         data = map2(data, lag2, ~ rbind(.y, .x))) %>% 
  select(-lag, -lag2) %>% 
  mutate(elo3 = map(data, ~ 
                     elo.run(win_flg ~ WTeamID + LTeamID,
                             data = .x,
                             k = 32) %>% 
                     as.matrix() %>% 
                     as_tibble() %>% 
                     tail(1))) %>% 
  select(Season, elo3) %>% 
  unnest(elo3) %>% 
  gather(TeamID, elo_3r, -Season) %>% 
  drop_na()

tmp %>% 
  select(Season, elo) %>% 
  unnest(elo) %>% 
  gather(TeamID, elo_r, -Season) %>% 
  drop_na() %>% 
  left_join(tmp3, by = c("Season", "TeamID")) %>% 
  left_join(tmp4, by = c("Season", "TeamID")) %>% 
  write_csv("data/processed/elo.csv")

rm(tmp, tmp2, tmp3);gc()

