library(tidyverse)

reg_stats <- read_csv("data/DataFiles/RegularSeasonDetailedResults.csv")
reg_stats_compact <- read_csv("data/DataFiles/RegularSeasonCompactResults.csv")
target <- read_csv("data/processed/target.csv")

tmp <- reg_stats_compact %>% 
  mutate(ScoreRate = WScore / (WScore+LScore)) %>% 
  select(Season, WTeamID, LTeamID, ScoreRate) %>% 
  gather(key, TeamID, -Season, -ScoreRate) %>% 
  mutate(ScoreRate = if_else(key == "WTeamID", ScoreRate, 1-ScoreRate)) %>% 
  group_by(Season, TeamID) %>% 
  summarise(avg_ScoreRate = mean(ScoreRate)) %>% 
  ungroup()

tmp2 <- reg_stats %>%
  mutate(WPoss = WFGA + (WFTA * 0.47) + WTO - WOR,
         LPoss = LFGA + (LFTA * 0.47) + LTO - LOR) %>% 
  transmute(Season, WTeamID, LTeamID, 
            WPPP = WScore / WPoss,
            LPPP = LScore / LPoss) %>% 
  gather(key, TeamID, -Season, -WPPP, -LPPP) %>% 
  transmute(Season, TeamID, 
            PPP = if_else(key == "WTeamID", WPPP, LPPP)) %>% 
  group_by(Season, TeamID) %>% 
  summarise(avg_PPP = mean(PPP)) %>% 
  ungroup()
  
tmp3 <- reg_stats %>% 
  transmute(Season, WTeamID, LTeamID,
            WeFG = (WFGM + 0.5*WFGM3)/(WFGA + WFGA3*0.5),
            LeFG = (LFGM + 0.5*LFGM3)/(LFGA + LFGA3*0.5)) %>% 
  gather(key, TeamID, -Season, -WeFG, -LeFG) %>% 
  transmute(Season, TeamID, 
            eFG = if_else(key == "WTeamID", WeFG, LeFG)) %>% 
  group_by(Season, TeamID) %>% 
  summarise(avg_eFG = mean(eFG)) %>% 
  ungroup()


reg_season_fe <- target %>% 
  select(-target) %>% 
  mutate(Season = as.integer(str_sub(ID, 1, 4)),
         team1 = as.integer(str_sub(ID, 6, 9)),
         team2 = as.integer(str_sub(ID, 11, 14))) %>% 
  left_join(tmp, by = c("Season", "team1" = "TeamID")) %>% 
  left_join(tmp, by = c("Season", "team2" = "TeamID")) %>% 
  left_join(tmp2, by = c("Season", "team1" = "TeamID")) %>% 
  left_join(tmp2, by = c("Season", "team2" = "TeamID")) %>% 
  left_join(tmp3, by = c("Season", "team1" = "TeamID")) %>% 
  left_join(tmp3, by = c("Season", "team2" = "TeamID")) %>% 
  select(-Season, -team1, -team2) %>% 
  transmute(ID, 
            avg_ScoreRate_diff = avg_ScoreRate.x - avg_ScoreRate.y,
            avg_PPP_diff = avg_PPP.x - avg_PPP.y,
            avg_eFG_diff = avg_eFG.x - avg_eFG.y)

reg_season_fe %>% 
  write_csv("data/features/reg_season_fe.csv")
rm(tmp, reg_season_fe); gc()
