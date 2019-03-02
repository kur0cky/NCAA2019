library(tidyverse)

reg_season_stats <- read_csv("data/DataFiles/RegularSeasonDetailedResults.csv")
target <- read_csv("data/processed/target.csv")


reg_season <- reg_season_stats %>% 
  transmute(Season, WTeamID, LTeamID,
            WPoss = WFGA + 0.47 * WFTA - WOR + WTO,
            LPoss = LFGA + 0.47 * LFTA - LOR + LTO) %>% 
  gather(win_flg, TeamID, -Season, -WPoss, -LPoss) %>% 
  transmute(Season, TeamID,
            Poss = if_else(win_flg == "WTeamID", WPoss, LPoss)) %>% 
  group_by(Season, TeamID) %>% 
  summarise(mean_poss = mean(Poss),
            median_poss = median(Poss),
            sd_poss = sd(Poss),
            q10_poss = quantile(Poss, .1),
            q90_poss = quantile(Poss, .9))

reg_season_fe <- target %>% 
  select(-target) %>% 
  mutate(Season = as.integer(str_sub(ID, 1, 4)),
         team1 = as.integer(str_sub(ID, 6, 9)),
         team2 = as.integer(str_sub(ID, 11, 14))) %>% 
  left_join(reg_season, by = c("Season", "team1" = "TeamID")) %>% 
  left_join(reg_season, by = c("Season", "team2" = "TeamID")) %>% 
  select(-Season, -team1, -team2) %>% 
  transmute(ID, 
            mean_poss_diff = mean_poss.x - mean_poss.y,
            median_poss_diff = median_poss.x - median_poss.y,
            sd_poss.x, sd_poss.y)

reg_season_fe %>% 
  write_csv("data/features/reg_season_fe.csv")
rm(reg_season_fe, reg_season, reg_season_stats); gc()
