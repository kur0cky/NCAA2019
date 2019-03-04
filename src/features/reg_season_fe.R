library(tidyverse)

reg_stats <- read_csv("data/DataFiles/RegularSeasonDetailedResults.csv")
target <- read_csv("data/processed/target.csv")

tmp <- reg_stats %>% 
  mutate(WPoss = WFGA + 0.47 * WFTA - WOR + WTO,
         LPoss = LFGA + 0.47 * LFTA - LOR + LTO) %>% 
  transmute(Season, WTeamID, LTeamID,
            PossDiff = WPoss - LPoss) %>% 
  gather(key, TeamID, -Season, -PossDiff) %>% 
  group_by(Season, TeamID) %>% 
  summarise(avg_PossDiff = mean(PossDiff))


reg_season_fe <- target %>% 
  select(-target) %>% 
  mutate(Season = as.integer(str_sub(ID, 1, 4)),
         team1 = as.integer(str_sub(ID, 6, 9)),
         team2 = as.integer(str_sub(ID, 11, 14))) %>% 
  left_join(tmp, by = c("Season", "team1" = "TeamID")) %>% 
  left_join(tmp, by = c("Season", "team2" = "TeamID")) %>% 
  select(-Season, -team1, -team2) %>% 
  transmute(ID, 
            avg_PossDiff_diff = avg_PossDiff.x - avg_PossDiff.y)

reg_season_fe %>% 
  write_csv("data/features/reg_season_fe.csv")
rm(tmp, reg_season_fe); gc()
