# Keener

library(tidyverse)
set.seed(1)

reg_stats_compact <- read_csv("data/Stage2datafiles/RegularSeasonCompactResults.csv")


tmp <- bind_rows(
  reg_stats_compact %>% 
    select(Season, DayNum, TeamID = WTeamID, OTeamID = LTeamID,
           Score = WScore, OScore = LScore),
  reg_stats_compact %>% 
    select(Season, DayNum, TeamID = LTeamID, OTeamID = WTeamID,
           Score = LScore, OScore = WScore)
)


write_csv(tmp, "data/processed/reg_stats_compact.csv")
rm(tmp);gc()
