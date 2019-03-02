# Pythagorean expectation

library(tidyverse)
reg_stats <- read.csv("data/datafiles/RegularSeasonDetailedResults.csv",
                      stringsAsFactors = FALSE) %>% 
  as_tibble()
reg_stats_compact <- read.csv("data/datafiles/RegularSeasonCompactResults.csv",
                          stringsAsFactors = FALSE) %>% 
  as_tibble()

Pyth_ratio <- bind_rows(
    reg_stats_compact %>% 
      select(Season, TeamID = WTeamID, Score = WScore, Allow = LScore),
    reg_stats_compact %>% 
      select(Season, TeamID = LTeamID, Score = LScore, Allow = WScore)
  ) %>% 
  group_by(Season, TeamID) %>% 
  summarise(Pyth_ratio = sum(Score)^2 / (sum(Score)^2 + sum(Allow)^2)) %>% 
  ungroup()

Pyth_ratio %>% 
  write_csv("data/processed/Pyth_ratio.csv")
rm(Pyth_ratio);gc()
