library(tidyverse)
tourney_stats_compact <- read_csv("data/Stage2DataFiles/NCAATourneyCompactResults.csv")
set.seed(1)
target <- tourney_stats_compact %>% 
  mutate(team1 = if_else(WTeamID < LTeamID, WTeamID, LTeamID),
         team2 = if_else(WTeamID < LTeamID, LTeamID, WTeamID),
         target = if_else(WTeamID < LTeamID, 1, 0)) %>% 
  transmute(
    ID = str_c(Season, team1, team2, sep = "_"),
    target,
    # Season,
    # team1,
    # team2
    )

write_csv(target, "data/processed/target.csv")
