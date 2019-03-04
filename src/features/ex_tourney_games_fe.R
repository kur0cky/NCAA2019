# ex tourney games

tourney_stats_compact <- read.csv("data/DataFiles/NCAATourneyCompactResults.csv", 
                                  stringsAsFactors = FALSE) %>% 
  as_tibble()
target <- read.csv("data/processed/target.csv",
                   stringsAsFactors = FALSE) %>% 
  as_tibble()

tmp <- tourney_stats_compact %>% 
  select(Season, WTeamID, LTeamID) %>% 
  gather(type, TeamID, -Season) %>% 
  count(Season, TeamID) %>% 
  complete(Season, TeamID) %>%
  arrange(TeamID, Season) %>% 
  replace_na(list(n = 0)) %>% 
  mutate(Season = Season + 1) %>% 
  rename(ex_games = n)

fe <- target %>% 
  select(-target) %>% 
  mutate(Season = as.integer(str_sub(ID, 1, 4)),
         team1 = as.integer(str_sub(ID, 6, 9)),
         team2 = as.integer(str_sub(ID, 11, 14))) %>% 
  left_join(tmp, by = c("Season", "team1" = "TeamID")) %>% 
  left_join(tmp, by = c("Season", "team2" = "TeamID")) %>% 
  select(-Season, -team1, -team2) %>% 
  transmute(ID,
            ex_games_diff = ex_games.x - ex_games.y)

write_csv(fe, "data/features/ex_tourney_games_fe.csv")
rm(tmp, fe);gc()
