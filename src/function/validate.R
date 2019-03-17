library(tidyverse)
tourney_compact <- read_csv("data/DataFiles/NCAATourneyCompactResults.csv") %>% 
  mutate(team1 = if_else(WTeamID < LTeamID, WTeamID, LTeamID),
         team2 = if_else(WTeamID > LTeamID, WTeamID, LTeamID)) %>% 
  transmute(Season, 
            ID = str_c(Season, team1, team2, sep = "_"),
            DayNum) 
target <- read_csv("data/processed/target.csv")
target <- target %>%
  inner_join(tourney_compact, by = "ID") %>%
  arrange(Season, DayNum, ID) %>%
  group_by(Season) %>%
  dplyr::slice(-c(1:4)) %>%
  ungroup() %>%
  filter(Season >= 2014) %>% 
  select(ID, target)

validate <- function(df){
  df %>% 
    inner_join(target, by = "ID") %>% 
    gather(type, Pred, -ID, -target) %>% 
    group_by(type) %>% 
    summarise(score = -mean(target * log(Pred) + (1-target) * log(1-Pred)))
}

validate_y <- function(df){
  df %>% 
    gather(model, Pred,-ID) %>% 
    inner_join(target, by = "ID") %>% 
    group_by(Season = str_sub(ID, 1, 4), model) %>% 
    summarise(score = -mean(target * log(Pred) + (1-target) * log(1-Pred))) %>% 
    ungroup() %>% 
    spread(model, score)
}

rm(tourney_compact); gc()
