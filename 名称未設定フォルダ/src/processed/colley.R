# colley rating

library(tidyverse)
set.seed(1)

reg_stats_compact <- read_csv("data/Stage2DataFiles/RegularSeasonCompactResults.csv")

b <- reg_stats_compact %>% 
  select(Season, WTeamID, LTeamID) %>% 
  gather(key, TeamID, -Season) %>% 
  group_by(Season, TeamID) %>% 
  summarise(n_win = sum(key == "WTeamID"),
            n_lose = sum(key == "LTeamID")) %>% 
  ungroup() %>% 
  mutate(b = 1 + (n_win - n_lose) / 2) %>% 
  arrange(Season, TeamID) %>% 
  select(Season, b) %>% 
  group_by(Season) %>% 
  nest() %>% 
  mutate(data = map(data, ~ .x$b))

diag <- reg_stats_compact %>% 
  select(Season, WTeamID, LTeamID) %>% 
  gather(key, TeamID, -Season) %>% 
  count(Season, TeamID) %>% 
  arrange(Season, TeamID) %>% 
  select(-TeamID) %>% 
  group_by(Season) %>% 
  nest() %>% 
  mutate(data = map(data, ~ .x$n))

tmp <- rbind(
  reg_stats_compact %>% 
    select(Season, team1 = WTeamID, team2 = LTeamID),
  reg_stats_compact %>% 
    select(Season, team1 = LTeamID, team2 = WTeamID)
) %>% 
  count(Season, team1, team2) %>% 
  mutate(n = -n) %>% 
  group_by(Season) %>% 
  nest() %>% 
  mutate(data = map(data, ~ .x %>% 
                      spread(team2, n, fill=0) %>% 
                      as.data.frame() %>% 
                      column_to_rownames(var = "team1") %>% 
                      as.matrix()))

for(i in 1:nrow(tmp)){
  diag(tmp$data[[i]]) <- diag$data[[i]]
}

tmp %>% 
  mutate(b = b$data,
         r = map2(data, b, ~  
                    (MASS::ginv(.x) %*% .y) %>% 
                    data.frame(TeamID = as.integer(rownames(.x)),
                               colley_r = .))) %>% 
  unnest(r) %>% 
  write_csv("data/processed/colley.csv")
rm(tmp, diag, b);gc()
