# massey rating 2y

library(tidyverse)

reg_stats_compact <- read_csv("data/DataFiles/RegularSeasonCompactResults.csv")
# teamconf <-read_csv("data/Wdatafiles/WTeamConferences.csv")
target <- read_csv("data/processed/target.csv")

# matrix X
X <- reg_stats_compact %>% 
  select(Season, WTeamID, LTeamID) %>% 
  group_by(Season) %>% 
  mutate(game_id = 1:n(), s = Season) %>% 
  ungroup() %>% 
  gather(key, TeamID, -Season, -game_id, -s) %>% 
  mutate(win_flg = if_else(key == "WTeamID", 1, -1)) %>% 
  select(-key) %>% 
  group_by(Season) %>% 
  nest() %>% 
  mutate(lag = lag(data, 1),
         data = map2(data, lag, ~ drop_na(rbind(.y, .x))),
         data = map(data, ~ .x %>% 
                      spread(TeamID, win_flg, fill = 0) %>% 
                      arrange(game_id) %>% 
                      select(-game_id, -s))) %>% 
  select(-lag)



y <- reg_stats_compact %>% 
  group_by(Season) %>% 
  mutate(game_id = 1:n()) %>% 
  ungroup() %>% 
  transmute(Season, game_id,
            Score_diff = WScore - LScore) %>% 
  arrange(Season, game_id) %>% 
  select(-game_id) %>% 
  group_by(Season) %>% 
  nest() %>% 
  mutate(data = map(data, ~ unlist(.x)),
         lag = lag(data, 1),
         data = map2(data, lag, ~ c(.y, .x)),
         data = map(data, ~ .x[!is.na(.x)]))

X <- X %>% 
  mutate(data = map(data, ~ as.matrix(.x)),
         M = map(data, ~ t(.x) %*% .x),
         y = y$data,
         p = map2(data, y, ~ t(.x) %*% .y)); gc()

for(i in 1:nrow(X)){
  X$M[[i]][nrow(X$M[[i]]),] <- 1
  X$p[[i]][nrow(X$p[[i]]),] <- 0
}
X <- X %>% 
  mutate(r = map2(M, p, ~ (MASS::ginv(.x) %*% .y) %>% 
                    as.data.frame() %>% 
                    mutate(TeamID = colnames(.x)) ))

X %>% 
  select(Season, r) %>% 
  unnest(r) %>% 
  rename(massey_r = V1) %>% 
  write_csv("data/processed/massey_2yr.csv")
rm(X, y, o, d);gc()

