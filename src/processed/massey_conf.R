# massey rating

library(tidyverse)

reg_stats_compact <- read_csv("data/DataFiles/RegularSeasonCompactResults.csv")
teamconf <-read_csv("data/datafiles/TeamConferences.csv")
target <- read_csv("data/processed/target.csv")

# matrix X
X <- reg_stats_compact %>% 
  left_join(teamconf, by = c("Season", "WTeamID" = "TeamID")) %>% 
  left_join(teamconf, by = c("Season", "LTeamID" = "TeamID")) %>% 
  filter(ConfAbbrev.x != ConfAbbrev.y) %>% 
  select(Season, ConfAbbrev.x, ConfAbbrev.y) %>% 
  group_by(Season) %>% 
  mutate(game_id = 1:n()) %>% 
  ungroup() %>% 
  gather(key, conf, -Season, -game_id) %>% 
  mutate(win_flg = if_else(key == "WTeamID", 1, -1)) %>% 
  select(-key) %>% 
  group_by(Season) %>% 
  nest() %>% 
  mutate(data = map(data, ~ .x %>% 
                      spread(conf, win_flg, fill = 0) %>% 
                      arrange(game_id) %>% 
                      select(-game_id)))



y <- reg_stats_compact %>% 
  left_join(teamconf, by = c("Season", "WTeamID" = "TeamID")) %>% 
  left_join(teamconf, by = c("Season", "LTeamID" = "TeamID")) %>% 
  filter(ConfAbbrev.x != ConfAbbrev.y) %>% 
  group_by(Season) %>% 
  mutate(game_id = 1:n()) %>% 
  ungroup() %>% 
  transmute(Season, game_id,
            Score_diff = WScore - LScore) %>% 
  arrange(Season, game_id) %>% 
  select(-game_id) %>% 
  group_by(Season) %>% 
  nest() %>% 
  mutate(data = map(data, ~ unlist(.x)))

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
                    mutate(ConfAbbrev = colnames(.x)) ))
X %>% 
  unnest(r) %>% 
  rename(massey_conf_r = V1) %>% 
  write_csv("data/processed/massey_conf.csv")
rm(X, y);gc()
