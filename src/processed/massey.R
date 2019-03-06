# massey rating

library(tidyverse)

reg_stats_compact <- read_csv("data/DataFiles/RegularSeasonCompactResults.csv")
teamconf <-read_csv("data/datafiles/TeamConferences.csv")
target <- read_csv("data/processed/target.csv")

# matrix X
X <- reg_stats_compact %>% 
  select(Season, WTeamID, LTeamID) %>% 
  group_by(Season) %>% 
  mutate(game_id = 1:n()) %>% 
  ungroup() %>% 
  gather(key, TeamID, -Season, -game_id) %>% 
  mutate(win_flg = if_else(key == "WTeamID", 1, -1)) %>% 
  select(-key) %>% 
  group_by(Season) %>% 
  nest() %>% 
  mutate(data = map(data, ~ .x %>% 
                      spread(TeamID, win_flg, fill = 0) %>% 
                      arrange(game_id) %>% 
                      select(-game_id)))



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
                    mutate(TeamID = colnames(.x)) ))

X$t <- reg_stats_compact %>% 
  select(Season, ends_with("ID")) %>% 
  gather(key, TeamID, -Season) %>% 
  count(Season, TeamID) %>% 
  arrange(Season, TeamID) %>% 
  select(-TeamID) %>% 
  group_by(Season) %>% 
  nest() %>% 
  mutate(data = map(data, ~ diag(.x$n))) %>% 
  .$data

X$P <- rbind(
  reg_stats_compact %>% 
    select(Season, team1 = WTeamID, team2 = LTeamID),
  reg_stats_compact %>% 
    select(Season, team1 = LTeamID, team2 = WTeamID)
) %>% 
  count(Season, team1, team2) %>% 
  group_by(Season) %>% 
  nest() %>% 
  mutate(data = map(data, ~ .x %>% 
                      spread(team2, n, fill=0) %>% 
                      as.data.frame() %>% 
                      column_to_rownames(var = "team1") %>% 
                      as.matrix())) %>% 
  .$data

X$f <- reg_stats_compact %>% 
  select(Season, ends_with("ID")) %>% 
  gather(key, TeamID, -Season) %>% 
  mutate(win_flg = if_else(key == "WTeamID", 1, 0)) %>% 
  group_by(Season, TeamID) %>% 
  summarise(n_win = sum(win_flg)) %>% 
  ungroup() %>% 
  select(-TeamID) %>% 
  group_by(Season) %>% 
  nest() %>% 
  mutate(data = map(data, ~ .x$n_win)) %>% 
  .$data

d <- list()
o <- list()
for(i in 1:nrow(X)){
  d[[i]] <- c(solve(X$t[[i]] + X$P[[i]]) %*% (X$t[[i]] %*% as.matrix(X$r[[i]][,1]) - X$f[[i]]))
  o[[i]] <- c(as.matrix(X$r[[i]][,1]) - d[[i]])
}
X %>% 
  mutate(d = d,
         o = o) %>% 
  select(Season, r, d, o) %>% 
  mutate(r = map2(r, d, ~ mutate(.x, massey_d = .y)),
         r = map2(r, o, ~ mutate(.x, massey_o = .y))) %>% 
  unnest(r) %>% 
  rename(massey_r = V1) %>% 
  write_csv("data/processed/massey.csv")
rm(X, y, o, d);gc()


# カンファレンスごとにするか否か
# reg_stats %>% 
#   select(1:5) %>% 
#   left_join(teamconf, by = c("Season", "WTeamID" = "TeamID")) %>% 
#   left_join(teamconf, by = c("Season", "LTeamID" = "TeamID")) %>% 
#   count(ConfAbbrev.x,ConfAbbrev.y) %>% 
#   arrange(desc(n)) %>% View()

