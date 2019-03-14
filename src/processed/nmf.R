library(tidyverse)
library(NMF)

reg_stats_compact <- read_csv("data/DataFiles/RegularSeasonCompactResults.csv")

tmp <- bind_rows(
  reg_stats_compact %>% 
    select(Season,
           TeamID = WTeamID, OTeamID = LTeamID,
           Score = WScore,
           OScore = LScore),
  reg_stats_compact %>% 
    select(Season,
           TeamID = LTeamID, OTeamID = WTeamID,
           Score = LScore,
           OScore = WScore)
) %>% 
  group_by(Season, TeamID, OTeamID) %>% 
  summarise(Score = mean(Score / (OScore + Score))) %>% 
  group_by(Season) %>% 
  nest() %>% 
  mutate(data = map(data, ~ .x %>% 
                      spread(OTeamID, Score, fill = 0.5) %>% 
                      as.data.frame() %>% 
                      column_to_rownames(var = "TeamID") %>% 
                      as.matrix()),
         nmf = map(data, ~ nmf(.x, rank = 1, seed = "ica")),
         coef = map(nmf, ~ coef(.x)),
         basis = map(nmf, ~ basis(.x)),
         mat = map2(coef, basis, ~ .y %*% .x))
tmp %>% 
  mutate(data = map(mat, ~ .x %>% 
                      as.data.frame() %>% 
                      rownames_to_column(var = "team1") %>% 
                      as_tibble %>% 
                      gather(team2, rate, -team1))) %>% 
  select(Season, data) %>% 
  unnest() %>% 
  write_csv("data/processed/nmf.csv")
tmp %>% 
  select(Season, coef) %>% 
  mutate(coef = map(coef, ~ tibble(coef = c(.x),
                                   TeamID = colnames(.x)))) %>% 
  unnest() %>% 
  write_csv("data/processed/coef_nmf.csv")
tmp %>% 
  select(Season, basis) %>% 
  mutate(basis = map(basis, ~ tibble(basis = c(.x),
                                     TeamID = rownames(.x)))) %>% 
  unnest() %>% 
  write_csv("data/processed/basis_nmf.csv")

rm(tmp);gc()
