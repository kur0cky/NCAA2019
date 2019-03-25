library(tidyverse)
set.seed(1)

massey <- read_csv("data/processed/massey.csv")
elo <- read_csv("data/processed/elo.csv")
keener <- read_csv("data/processed/keener_scorerate.csv")
colley <- read_csv("data/processed/colley.csv")

tmp_m <- massey %>% 
  select(Season, TeamID, massey_r) %>% 
  group_by(Season) %>% 
  nest() %>% 
  mutate(data1 = map(data, ~ bind_cols(.x, .x) %>% 
                       select(-massey_r, -massey_r1) %>% 
                       complete(TeamID, TeamID1) %>% 
                       left_join(.x, by = c("TeamID")) %>% 
                       left_join(.x, by = c("TeamID1" = "TeamID")) %>% 
                       transmute(TeamID, TeamID1, rate_diff = massey_r.x - massey_r.y,
                                 rate_diff = if_else(rate_diff < 0, 0, rate_diff)) %>% 
                       spread(TeamID1, rate_diff) %>% 
                       as.data.frame() %>% 
                       column_to_rownames("TeamID"))) %>% 
  mutate(data1 = map(data1, function(x) x/sum(x))) %>% 
  select(-data)

tmp_e <- elo %>% 
  select(Season, TeamID, rate = elo_r) %>% 
  group_by(Season) %>% 
  nest() %>% 
  mutate(data1 = map(data, ~ bind_cols(.x, .x) %>% 
                       select(-rate, -rate1) %>% 
                       complete(TeamID, TeamID1) %>% 
                       left_join(.x, by = c("TeamID")) %>% 
                       left_join(.x, by = c("TeamID1" = "TeamID")) %>% 
                       transmute(TeamID, TeamID1, rate_diff = rate.x - rate.y,
                                 rate_diff = if_else(rate_diff < 0, 0, rate_diff)) %>% 
                       spread(TeamID1, rate_diff) %>% 
                       as.data.frame() %>% 
                       column_to_rownames("TeamID"))) %>% 
  mutate(data1 = map(data1, function(x) x/sum(x))) %>% 
  select(-data)

tmp_k <- keener %>% 
  select(Season, TeamID, rate) %>% 
  group_by(Season) %>% 
  nest() %>% 
  mutate(data1 = map(data, ~ bind_cols(.x, .x) %>% 
                       select(-rate, -rate1) %>% 
                       complete(TeamID, TeamID1) %>% 
                       left_join(.x, by = c("TeamID")) %>% 
                       left_join(.x, by = c("TeamID1" = "TeamID")) %>% 
                       transmute(TeamID, TeamID1, rate_diff = rate.x - rate.y,
                                 rate_diff = if_else(rate_diff < 0, 0, rate_diff)) %>% 
                       spread(TeamID1, rate_diff) %>% 
                       as.data.frame() %>% 
                       column_to_rownames("TeamID"))) %>% 
  mutate(data1 = map(data1, function(x) x/sum(x))) %>% 
  select(-data)
tmp_c <- colley %>% 
  select(Season, TeamID, rate = colley_r) %>% 
  group_by(Season) %>% 
  nest() %>% 
  mutate(data1 = map(data, ~ bind_cols(.x, .x) %>% 
                       select(-rate, -rate1) %>% 
                       complete(TeamID, TeamID1) %>% 
                       left_join(.x, by = c("TeamID")) %>% 
                       left_join(.x, by = c("TeamID1" = "TeamID")) %>% 
                       transmute(TeamID, TeamID1, rate_diff = rate.x - rate.y,
                                 rate_diff = if_else(rate_diff < 0, 0, rate_diff)) %>% 
                       spread(TeamID1, rate_diff) %>% 
                       as.data.frame() %>% 
                       column_to_rownames("TeamID"))) %>% 
  mutate(data1 = map(data1, function(x) x/sum(x))) %>% 
  select(-data)
func <- function(x, y, z, w){
  (x + y + z + w) / 4
}
tmp <- tmp_m %>% 
  left_join(tmp_e, by = "Season") %>% 
  left_join(tmp_k, by = "Season") %>% 
  left_join(tmp_c, by = "Season") %>% 
  mutate(rate = pmap(list(data1.x, data1.y, data1.x.x, data1.y.y), func))

tmp2 <- tmp %>% 
  select(Season, rate) %>% 
  mutate(rate = map(rate, ~ .x %>% 
                      as.matrix() %>% 
                      eigen %>% 
                      .$vectors %>% 
                      .[,1] %>% 
                      unlist() %>% 
                      Re() %>% 
                      tibble(TeamID = colnames(.x),
                             rate = .))) %>% 
  unnest()
tmp2 %>% 
  write_csv("data/processed/rating_mean.csv")
