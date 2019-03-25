# markov method

library(tidyverse)

reg_stats_compact <- read_csv("data/processed/reg_stats_compact.csv")
target <- read_csv("data/processed/target.csv")
sample <- read_csv("data/SampleSubmissionStage2.csv")


tmp <- reg_stats_compact %>%
  group_by(Season, TeamID, OTeamID) %>% 
  summarise(Score_rate = sum(Score > OScore)) %>% 
  ungroup() %>% 
  # ここで歪み補正入れてもいい
  group_by(Season) %>% 
  nest(.key = "mat") %>% 
  mutate(mat = map(mat, ~ .x %>% 
                     spread(OTeamID, Score_rate, fill = 0.00001) %>% 
                     as.data.frame() %>% 
                     column_to_rownames("TeamID") %>% 
                     as.matrix() %>% 
                     apply(1, function(x) x/sum(x))),
         eigen = map(mat, eigen),
         rate = map2(eigen, mat,
                     ~ tibble(TeamID = colnames(.y),
                              rate = Re(.x$vectors[,1])) ))

tmp2 <- tmp %>% 
  select(Season, rate) %>% 
  unnest() %>% 
  group_by(Season) %>% 
  mutate(rate = abs(rate)) %>% 
  ungroup() %>% 
  mutate(TeamID = as.integer(TeamID))

fe <- target %>% 
  bind_rows(sample) %>% 
  distinct(ID) %>% 
  mutate(Season = as.integer(str_sub(ID, 1, 4)),
         team1 = as.integer(str_sub(ID, 6, 9)),
         team2 = as.integer(str_sub(ID, 11, 14))) %>% 
  left_join(tmp2, by = c("Season", "team1" = "TeamID")) %>% 
  left_join(tmp2, by = c("Season", "team2" = "TeamID")) %>% 
  select(-Season, -team1, -team2) %>% 
  transmute(ID,
            markov_win_diff = rate.x - rate.y)
fe %>% 
  write_csv("data/features/markov_win_diff.csv")
rm(tmp, tmp2, fe); gc()
