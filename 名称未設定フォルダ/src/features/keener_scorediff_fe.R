# keener wildcard

library(tidyverse)

reg_stats_compact <- read_csv("data/processed/reg_stats_compact.csv")
target <- read_csv("data/processed/target.csv")
sample <- read_csv("data/SampleSubmissionStage2.csv")

tmp <- reg_stats_compact %>%
  group_by(Season, TeamID, OTeamID) %>% 
  summarise(score_diff = (sum(Score > OScore)+1) / (n()+2) ) %>% 
  ungroup() %>% 
  # ここで歪み補正入れてもいい
  group_by(Season) %>% 
  nest(.key = "mat") %>% 
  mutate(mat = map(mat, ~ .x %>% 
                     spread(OTeamID, score_diff, fill = 0.00001) %>% 
                     as.data.frame() %>% 
                     column_to_rownames("TeamID") %>% 
                     as.matrix()),
         eigen = map(mat, eigen),
         score_diff = map2(eigen,mat, ~
                             tibble(TeamID = colnames(.y),
                                    rate = Re(.x$vectors[,1])))) 

tmp2 <- tmp %>% 
  select(Season, score_diff) %>% 
  unnest() %>% 
  group_by(Season) %>% 
  mutate(score_diff = abs(rate)) %>% 
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
            keener_scorediff_diff = score_diff.x - score_diff.y)

write_csv(fe, "data/features/keener_scorediff_fe.csv")
rm(tmp, tmp2, fe);gc()
