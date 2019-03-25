# keener score

library(tidyverse)
set.seed(1)

reg_stats_compact <- read_csv("data/processed/reg_stats_compact.csv")

tmp <- reg_stats_compact %>%
  group_by(Season, TeamID, OTeamID) %>% 
  summarise(Score_rate = mean( (Score + 1)/(Score + OScore + 2) )) %>% 
  ungroup() %>% 
  # ここで歪み補正入れてもいい
  group_by(Season) %>% 
  nest(.key = "mat") %>% 
  mutate(mat = map(mat, ~ .x %>% 
                     spread(OTeamID, Score_rate, fill = 0.00001) %>% 
                     as.data.frame() %>% 
                     column_to_rownames("TeamID") %>% 
                     as.matrix()),
         eigen = map(mat, eigen),
         rate = map2(eigen,mat, ~
                       tibble(TeamID = colnames(.y),
                              rate = Re(.x$vectors[,1])))) 

tmp2 <- tmp %>% 
  select(Season, rate) %>% 
  unnest() %>% 
  group_by(Season) %>% 
  mutate(rate = abs(rate)) %>% 
  ungroup()

tmp2 %>% 
  write_csv("data/processed/keener_scorerate.csv")
rm(tmp, tmp2);gc()
