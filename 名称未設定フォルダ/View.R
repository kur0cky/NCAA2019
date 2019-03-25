library(tidyverse)
tmp<- read_csv("data/submit/first.csv")
seeds <- read_csv("data/processed/seed.csv") %>% 
  filter(Season == 2019) %>% 
  select(-Season)
teams <- read_csv("data/stage2datafiles/teams.csv")


tmp2 <- tmp %>% 
  transmute(team1 = as.integer(str_sub(ID, 6,9)),
            team2 = as.integer(str_sub(ID, 11,14)),
            Pred) %>% 
  left_join(teams, by = c("team1" = "TeamID")) %>% 
  select(-ends_with("Season")) %>% 
  left_join(teams, by = c("team2" = "TeamID")) %>% 
  select(-ends_with("Season")) %>% 
  left_join(seeds, by = c("team1" = "TeamID"))%>% 
  left_join(seeds, by = c("team2" = "TeamID")) 

tmp2 %>% 
  filter(seed.x + seed.y == 9) %>% 
  View()

            