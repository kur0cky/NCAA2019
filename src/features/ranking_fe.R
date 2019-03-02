library(tidyverse)

massey <- read_csv("data/MasseyOrdinals.csv")
target <- read_csv("data/processed/target.csv")


ranking <- massey %>% 
  filter(RankingDayNum == 128,
         SystemName %in% c("POM", "MOR", "MAS", 
                           # "EBP",
                           "RPI")) %>% 
  select(-RankingDayNum) %>% 
  spread(SystemName, OrdinalRank)

ranking_fe <- target %>% 
  select(-target) %>% 
  mutate(Season = as.integer(str_sub(ID, 1, 4)),
         team1 = as.integer(str_sub(ID, 6, 9)),
         team2 = as.integer(str_sub(ID, 11, 14))) %>% 
  left_join(ranking, by = c("Season", "team1" = "TeamID")) %>% 
  left_join(ranking, by = c("Season", "team2" = "TeamID")) %>% 
  select(-Season, -team1, -team2) %>% 
  transmute(ID,
            MAS_diff = MAS.x - MAS.y,
            RPI_diff = RPI.x - RPI.y,
            POM_diff = POM.x - POM.y,
            MOR_diff = MOR.x - MOR.y)

ranking_fe %>% 
  write_csv("data/features/ranking_fe.csv")
rm(ranking_fe, ranking);gc()
