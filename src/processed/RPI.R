# RPI feature from regular season

library(tidyverse)
reg_stats_compact <- read.csv("data/datafiles/RegularSeasonCompactResults.csv",
                      stringsAsFactors = FALSE) %>% 
  as_tibble()


# winning percentage
WP <- reg_stats_compact %>% 
  select(Season, WTeamID, LTeamID) %>% 
  gather(win_flg, TeamID, -Season) %>% 
  mutate(win_flg = if_else(win_flg == "WTeamID", 1, 0)) %>% 
  group_by(Season, TeamID) %>% 
  summarise(WP = sum(win_flg) / n()) %>% 
  ungroup()

# opponents' winning percentage
OWP_tmp <- bind_rows(
  reg_stats_compact %>% 
    select(Season, TeamID = WTeamID, OTeamID = LTeamID) %>% 
    mutate(win_flg = 1),
  reg_stats_compact %>% 
    select(Season, TeamID = LTeamID, OTeamID = WTeamID) %>% 
    mutate(win_flg = 0)
  ) %>% 
  arrange(Season, TeamID)

OWP <- OWP_tmp %>% 
  select(-win_flg) %>% 
  group_by(Season, TeamID) %>% 
  nest(.key ="OTeamID") %>% 
  mutate(OTeamID = map(OTeamID, ~ unlist(.x$OTeamID))) %>% 
  mutate(OWP = NA)

for(i in 1:nrow(OWP)){
  OWP$OWP[i] <- OWP_tmp %>% 
    filter(Season == OWP$Season[i],
           TeamID %in% OWP$OTeamID[[i]],
           OTeamID != OWP$TeamID[[i]]) %>% 
    summarise(sum(win_flg) / n()) %>% 
    unlist()
}
OWP <- OWP %>%
  select(-OTeamID)

# OOWP
OOWP <- OWP_tmp %>% 
  left_join(OWP, by = c("Season", "OTeamID" = "TeamID")) %>% 
  group_by(Season, TeamID) %>% 
  summarise(OOWP = mean(OWP)) %>% 
  ungroup()


RPI <- WP %>% 
  left_join(OWP, by = c("Season", "TeamID")) %>% 
  left_join(OOWP, by = c("Season", "TeamID")) %>% 
  mutate(RPI = WP*0.25 + OWP*0.5 + OOWP*0.25) 

RPI %>% 
  write_csv("data/processed/RPI.csv")
rm(WP, OWP, OOWP); gc()
