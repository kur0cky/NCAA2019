# OD matrix

library(tidyverse)

reg_stats_compact <- read_csv("data/processed/reg_stats_compact.csv")
target <- read_csv("data/processed/target.csv")


tmp <- reg_stats_compact %>%
  group_by(Season, TeamID, OTeamID) %>% 
  summarise(Score = mean(Score)) %>% 
  ungroup() %>% 
  group_by(Season) %>% 
  nest() %>% 
  mutate(data = map(data, ~ .x %>% 
                      spread(TeamID, Score, fill = 0) %>% 
                      as.data.frame() %>% 
                      column_to_rownames(var = "OTeamID") %>% 
                      as.matrix()),
         rate = map(data, ~ tibble(TeamID = colnames(.x))))

for(j in 1:nrow(tmp)){
  o <-list()
  o[[1]] <- rep(1, ncol(tmp$data[[j]]))
  for(i in 1:100){
    o[[i+1]] <- t(tmp$data[[j]]) %*% (1/(tmp$data[[j]] %*% (1/o[[i]])))
    if( sum((o[[i+1]] - o[[i]])^2) < 1e-16 ) break()
  }
  o <- o[[length(o)]]
  d <- tmp$data[[j]] %*% (1/o)
  tmp$rate[[j]]$o <- o
  tmp$rate[[j]]$d <- d
  print(i)
}
rm(o, d);gc()

tmp2 <- tmp %>% 
  select(Season, rate) %>% 
  unnest() %>% 
  mutate(od_rate = o/d) 

tmp2 %>% 
  write_csv("data/processed/od.csv")
