library(tidyverse)
target <- read_csv("data/processed/target.csv")

validate <- function(df){
  df %>% 
    inner_join(target, by = "ID") %>% 
    gather(type, Pred, -ID, -target) %>% 
    group_by(type) %>% 
    summarise(score = -mean(target * log(Pred) + (1-target) * log(1-Pred)))
}

validate_y <- function(df){
  df %>% 
    gather(model, Pred,-ID) %>% 
    inner_join(target, by = "ID") %>% 
    group_by(Season = str_sub(ID, 1, 4), model) %>% 
    summarise(score = -mean(target * log(Pred) + (1-target) * log(1-Pred))) %>% 
    ungroup() %>% 
    spread(model, score)
}
