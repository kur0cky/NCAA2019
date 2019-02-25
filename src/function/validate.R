library(tidyverse)
target <- read_csv("data/processed/target.csv")

validate <- function(df){
  df %>% 
    select(ID, Pred) %>% 
    inner_join(target, by = "ID") %>% 
    summarise(score = -mean(target * log(Pred) + (1-target) * log(1-Pred)))
}
