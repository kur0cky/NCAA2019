# feature_matrix
# create train data

# list.files("src/features", full.names = T) %>%
#   lapply(source)

library(tidyverse)

basis_nmf_fe <- read.csv("data/features/basis_nmf_fe.csv", stringsAsFactors = FALSE)
coef_nmf_fe <- read.csv("data/features/coef_nmf_fe.csv", stringsAsFactors = FALSE)
colley_fe <- read.csv("data/features/colley_fe.csv", stringsAsFactors = FALSE)
elo_fe <- read.csv("data/features/elo_fe.csv", stringsAsFactors = FALSE)
elo_score_fe <- read.csv("data/features/elo_score_fe.csv", stringsAsFactors = FALSE)
keener_scorerate_fe <- read.csv("data/features/keener_scorerate_fe.csv", stringsAsFactors = FALSE)
keener_scorediff_fe <- read.csv("data/features/keener_scorediff_fe.csv", stringsAsFactors = FALSE)
massey_fe <- read.csv("data/features/massey_fe.csv", stringsAsFactors = FALSE)
massey_2yr_fe <- read.csv("data/features/massey_2yr_fe.csv", stringsAsFactors = FALSE)
massey_3yr_fe <- read.csv("data/features/massey_3yr_fe.csv", stringsAsFactors = FALSE)
markov_win_diff <- read.csv("data/features/markov_win_diff.csv", stringsAsFactors = FALSE)
nmf_fe <- read.csv("data/features/nmf_fe.csv", stringsAsFactors = FALSE)
od_fe <- read.csv("data/features/od_fe.csv", stringsAsFactors = FALSE)
Pyth_ratio_fe <- read.csv("data/features/Pyth_ratio_fe.csv",stringsAsFactors = FALSE)
RPI_fe <- read.csv("data/features/RPI_fe.csv", stringsAsFactors = FALSE)
seed_fe <- read_csv("data/features/seed_fe.csv")

target <- read_csv("data/processed/target.csv")
sample <- read_csv("data/SampleSubmissionStage1.csv")

target %>% 
  bind_rows(anti_join(sample, target, by = "ID")) %>% 
  select(-Pred) %>% 
  left_join(keener_scorerate_fe, by = "ID") %>%
  left_join(keener_scorediff_fe, by = "ID") %>%
  left_join(nmf_fe, by = "ID") %>%
  left_join(basis_nmf_fe, by = "ID") %>%
  left_join(coef_nmf_fe, by = "ID") %>%
  left_join(od_fe, by = "ID") %>%
  left_join(markov_win_diff, by = "ID") %>%
  left_join(seed_fe, by = "ID") %>%
  left_join(massey_fe, by = "ID") %>%
  left_join(massey_2yr_fe, by = "ID") %>%
  left_join(massey_3yr_fe, by = "ID") %>%
  left_join(colley_fe, by = "ID") %>%
  left_join(elo_fe, by = "ID") %>%
  left_join(elo_score_fe, by = "ID") %>%
  left_join(Pyth_ratio_fe, by = "ID") %>%
  left_join(RPI_fe, by = "ID") %>%
  write_csv("data/train/features.csv")

rm(list = ls());gc()

  