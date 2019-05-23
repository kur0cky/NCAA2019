# feature selection with boruta
library(Boruta)
library(tidyverse)
features_kstest <- read_rds("data/features/features_ks-test.RDS")
tr <- read_csv("data/features/features.csv") %>% 
  drop_na() %>% 
  select(target, features_kstest)

set.seed(111)
res_boruta <- Boruta(target ~ .,
                     # -quake_index - id
                     # -peak_acf_lag -peak_acf_start_lag-sd_flg-peak_acf_end_lag-spec_ar_mode,
                     data = tr,
                     maxRuns = 100,
                     doTrace=2)

print(res_boruta)
plot(res_boruta)
attStats(res_boruta) %>%
  rownames_to_column("feature") %>%
  arrange(desc(meanImp))
plotImpHistory(res_boruta)


# 
# attStats(res_boruta) %>%
#   rownames_to_column("feature") %>%
#   arrange(desc(meanImp)) %>% 
#   filter(decision == "Confirmed") %>% 
#   .$feature %>% 
#   write_rds("data/features/features_boruta.RDS")
 