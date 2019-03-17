library(tidyverse)
library(xgboost)
library(doParallel)

features <- read.csv("data/train/features.csv", stringsAsFactors = FALSE)
sample <- read.csv("data/WSampleSubmissionStage1.csv", 
                   stringsAsFactors = FALSE) %>% 
  as_tibble()
seeds <- read_csv("data/processed/seed.csv")
source("src/function/validate.R")

tr <- features %>% 
  filter(as.integer(str_sub(ID, 1, 4)) < 2014) %>% 
  drop_na()
te <- features %>% 
  semi_join(sample, by = "ID") %>% 
  arrange(ID)

tr_df <- tr %>% 
  select(-ID, -target)

tr_lab <- tr$target

te_df <- te %>% 
  select(-ID, -target) 



# xgb----
## 1----

dtrain <- xgb.DMatrix(as.matrix(tr_df),
                      label =  tr_lab)
dtest <- xgb.DMatrix(as.matrix(te_df),
                     label = te$target)

set <- expand.grid(colsample_bytree = c(0.7, 0.8, 0.9, 1),
                   subsample = c(0.7, 0.8, 0.9, 1))

set <- expand.grid(max_depth = 1:4,
                   min_child_weight = 1:5)
# cl <- makePSOCKcluster(4)
# registerDoParallel(cl)

a <- foreach(i = 1:nrow(set),
             .packages = c("xgboost")) %do%{
               param <- list(
                 max_depth = set$max_depth[i],
                 min_child_weight = set$min_child_weight[i],
                 # colsample_bytree = set$colsample_bytree[i],
                 # subsample = set$subsample[i],
                 eta = .05,
                 silent = 1, 
                 booster = "dart",
                 objective = "binary:logistic",
                 eval_metric = "logloss",
                 nthread = 1)
               
               set.seed(1)
               cv <- xgb.cv(params = param, dtrain, nrounds = 10000, nfold = 20,
                            early_stopping_rounds = 30,
                            prediction = TRUE)
               return(cv)
             }
# stopCluster(cl)
set$score <- lapply(a, function(x) x$evaluation_log[x$best_iteration,4]) %>% 
  unlist()
set %>% 
  ggplot(aes(alpha, lambda))+
  geom_point(aes(size = 1/score))+
  scale_x_continuous(limits = c(0,0.2))
set %>% 
  arrange(score)

# max_depth = 1
# min_child_weight = 1.7

# gamma = 0
set <- expand.grid(max_depth = 1:8,
                   min_child_weight = seq(0,2, length = 10))
# cl <- makePSOCKcluster(4)
# registerDoParallel(cl)

a <- foreach(i = 1:nrow(set),
             .packages = c("xgboost")) %do%{
               param <- list(max_depth = set$max_depth[i],
                             min_child_weight = set$min_child_weight[i],
                             # gamma = 0,
                             # colsample_bytree = set$colsample_bytree[i],
                             # subsample = set$subsample[i],
                             eta = .02,
                             silent = 10, 
                             lambda = 0,
                             booster = "glinear",
                             objective = "binary:logistic",
                             eval_metric = "logloss"
               )
               
               set.seed(1)
               cv <- xgb.cv(params = param, dtrain, nrounds = 10000, nfold = 20,
                            early_stopping_rounds = 10,
                            prediction = TRUE)
               return(cv)
             }
# stopCluster(cl)
set$score <- lapply(a, function(x) x$evaluation_log[x$best_iteration,4]) %>% 
  unlist()
set %>% 
  ggplot(aes(colsample_bytree, subsample))+
  geom_point(aes(size = 1/score))
set %>% 
  arrange(score)
# colsample_bytree = 0.5
# subsample = 1

set.seed(1)
bst <- xgb.train(params = param, dtrain, nrounds = cv$best_iteration)