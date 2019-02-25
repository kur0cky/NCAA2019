# feature_matrix
# create train data

# data/train/featuresから始まって
# trainとtestに分けて
# submitまで

library(tidyverse)

features <- read_csv("data/train/features.csv")
source("src/function/validate.R")

tr <- features %>% 
  filter(as.integer(str_sub(ID, 1, 4)) < 2014)
te <- features %>% 
  filter(as.integer(str_sub(ID, 1, 4)) >= 2014) %>% 
  arrange(ID)

tr_df <- tr %>% 
  select(-ID, -target) %>% 
  as.data.frame()

tr_lab <- tr$target

te_df <- te %>% 
  select(-ID, -target) %>% 
  as.data.frame()


fit <- glm(tr_lab ~ . - 1 - seed_diff, data = tr_df, family = binomial(logit)) 

pred <- predict(fit, newdata = te_df) 
submit <- tibble(ID = te$ID,
                 Pred = 1/(1 + exp(-pred)))

submit %>% 
  group_by(str_sub(ID, 1, 4)) %>% 
  validate()
validate(submit)

# xgb

dtrain <- xgb.DMatrix(as.matrix(tr_df),
                      label =  tr_lab)
dtest <- xgb.DMatrix(as.matrix(te_df),
                     label = te$target)

param <- list(max_depth = 3, eta = .1, silent = 1, nthread = 2, 
              objective = "binary:logistic", eval_metric = "logloss")
cv <- xgb.cv(params = param, dtrain, nrounds = 100, nfold = 10,
             early_stopping_rounds = 10)

bst <- xgb.train(params = param, dtrain, nrounds = cv$best_iteration)
submit <- tibble(ID = te$ID,
                 Pred = predict(bst, dtest))

validate(submit)
submit %>% 
  group_by(str_sub(ID, 1, 4)) %>% 
  validate()
