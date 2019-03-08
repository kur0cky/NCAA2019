# feature_matrix
# create train data

# data/train/featuresから始まって
# trainとtestに分けて
# submitまで

library(tidyverse)
library(xgboost)

features <- read.csv("data/train/features.csv", stringsAsFactors = FALSE)
source("src/function/validate.R")

tr <- features %>% 
  filter(as.integer(str_sub(ID, 1, 4)) < 2014)
te <- features %>% 
  semi_join(sample, by = "ID") %>% 
  arrange(ID)

tr_df <- tr %>% 
  select(-ID, -target)

tr_lab <- tr$target

te_df <- te %>% 
  select(-ID, -target) 

# 
# fit <- glm(tr_lab ~ . - 1 - seed_diff, data = tr_df, family = binomial(logit)) 
# 
# pred <- predict(fit, newdata = te_df) 
# submit <- tibble(ID = te$ID,
#                  Pred = 1/(1 + exp(-pred)))
# 
# submit %>% 
#   group_by(str_sub(ID, 1, 4)) %>% 
#   validate()
# validate(submit)

# xgb

dtrain <- xgb.DMatrix(as.matrix(tr_df),
                      label =  tr_lab)
dtest <- xgb.DMatrix(as.matrix(te_df),
                     label = te$target)

set.seed(1)
param <- list(max_depth = 3, eta = .01, silent = 1, nthread = 2, 
              lambda = .1,
              objective = "binary:logistic", eval_metric = "logloss")
cv <- xgb.cv(params = param, dtrain, nrounds = 1000, nfold = 20,
             early_stopping_rounds = 2)

bst <- xgb.train(params = param, dtrain, nrounds = cv$best_iteration)
submit <- tibble(ID = te$ID,
                 Pred = predict(bst, dtest))

xgb.importance(colnames(dtrain), bst)



fit_glm <- glm(tr_lab ~ .,
               data = tr_df,
               family = binomial)
library(glmnet)
cvfit <- cv.glmnet(x = as.matrix(tr_df), y = tr_lab, family = "binomial", standardize = TRUE,
                   alpha = 1)
fit_lasso <- glmnet(x = as.matrix(tr_df), y = tr_lab, family = "binomial", standardize = TRUE,
                    alpha = 1,
                    lambda = cvfit$lambda.min)
cvfit <- cv.glmnet(x = as.matrix(tr_df), y = tr_lab, family = "binomial", standardize = TRUE,
                   alpha = 0)
fit_ridge <- glmnet(x = as.matrix(tr_df), y = tr_lab, family = "binomial", standardize = TRUE,
                    alpha = 0,
                    lambda = cvfit$lambda.min)

library(ranger)

fit_rf <- ranger(target ~ . - ID,
                 data = tr,
                 num.trees = 2000,
                 importance = "permutation")
fit_ext <- ranger(target ~ . - ID,
                  data = tr,
                  num.trees = 2000,
                  importance = "permutation",
                  splitrule = "extratrees")

submit <- submit %>% 
  mutate(pred_xgb = predict(bst, dtest),
         pred_glm = predict(fit_glm, newdata = te_df, type = "response"),
         pred_lasso = predict(fit_lasso, as.matrix(te_df), type = "response"),
         pred_ridge = predict(fit_ridge, as.matrix(te_df), type = "response"),
         pred_rf = predict(fit_rf, te_df)$predictions,
         pred_ext = predict(fit_ext, te_df)$predictions) %>% 
  mutate(pred_rf = if_else(pred_rf < .025, .025, pred_rf),
         pred_rf = if_else(pred_rf > .975, .975, pred_rf),
         pred_ext = if_else(pred_ext < .025, .025, pred_ext),
         pred_ext = if_else(pred_ext > .975, .975, pred_ext)) %>% 
  mutate(Pred = (pred_xgb + pred_glm + pred_lasso + pred_ridge + pred_ext ) / 5)

validate(submit)
validate_y(submit)
  

sub <- submit %>% 
  select(ID, Pred) %>% 
  arrange(ID)
sub %>% 
  write_csv("data/submit/first.csv")