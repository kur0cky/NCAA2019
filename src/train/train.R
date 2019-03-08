# feature_matrix
# create train data

# data/train/featuresから始まって
# trainとtestに分けて
# submitまで

library(tidyverse)
library(xgboost)
library(recipes)

features <- read.csv("data/train/features.csv", stringsAsFactors = FALSE)
sample <- read.csv("data/SampleSubmissionStage1.csv", 
                   stringsAsFactors = FALSE) %>% 
  as_tibble()
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

# recipes----
rec = recipe( ~ ., data = tr_df) %>% 
  step_center(all_numeric()) %>% 
  step_scale(all_numeric()) %>% 
  step_pca(all_numeric(), threshold = .95) %>% 
  prep()

tr_rec <- bake(rec, tr_df)
te_rec <- bake(rec, te_df)
# xgb----

dtrain <- xgb.DMatrix(as.matrix(tr_df),
                      label =  tr_lab)
dtest <- xgb.DMatrix(as.matrix(te_df),
                     label = te$target)

set.seed(1)
param <- list(max_depth = 3, eta = .01, silent = 1, nthread = 2, 
              lambda = .2,
              objective = "binary:logistic", eval_metric = "logloss")
cv <- xgb.cv(params = param, dtrain, nrounds = 1000, nfold = 20,
             early_stopping_rounds = 2)

bst <- xgb.train(params = param, dtrain, nrounds = cv$best_iteration)
submit <- tibble(ID = te$ID,
                 Pred = predict(bst, dtest))

xgb.importance(colnames(dtrain), bst)



fit_glm <- glm(tr_lab ~ .,
               data = tr_rec,
               family = binomial)
library(glmnet)
cvfit <- cv.glmnet(x = as.matrix(tr_rec), y = tr_lab, family = "binomial", standardize = TRUE,
                   alpha = 1)
fit_lasso <- glmnet(x = as.matrix(tr_rec), y = tr_lab, family = "binomial", standardize = TRUE,
                    alpha = 1,
                    lambda = cvfit$lambda.min)
cvfit <- cv.glmnet(x = as.matrix(tr_rec), y = tr_lab, family = "binomial", standardize = TRUE,
                   alpha = 0)
fit_ridge <- glmnet(x = as.matrix(tr_rec), y = tr_lab, family = "binomial", standardize = TRUE,
                    alpha = 0,
                    lambda = cvfit$lambda.min)

library(ranger)

fit_rf <- ranger(tr_lab ~ .,
                 data = tr_df,
                 num.trees = 2000,
                 importance = "permutation")
fit_ext <- ranger(tr_lab ~ .,
                  data = tr_df,
                  num.trees = 2000,
                  importance = "permutation",
                  splitrule = "extratrees")

# # knn
# cv_knn <- c()
# for(i in (1:10) * 100){
#   a <- class::knn.cv(tr_rec, tr_lab, k = i, prob=TRUE)
#   tr %>% 
#     mutate(Pred = attr(a, "prob"),
#            Pred = if_else(Pred < .01, .01, Pred),
#            Pred = if_else(Pred > .99, .99, Pred)) %>% 
#     select(ID, Pred) %>% 
#     validate() %>% 
#     .$score %>% 
#     unlist() %>% 
#     print()
#   print(i)
# }

# # svm
# library(e1071)
# t <- tune.svm(tr_df, factor(tr_lab),
#               gamma = 10^(-3:-4), cost = 10^(1:3), 
#               tunecontrol = tune.control(cross = 10))
# fit_svm <- svm(tr_df, factor(tr_lab),
#                gamma = 10^(-4), cost = 10,
#                probability = T)
# predict(fit_svm, probability = TRUE) 

submit <- submit %>% 
  mutate(pred_xgb = predict(bst, dtest),
         pred_glm = predict(fit_glm, newdata = te_rec, type = "response"),
         pred_lasso = predict(fit_lasso, as.matrix(te_rec), type = "response"),
         pred_ridge = predict(fit_ridge, as.matrix(te_rec), type = "response"),
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