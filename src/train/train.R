# training

library(tidyverse)
library(xgboost)
library(recipes)
library(glmnet)
library(ranger)

tmp <- read_csv("data/tmp.csv")
features <- read.csv("data/train/features.csv", stringsAsFactors = FALSE) %>% 
  group_by(ID) %>% 
  mutate(rating_sum = sum((massey_r_diff > 0) + 
                            (od_diff > 0)+
                            (elo_all_diff > 0)+
                            (elo_r_diff > 0)+
                            (RPI_diff > 0)+
                            (colley_r_diff > 0)+
                            (markov_win_diff > 0)+
                            (nmf > 0)+
                            (keener_scorediff_diff > 0))) %>% 
  ungroup()
sample <- read.csv("data/SampleSubmissionStage1.csv", 
                   stringsAsFactors = FALSE) %>% 
  as_tibble()
seeds <- read_csv("data/processed/seed.csv")
source("src/function/validate.R")

tr <- features %>% 
  filter(as.integer(str_sub(ID, 1, 4)) < 2014,
         # as.integer(str_sub(ID, 1, 4)) > 2002
         ) 
te <- features %>% 
  semi_join(sample, by = "ID") %>% 
  arrange(ID)

tr_df <- tr %>% 
  select(-ID, -target)

tr_lab <- tr$target

te_df <- te %>% 
  select(-ID, -target) 

# recipes----
set.seed(1)
rec_PCA = recipe( ~ ., data = tr_df) %>% 
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  step_pca(all_numeric(), threshold = .9) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  prep()
set.seed(1)
rec2 = recipe( ~ ., data = tr_df) %>% 
  step_corr(all_numeric(), - massey_r_diff, threshold = .99) %>%
  step_center(all_numeric()) %>% 
  step_scale(all_numeric()) %>% 
  prep()

tr_rec_PCA <- bake(rec_PCA, tr_df) 
te_rec_PCA <- bake(rec_PCA, te_df)
tr_rec <- bake(rec2, tr_df)
te_rec <- bake(rec2, te_df)
dtrain <- xgb.DMatrix(as.matrix(tr_rec),
                      label =  tr_lab)
dtest <- xgb.DMatrix(as.matrix(te_rec),
                     label = te$target)
# gbtree----
param <- list(max_depth = 1,
              min_child_weight = 2,
              colsample_bytree = 0.9,
              subsample = 0.9,
              eta = .02,
              silent = 1, 
              booster = "gbtree",
              objective = "binary:logistic",
              eval_metric = "logloss",
              nthread = 1)

set.seed(1)
cv <- xgb.cv(params = param, dtrain, nrounds = 10000, nfold = 20,
             early_stopping_rounds = 100,
             prediction = TRUE)

set.seed(1)
bst <- xgb.train(params = param, dtrain, nrounds = cv$best_iteration)


# gblinear----

paraml <- list(eta = .01,
               silent = 1, 
               lambda = 0,
               lambda_bias = 0,
               alpha = 0,
               booster = "gblinear",
               objective = "binary:logistic",
               eval_metric = "logloss",
               nthread = 1)

set.seed(1)
cv_l <- xgb.cv(params = paraml, dtrain, nrounds = 10000, nfold = 20,
               early_stopping_rounds = 20,
               prediction = TRUE)

set.seed(1)
linear <- xgb.train(params = paraml, dtrain, nrounds = cv_l$best_iteration)

# dart----

# paramd <- list(max_depth = 1,
#                min_child_weight = 2,
#                eta = .02,
#                silent = 10, 
#                booster = "dart",
#                objective = "binary:logistic",
#                eval_metric = "logloss",
#                nthread = 1)
# 
# set.seed(1)
# cv_d <- xgb.cv(params = paramd, dtrain, nrounds = 10000, nfold = 20,
#                early_stopping_rounds = 10,
#                prediction = TRUE)
# 
# set.seed(1)
# dart <- xgb.train(params = paramd, dtrain, nrounds = cv_d$best_iteration)


# impo----
submit <- tibble(ID = te$ID,
                 Pred_l = predict(linear, dtest),
                 Pred_b = predict(bst, dtest),
                 # Pred_d = predict(dart, dtest),
                 Pred = (Pred_l + Pred_b)/ 2)

validate(submit)
validate_y(submit)
xgb.importance(colnames(dtrain), bst)
xgb.importance(colnames(dtrain), dart)
xgb.importance(colnames(dtrain), linear)

# linear model----

fit_glm <- glm(tr_lab ~ . -1,
               data = tr_rec_PCA,
               family = binomial) 
summary(fit_glm)
library(glmnet)

set.seed(1)
cvfit <- cv.glmnet(x = as.matrix(tr_rec), y = tr_lab, family = "binomial", standardize = TRUE,
                   alpha = 1,
                   nfolds = 20)
fit_lasso <- glmnet(x = as.matrix(tr_rec), y = tr_lab, family = "binomial", standardize = TRUE,
                    alpha = 1,
                    lambda = cvfit$lambda.min)

set.seed(1)
cvfit <- cv.glmnet(x = as.matrix(tr_rec), y = tr_lab, family = "binomial", standardize = TRUE,
                   alpha = 0,
                   nfolds = 20)
fit_ridge <- glmnet(x = as.matrix(tr_rec), y = tr_lab, family = "binomial", standardize = TRUE,
                    alpha = 0,
                    lambda = cvfit$lambda.min)

library(ranger)

set.seed(1)
fit_rf <- ranger(tr_lab ~ .,
                 data = tr_rec,
                 num.trees = 2000,
                 mtry = 1,
                 # probability = TRUE,
                 importance = "impurity")

set.seed(1)
fit_ext <- ranger(tr_lab ~ .,
                  data = tr_rec,
                  mtry = 1,
                  num.trees = 2000,
                  importance = "permutation",
                  splitrule = "extratrees")


submit <- submit %>% 
  mutate(pred_glm = predict(fit_glm, newdata = te_rec_PCA, type = "response"),
         pred_lasso = predict(fit_lasso, as.matrix(te_rec), type = "response"),
         pred_ridge = predict(fit_ridge, as.matrix(te_rec), type = "response"),
         pred_rf = predict(fit_rf, te_rec)$predictions,
         pred_ext = predict(fit_ext, te_rec)$predictions) %>% 
  mutate(pred_rf = if_else(pred_rf < .025, .025, pred_rf),
         pred_rf = if_else(pred_rf > .975, .975, pred_rf),
         pred_ext = if_else(pred_ext < .025, .025, pred_ext),
         pred_ext = if_else(pred_ext > .975, .975, pred_ext),
         Pred_l = predict(linear, dtest),
         Pred_b = predict(bst, dtest),
         # Pred_d = predict(dart, dtest)
         ) %>% 
  mutate(Pred = (pred_lasso + pred_ridge + pred_glm +
                   pred_ext + pred_rf + 
                   Pred_l #+ Pred_b
                 ) / 6)




validate(submit)
validate_y(submit)
sub <- submit %>% 
  select(ID, Pred) %>% 
  arrange(ID)
sub %>%
  write_csv("data/submit/first.csv")

# boruta----

library(Boruta)
set.seed(111)
res_boruta <- Boruta(tr_lab ~ . -1,
                     data = tr_df,
                     doTrace = 2,
                     maxRuns = 500)
print(res_boruta)
plot(res_boruta)
attStats(res_boruta) %>%
  rownames_to_column("feature") %>%
  arrange(desc(meanImp))
plotImpHistory(res_boruta)

x <- 0.466
(x * 63 + (0 * log(0.4) + 1*log(0.6)))/63
