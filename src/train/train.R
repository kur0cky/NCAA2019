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
seeds <- read_csv("data/datafiles/NCAATourneySeeds.csv")
source("src/function/validate.R")

tr <- features %>% 
  filter(as.integer(str_sub(ID, 1, 4)) < 2014,
         as.integer(str_sub(ID, 1, 4)) > 1986)
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
  step_pca(all_numeric(), threshold = .9) %>%
  # step_ica(all_numeric(), num_comp = 3) %>% 
  prep()

tr_rec <- bake(rec, tr_df) %>% 
  select(-PC2)
te_rec <- bake(rec, te_df) %>% 
  select(-PC2)
# xgb----

dtrain <- xgb.DMatrix(as.matrix(tr_df),
                      label =  tr_lab)
dtest <- xgb.DMatrix(as.matrix(te_df),
                     label = te$target)

set.seed(1)
param <- list(max_depth = 3,
              eta = .01,
              silent = 1, 
              lambda = .5,
              objective = "binary:logistic", eval_metric = "logloss")

# N = nrow(dtrain)
# fold5list = c(
#   rep( 1, floor(N/5) ),
#   rep( 2, floor(N/5) ),
#   rep( 3, floor(N/5) ),
#   rep( 4, floor(N/5) ),
#   rep( 5, N - 4*floor(N/5) )
# )
# 
# iteration_count = c()
# smooth_model = list()
# 
# for (i in 1:10) {
#   
#   ### Resample fold split
#   set.seed(i)
#   folds = list()  
#   fold_list = sample(fold5list)
#   for (k in 1:5) folds[[k]] = which(fold_list == k)
#   
#   set.seed(120)
#   xgb_cv = 
#     xgb.cv(
#       params = param,
#       data = dtrain,
#       nrounds = 3000,
#       verbose = 0,
#       nthread = 12,
#       folds = folds,
#       early_stopping_rounds = 25,
#       maximize = FALSE,
#       prediction = TRUE
#     )
#   iteration_count = c(iteration_count, xgb_cv$best_iteration)
#   
#   ### Fit a smoothed GAM model on predicted result point differential to get probabilities
#   smooth_model[[i]] = smooth.spline(x = xgb_cv$pred, y = tr_lab)
#   
# }


cv <- xgb.cv(params = param, dtrain, nrounds = 1000, nfold = 20,
             early_stopping_rounds = 10)

bst <- xgb.train(params = param, dtrain, nrounds = cv$best_iteration)

# probs = list()
# for (i in 1:10) {
#   preds = predict(bst, dtest)
#   probs[[i]] = predict(smooth_model[[i]], preds)$y
# }

submit <- tibble(ID = te$ID,
                 Pred = predict(bst, dtest))

validate(submit)

xgb.importance(colnames(dtrain), bst)



fit_glm <- glm(tr_lab ~ . -1,
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

set.seed(1)
fit_rf <- ranger(tr_lab ~ .,
                 data = tr_df,
                 num.trees = 2000,
                 mtry = 1,
                 # probability = TRUE,
                 importance = "impurity")
fit_ext <- ranger(tr_lab ~ .,
                  data = tr_df,
                  mtry = 1,
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
  mutate(Pred = (pred_glm + pred_lasso + pred_ridge + pred_ext + pred_xgb + pred_rf ) / 6)



submit1 <- submit %>% 
  mutate(Season = as.integer(str_sub(ID, 1, 4)),
         team1 = as.integer(str_sub(ID, 6,9)),
         team2 = as.integer(str_sub(ID, 11,14))) %>% 
  left_join(seeds, by = c("Season", "team1" = "TeamID")) %>% 
  left_join(seeds, by = c("Season", "team2" = "TeamID")) %>% 
  rename(Seed1 = Seed.x, Seed2 = Seed.y) %>% 
  mutate(Seed1 = as.integer(str_sub(Seed1, 2,3)),
         Seed2 = as.integer(str_sub(Seed2, 2,3)))
  

submit1$Pred[submit1$Seed1 == 16 & submit1$Seed2 == 1] = 0.001
# submit1$Pred[submit1$Seed1 == 15 & submit1$Seed2 == 2] = 0.001
# submit1$Pred[submit1$Seed1 == 14 & submit1$Seed2 == 3] = 0.001
# submit1$Pred[submit1$Seed1 == 13 & submit1$Seed2 == 4] = 0.001
submit1$Pred[submit1$Seed1 == 1 & submit1$Seed2 == 16] = 0.999
# submit1$Pred[submit1$Seed1 == 2 & submit1$Seed2 == 15] = 0.999
# submit1$Pred[submit1$Seed1 == 3 & submit1$Seed2 == 14] = 0.999
# submit1$Pred[submit1$Seed1 == 4 & submit1$Seed2 == 13] = 0.999
submit1 <- submit1 %>% 
  select(-Seed1, -Seed2, -Season, -team1, -team2)
validate(submit1)
validate(submit)

validate_y(submit1)
validate_y(submit)
sub <- submit %>% 
  select(ID, Pred) %>% 
  arrange(ID)
# sub %>% 
#   write_csv("data/submit/first.csv")
# 
# submit %>% 
#   inner_join(target) %>% 
#   transmute(ID,
#             target,
#             diff = abs(Pred- 0.5)) %>% 
#   arrange(diff)


# boruta----

# library(Boruta)
# set.seed(111)
# res_boruta <- Boruta(tr_lab ~ . -1,
#                      data = tr_rec,
#                      doTrace = 2, 
#                      maxRuns = 500)
# print(res_boruta)
# plot(res_boruta)
# attStats(res_boruta) %>% 
#   rownames_to_column("feature") %>% 
#   arrange(desc(meanImp))
# plotImpHistory(res_boruta)
