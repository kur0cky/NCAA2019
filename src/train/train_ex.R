# feature_matrix
# create train data

# data/train/featuresから始まって
# trainとtestに分けて
# submitまで

library(tidyverse)
library(xgboost)
library(recipes)

tmp <- read_csv("data/tmp.csv")
features <- read.csv("data/train/features.csv", stringsAsFactors = FALSE) %>% 
  left_join(tmp, by = "ID")
sample <- read.csv("data/SampleSubmissionStage1.csv", 
                   stringsAsFactors = FALSE) %>% 
  as_tibble()
seeds <- read_csv("data/datafiles/NCAATourneySeeds.csv")
source("src/function/validate.R")

tr <- features %>% 
  filter(as.integer(str_sub(ID, 1, 4)) < 2014,
         as.integer(str_sub(ID, 1, 4)) > 2002)
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
param <- list(max_depth = 1,
              eta = .01,
              silent = 1, 
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
#                      data = tr_df,
#                      doTrace = 2,
#                      maxRuns = 500)
# print(res_boruta)
# plot(res_boruta)
# attStats(res_boruta) %>%
#   rownames_to_column("feature") %>%
#   arrange(desc(meanImp))
# plotImpHistory(res_boruta)

# feature     meanImp   medianImp     minImp     maxImp    normHits  decision
# 1          massey_r_diff 12.08300270 12.06386520 10.1709490 13.9683382 1.000000000 Confirmed
# 2            elo_3r_diff 11.94616371 11.93238966  9.6833412 14.1014561 1.000000000 Confirmed
# 3               RPI_diff 11.71558270 11.70333644  9.4150205 13.5569196 1.000000000 Confirmed
# 4          colley_r_diff 11.33281933 11.33330522  9.3243367 13.4411868 1.000000000 Confirmed
# 5         elo_score_diff 11.23694282 11.22191289  9.4976158 13.7980279 1.000000000 Confirmed
# 6                od_diff 11.05496829 11.07062302  8.6843808 13.0072215 1.000000000 Confirmed
# 7         basis_nmf_diff 10.41226438 10.41487945  8.0525739 12.5408323 1.000000000 Confirmed
# 8          coef_nmf_diff  9.81127546  9.84832926  7.2504283 11.6502709 1.000000000 Confirmed
# 9                    nmf  9.75847659  9.75046417  8.1458089 11.6988381 1.000000000 Confirmed
# 10            elo_r_diff  8.86321370  8.88961118  6.6958305 11.2495067 1.000000000 Confirmed
# 11       Pyth_ratio_diff  8.71246549  8.70850415  6.6299535 10.4707586 1.000000000 Confirmed
# 12         log_seed_diff  8.06586244  8.06457698  6.1577877  9.8102146 0.997995992 Confirmed
# 13           seed_diff.y  7.92394419  7.92867174  6.2031761  9.7880926 0.997995992 Confirmed
# 14           seed_diff.x  7.91666917  7.87605491  6.1436922  9.6913770 0.997995992 Confirmed
# 15            massey_r.y  6.95935379  6.97422756  4.5472232  9.2304900 0.997995992 Confirmed
# 16              Team1APD  6.71227604  6.72096675  4.2594106  8.8194613 0.993987976 Confirmed
# 17          elo_all_diff  6.39861770  6.38330295  4.1334409  8.1146619 0.987975952 Confirmed
# 18          Team1WinPerc  6.35060108  6.35594702  3.6506279  8.8277877 0.987975952 Confirmed
# 19         Team1PPPAllow  6.01579302  6.00663124  3.1454586  8.4281393 0.983967936 Confirmed
# 20    elo_score_all_diff  5.83949526  5.84903899  3.5205437  7.9446978 0.981963928 Confirmed
# 21            massey_r.x  5.34111280  5.36029647  2.9290066  7.3767793 0.973947896 Confirmed
# 22       markov_win_diff  5.30742756  5.36581982  3.0345517  7.5048950 0.963927856 Confirmed
# 23 keener_scorediff_diff  4.65666976  4.65125624  1.4225419  6.7788409 0.909819639 Confirmed
# 24              Team2APD  4.39832814  4.45166040  1.0745730  6.7581537 0.867735471 Confirmed
# 25 keener_scorerate_diff  4.31920596  4.37662713  1.2657921  6.4191507 0.855711423 Confirmed
# 26          Team2WinPerc  3.38874491  3.41299037 -0.4126636  6.1534239 0.661322645 Confirmed
# 27              Team2PPP  3.35127230  3.35460587  0.4347644  6.1478134 0.627254509 Confirmed
# 28              Team1PPP  3.03697243  3.03598008 -0.1755569  5.6237045 0.555110220 Tentative
# 29        team1_seed_str  2.64955879  2.69745090  0.5018649  4.1744091 0.448897796 Tentative
# 30        team2_seed_str  0.84180331  0.82152461 -1.1019458  2.0462806 0.002004008  Rejected
# 31         Team2PPPAllow  0.60540956  0.63984766 -0.6836567  1.7208418 0.000000000  Rejected
# 32     Team2AvgPossAllow  0.07097896 -0.00861208 -2.1634156  2.1900982 0.000000000  Rejected
# 33     Team1AvgPossAllow -0.11230283 -0.01213112 -1.3788578  0.6077951 0.000000000  Rejected
# 34          Team2AvgPoss -0.17514517 -0.34665071 -2.0649920  2.4756841 0.002004008  Rejected
# 35          Team1AvgPoss -1.06081128 -1.14877141 -3.0095692  1.1094253 0.000000000  Rejected