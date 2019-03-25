# training 
# practice of cross validation using rsample

library(tidyverse)
library(tidymodels)

# training

library(tidyverse)
library(xgboost)
library(recipes)
library(glmnet)
library(ranger)


features <- read.csv("data/train/features.csv", stringsAsFactors = FALSE) %>% 
  filter(str_sub(ID, 1, 4) != "2019") %>% 
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

tr <- features %>% 
  drop_na()
tr <- bind_rows(
  tr,
  tr %>% 
    mutate(target = 1-target) %>% 
    mutate_at(vars(everything(), -target, -ID), function(x) -x)
)

tr_df <- tr %>% 
  select(-ID, -target)

# glm
for(i in 2:10){
  set.seed(1)
  rec <- recipe(target ~ . - ID, data = tr) %>% 
    step_center(all_numeric(), -all_outcomes()) %>% 
    step_scale(all_numeric(), -all_outcomes()) %>% 
    step_pca(all_numeric(), -all_outcomes(), -massey_r_diff, num_comp = i) %>% 
    step_center(all_numeric(), -all_outcomes()) %>% 
    step_scale(all_numeric(), -all_outcomes()) 
  
  df_cv <- tr %>% 
    mutate(Season = str_sub(ID, 1, 4)) %>% 
    group_vfold_cv(., 34, group = "Season")
  
  tmp <- df_cv %>% 
    mutate(rec = map(splits, prepper, rec))
  
  pred_func <- function(split_obj, rec_obj, model_obj, ...) {
    df_pred <- bake(rec_obj, assessment(split_obj)) 
    df_pred <- df_pred %>% 
      mutate(Pred = predict(model_obj, df_pred, type = "response")) %>% 
      select(target, Pred)
    return(df_pred)
  }
  
  
  tmp2 <- tmp %>% 
    mutate(fit_glm = map(rec, ~ glm(target ~ . , 
                                    data = dplyr::select(juice(.x), -ID), 
                                    family = binomial)),
           predicted = pmap(list(splits, rec, fit_glm),
                            pred_func),
           score = map_dbl(predicted, ~ .x %>% 
                             summarise(score = -mean(target * log(Pred) + (1-target) * log(1-Pred))) %>% 
                             unlist()))
  tmp2 %>% 
    select(score) %>% 
    summarise(max = max(score),
              mean = mean(score),
              median = median(score),
              min = min(score),
              sd = sd(score)) %>% 
    print()
  print(i)

}

# randomforest
for(i in 3){
  set.seed(1)
  
  df_cv <- tr %>% 
    mutate(Season = str_sub(ID, 1, 4)) %>% 
    group_vfold_cv(., 34, group = "Season")
  
  fit_func <- function(split){
    df <- analysis(split)
    fit <- ranger(target ~ .-ID-Season,
                  data = df,
                  mtry = i,
                  num.trees = 2000,
                  classification = T,
                  probability = T,
                  importance = "impurity") 
    return(fit)
  }
  
  pred_func <- function(fit, split){
    df <- assessment(split)
    df %>% 
      select(target) %>% 
      mutate(Pred = predict(fit, df)$predictions[,1],
             Pred = if_else(Pred < 0.025, 0.025, Pred),
             Pred = if_else(Pred > 0.975, 0.975, Pred))
  }
  
  tmp2 <- tmp %>% 
    mutate(fit = map(splits, fit_func),
           predicted = pmap(list(fit, splits), pred_func),
           score = map_dbl(predicted, ~ .x %>% 
                             summarise(score = -mean(target * log(Pred) + (1-target) * log(1-Pred))) %>% 
                             unlist()))
  tmp2 %>% 
    select(score) %>% 
    summarise(max = max(score),
              mean = mean(score),
              median = median(score),
              min = min(score),
              sd = sd(score)) %>% 
    print()
  print(i)
  gc()
}

# extratrees
for(i in 1){
  df_cv <- tr %>% 
    mutate(Season = str_sub(ID, 1, 4)) %>% 
    group_vfold_cv(., 16, group = "Season")
  
  fit_func <- function(cv){
    df = analysis(cv)
    fit <- ranger(target ~ . - ID - Season,
                  data = df,
                  mtry = 1,
                  num.trees = 1000)
    return(fit)
  }
  pred_func <- function(fit, split){
    df <- assessment(split)
    df %>% 
      select(target) %>% 
      mutate(Pred = predict(fit, df)$predictions[,1],
             Pred = if_else(Pred < 0.025, 0.025, Pred),
             Pred = if_else(Pred > 0.975, 0.975, Pred))
  }
  set.seed(1)
  tmp2 <- df_cv %>% 
    mutate(fit = map(splits, ~ ranger(target ~ .-ID - Season,
                                      data = analysis(.x),
                                      mtry = i,
                                      num.trees = 2000,
                                      classification = T,
                                      probability = T,
                                      splitrule = "extratrees")),
           predicted = map2(fit, splits, ~ pred_func(.x,.y)),
           score = map_dbl(predicted, ~ .x %>% 
                             summarise(score = -mean(target * log(Pred) + (1-target) * log(1-Pred))) %>% 
                             unlist()))
  tmp2 %>% 
    select(score) %>% 
    summarise(max = max(score),
              mean = mean(score),
              median = median(score),
              min = min(score),
              sd = sd(score)) %>% 
    print()
  print(i)
}

# lasso
for(i in 0.004444){
  df_cv <- tr %>% 
    mutate(Season = str_sub(ID, 1, 4)) %>% 
    group_vfold_cv(., 16, group = "Season")
  
  fit_func <- function(cv){
    df = analysis(cv) %>% 
      select(-Season, -ID)
    fit <- glmnet(x = as.matrix(select(df, -target)), 
                  y = select(df, target)[,1], 
                  family = "binomial", 
                  standardize = TRUE,
                  alpha = 1,
                  lambda = i)
    return(fit)
  }
  pred_func <- function(fit, split){
    df <- assessment(split) %>% 
      select(-Season, -ID, -target)
    res <- assessment(split) %>% 
      select(target) %>% 
      mutate(Pred = c(predict(fit, as.matrix(df), type = "response")))
    return(res)
  }
  set.seed(1)
  tmp2 <- df_cv %>% 
    mutate(fit = map(splits, fit_func),
           predicted = map2(fit, splits, ~ pred_func(.x,.y)),
           score = map_dbl(predicted, ~ .x %>% 
                             summarise(score = -mean(target * log(Pred) + (1-target) * log(1-Pred))) %>% 
                             unlist()))
  tmp2 %>% 
    select(score) %>% 
    summarise(max = max(score),
              mean = mean(score),
              median = median(score),
              min = min(score),
              sd = sd(score)) %>% 
    print()
  print(i)
}

# ridge
for(i in 0.0205){
  df_cv <- tr %>% 
    mutate(Season = str_sub(ID, 1, 4)) %>% 
    group_vfold_cv(., 16, group = "Season")
  
  fit_func <- function(cv){
    df = analysis(cv) %>% 
      select(-Season, -ID)
    fit <- glmnet(x = as.matrix(select(df, -target)), 
                  y = select(df, target)[,1], 
                  family = "binomial", 
                  standardize = TRUE,
                  alpha = 0,
                  lambda = i)
    return(fit)
  }
  pred_func <- function(fit, split){
    df <- assessment(split) %>% 
      select(-Season, -ID, -target)
    res <- assessment(split) %>% 
      select(target) %>% 
      mutate(Pred = c(predict(fit, as.matrix(df), type = "response")))
    return(res)
  }
  set.seed(1)
  tmp2 <- df_cv %>% 
    mutate(fit = map(splits, fit_func),
           predicted = map2(fit, splits, ~ pred_func(.x,.y)),
           score = map_dbl(predicted, ~ .x %>% 
                             summarise(score = -mean(target * log(Pred) + (1-target) * log(1-Pred))) %>% 
                             unlist()))
  tmp2 %>% 
    select(score) %>% 
    summarise(max = max(score),
              mean = mean(score),
              median = median(score),
              min = min(score),
              sd = sd(score)) %>% 
    # .$mean %>% 
    print()
  print(i)
}

# xgbl
for(i in 0.94){
  set.seed(1)
  rec <- recipe(target ~ . - ID, data = tr)%>% 
    step_corr(all_numeric(),-massey_r_diff, threshold = i) %>% 
    step_center(all_numeric()) %>% 
    step_scale(all_numeric())
  
  df_cv <- tr %>% 
    mutate(Season = str_sub(ID, 1, 4)) %>% 
    group_vfold_cv(., 16, group = "Season")
  
  tmp <- df_cv %>% 
    mutate(rec = map(splits, prepper, rec))
  
  fit_func <- function(cv, rec){
    dtrain <- xgb.DMatrix(juice(rec) %>% 
                            select(-ID, -target) %>% 
                            as.matrix(),
                          label =  analysis(cv) %>% 
                            select(target) %>% 
                            .[,1])
    dtest<- xgb.DMatrix(cv %>% 
                          assessment() %>% 
                          bake(rec,.) %>% 
                          select(-ID, -target) %>% 
                          as.matrix(),
                        label =  assessment(cv) %>% 
                          select(target) %>% 
                          .[,1])
    params <- list(eta = .01,
                   silent = 1, 
                   lambda = 0,
                   alpha = 0,
                   booster = "gblinear",
                   objective = "binary:logistic",
                   eval_metric = "logloss",
                   nthread = 1)
    watchlist <- list(train= dtrain, test = dtest)
    set.seed(1)
    cv <- xgb.train(params = params, data =dtrain, nrounds = 1000,
                   early_stopping_rounds = 30,verbose = F,
                   watchlist = watchlist,
                   prediction = TRUE)
    return(cv)
  }
  # pred_func <- function(fit, split){
  #   df <- assessment(split) %>% 
  #     select(-Season, -ID, -target)
  #   res <- assessment(split) %>% 
  #     select(target) %>% 
  #     mutate(Pred = c(predict(fit, as.matrix(df), type = "response")))
  #   return(res)
  # }
  set.seed(1)
  tmp2 <- tmp %>% 
    mutate(fit = map2(splits, rec, fit_func))
  tmp2 %>% 
    .$fit %>% 
    lapply(function(x) x$evaluation_log[x$best_iteration,]) %>% 
    do.call("rbind",.) %>% 
    # rownames_to_column("iter") %>% 
    summarise_at(vars("test_logloss"), funs(max, mean,median,min,sd)) %>% 
    # as_tibble() %>% 
    print()
  tmp2 %>%
    .$fit %>%
    lapply(function(x) x$evaluation_log[x$best_iteration,]) %>%
    do.call("rbind",.) %>%
    summary() %>% 
    print()
  print(i)
  # print(set_lambda_alpha[i,])
}

# glm
# Group 34-fold cross-validation 
# max  mean median   min     sd
# <dbl> <dbl>  <dbl> <dbl>  <dbl>
# 0.629 0.533  0.531 0.472 0.0382

# lasso
# max  mean median   min     sd
# <dbl> <dbl>  <dbl> <dbl>  <dbl>
# 0.538 0.428  0.428 0.369 0.0506

# ridge
# max  mean median   min     sd
# <dbl> <dbl>  <dbl> <dbl>  <dbl>
# 0.539 0.431  0.427 0.371 0.0477

# ranger
# max  mean median   min     sd
# <dbl> <dbl>  <dbl> <dbl>  <dbl>
# 0.526 0.443  0.445 0.366 0.0501

# extratrees
# max  mean median   min     sd
# <dbl> <dbl>  <dbl> <dbl>  <dbl>
# 0.533 0.438  0.439 0.380 0.0438

# gblinear
# max  mean median   min     sd
# max      mean   median      min         sd
# 0.55113 0.4205064 0.420061 0.356587 0.05042693


# svm----

library(kernlab)
for(i in 1:2){
  set.seed(1)
  rec <- recipe(target ~ . - ID, data = tr) %>% 
    step_bin2factor(target, all_outcomes()) %>% 
    step_center(all_numeric(), -all_outcomes()) %>% 
    step_scale(all_numeric(), -all_outcomes()) %>% 
    step_pca(all_numeric(), -all_outcomes(), -massey_r_diff, -rating_sum, num_comp = i) %>%
    step_center(all_numeric(), -all_outcomes()) %>% 
    step_scale(all_numeric(), -all_outcomes()) 
  
  df_cv <- tr %>% 
    mutate(Season = str_sub(ID, 1, 4)) %>% 
    group_vfold_cv(., 34, group = "Season")
  
  tmp <- df_cv %>% 
    mutate(rec = map(splits, prepper, rec))
  
  pred_func <- function(split_obj, rec_obj, model_obj, ...) {
    df_pred <- bake(rec_obj, assessment(split_obj)) 
    df_pred <- df_pred %>% 
      mutate(Pred = predict(model_obj, df_pred, type = "probabilities")[,1]) %>% 
      select(target, Pred)
    return(df_pred)
  }
  
  
  tmp2 <- tmp %>% 
    mutate(
      fit_glm = map(rec, ~ ksvm(target ~ . ,
                                data = dplyr::select(juice(.x), -ID),
                                prob.model = T)),
      predicted = pmap(list(splits, rec, fit_glm),
                       pred_func),
      score = map_dbl(predicted, ~ .x %>% 
                        mutate(target = 1-as.integer(target)) %>% 
                        summarise(score = -mean(target * log(Pred) + (1-target) * log(1-Pred))) %>% 
                        unlist()))
  tmp2 %>% 
    select(score) %>% 
    summarise(max = max(score),
              mean = mean(score),
              median = median(score),
              min = min(score),
              sd = sd(score)) %>% 
    print()
  print(i)
  
}

