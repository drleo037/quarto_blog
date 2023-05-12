# double check what align does.
# (I don't want to accidentally introduce information leakage
# e.g. somewhere in an average or a rolling mean)
# align right means it only uses data to the left (earlier in time)
# note:I'm happy to leave rain_ea_mm in as rainfall is forecastable)
ea_tw_ts %>%
  mutate(tma = zoo::rollmean(rain_ea_mm, k = 3, fill = NA, align = "right")) %>%
  filter(date >= ymd("2023-01-01")) %>%
  ggplot() +
  geom_line(aes(x = date, y = rain_ea_mm), colour = "grey") +
  geom_line(aes(x = date, y = tma, colour = "red"))


# create a data-frame for modelling
# make sure we're not "leaking" and information
ea_tw_ts_features <- ea_tw_ts %>%
  arrange(date) %>%
  filter(date >= ymd("2023-01-01")) %>%
  mutate(stage_level_tm1_m = lag(stage_level_m, n = 1)) %>%
  mutate(stage_level_tm2_m = lag(stage_level_m, n = 2)) %>%
  select(-stage_level_m) %>%
  mutate(rain_ea_tm1_mm = lag(rain_ea_mm, n = 1)) %>%
  mutate(rain_ea_tm2_mm = lag(rain_ea_mm, n = 2)) %>%
  # I'm happy to leave in rainfall as forecasts of this can be purchased
  mutate(rain_ea_ma_mm = zoo::rollmean(rain_ea_mm, k = 3, fill = NA, align = "right")) %>%
  select(-discharging_edm_OL_count) %>%
  na.omit() # some lags will introduce NAs,  these can complicate modelling 

# View(ea_tw_ts_features)
names(ea_tw_ts_features)

set.seed(037)
df_split_rnd <- rsample::initial_split(ea_tw_ts_features, strata = discharging_edm_count) # note: stratify by Species
df_train_rnd <- rsample::training(df_split)
df_test_rnd  <- rsample::testing(df_split)
set.seed(037)
df_split <- rsample::initial_time_split(ea_tw_ts_features, strata = discharging_edm_count) # note: stratify by Species
df_train <- rsample::training(df_split)
df_test  <- rsample::testing(df_split)

# take the training data and make "copies"
# using V(=k) fold cross validation
set.seed(234)
df_folds <- vfold_cv(df_train, strata = discharging_edm_count)
# could use validation_split()

# lets have a look at where the train test splits have been taken from 
df_train %>%
  mutate(source = "train") %>%
  mutate(method = "initial_time_split") %>%
  bind_rows(
    df_test %>%
      mutate(source = "test") %>%
      mutate(method = "initial_time_split") 
  ) %>%
  bind_rows(
    df_train_rnd %>%
    mutate(source = "train") %>%
    mutate(method = "initial_split") 
  ) %>%
  bind_rows(
    df_test_rnd %>%
      mutate(source = "test") %>%
      mutate(method = "initial_split") 
  ) %>%
  ggplot() +
  geom_line(aes(x = date, y = discharging_edm_count), colour = "lightgrey") +
  geom_point(aes(x = date, y = discharging_edm_count, colour = source)) +
  facet_wrap( ~ method)


# r list the full models parameters in descending significance }
model_reg_lm <- lm(discharging_edm_count ~ . - date, data=df_train)
# r inspect full model}
model_reg_lm %>%
  broom::tidy() %>% arrange(p.value)

# r list the simplified models parameters in descending significance}
model_reg_lm_simplified <- MASS::stepAIC(model_reg_lm, direction = "both", trace = FALSE)
# have a look at this model 
model_reg_lm_simplified %>%
  broom::tidy() %>% arrange(p.value)

# r  compare the performance of the two lms}
performance::compare_performance(model_reg_lm, model_reg_lm_simplified, rank = TRUE)

# r visualise the lm model performance against the target variable, warning=FALSE, message=FALSE}
# creating ggplot object for visualization
df_test %>%
  bind_cols(predict(model_reg_lm, df_test) %>% 
              enframe(name = NULL, value = "full_prediction")
            ) %>%
  bind_cols(predict(model_reg_lm_simplified, df_test) %>% 
              enframe(name = NULL, value = "simplified_prediction")
            ) %>%
  select(discharging_edm_count, full_prediction, simplified_prediction) %>%
  pivot_longer(-c(discharging_edm_count), names_to = "model", values_to = "estimate") %>%
  ggplot(aes(discharging_edm_count, estimate)) +
  geom_point() +
  ggpubr::stat_regline_equation(aes(label =  paste(after_stat(eq.label),
                                                   after_stat(adj.rr.label), sep = "~~~~"))) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ model) 


## Regression using ML (Machine Learning)

# r decision tree regression directly using ranger}
set.seed(37) # setting a seed helps with reproducibility
# we're modelling Species as a function of everything...
model_reg_rf <- ranger::ranger(discharging_edm_count ~ . - date, 
                               data = df_train, 
#                               num.trees = ntrees_reg_rf,
                             # I've added the optional impurity
                             # so I check variable importance later
                               importance = "impurity" 
)
model_reg_rf
# r scatterplot of ranger model, warning=FALSE, message=FALSE}
# then do a quick check on the output
pred.edm_rf <- stats::predict(model_reg_rf, data = df_test)
bind_cols(df_test$discharging_edm_count, pred.edm_rf$predictions) %>%
  rename(actual = ...1, pred = ...2) %>%
  ggplot(aes(actual, pred)) +
  geom_point() +
  ggpubr::stat_regline_equation(aes(label =  paste(after_stat(eq.label),
                                                   after_stat(adj.rr.label), sep = "~~~~")), size = 4) + 
  geom_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0)

# r variable importance in the 1st go of ML RF model }
model_reg_rf %>%
  vip::vip(num_features = 20,  aesthetics = list(color = "grey50", fill = "lightblue"))


## Regression using XGBOOST

# r fit an xgboost regression model }
# xgboost regression (works,  but the SQL needs extra work to convert into excel) ----
library(parsnip)

model_reg_xgboost <- boost_tree(mode = "regression") %>%
  set_engine("xgboost") %>%
  fit(discharging_edm_count ~ . - date, data = df_train)

# r important variables according to the xgboost model}
model_reg_xgboost %>% 
      vip::vip(num_features = 20,  aesthetics = list(color = "grey50", fill = "lightblue"))

# r scatterplot ofxgboost model, warning=FALSE, message=FALSE}
# then do a quick check on the output
pred.edm_xgb <- predict(model_reg_xgboost, df_test)
bind_cols(df_test$discharging_edm_count, pred.edm_xgb) %>%
  rename(actual = ...1, pred = .pred) %>%
  ggplot(aes(actual, pred)) +
  geom_point() +
  ggpubr::stat_regline_equation(aes(label =  paste(after_stat(eq.label),
                                                   after_stat(adj.rr.label), sep = "~~~~"))) + 
  geom_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0) 
## Summary of regression models

### Plots of actual and modelled estimates of discharging_edm_count

# r compare modelled and actual for each regression model, warning=FALSE, message=FALSE}

# TEST creating ggplot object for visualization
models_test <- df_test %>%
  select(discharging_edm_count) %>%
  bind_cols(predict(model_reg_lm, df_test) %>% 
              enframe(name = NULL, value = "full_lm")
  ) %>%
  bind_cols(predict(model_reg_lm_simplified, df_test) %>% 
              enframe(name = NULL, value = "simplified_lm")
  ) %>%
  bind_cols(pred.edm_rf$predictions) %>%
  rename(Random_Forest = ...4) %>%
  bind_cols(pred.edm_xgb) %>%
  rename(XGBoost = .pred)

models_test %>%
  pivot_longer(-c(discharging_edm_count), names_to = "model", values_to = "estimate") %>%
  ggplot(aes(discharging_edm_count, estimate)) +
  geom_point() +
  ggpubr::stat_regline_equation(aes(label =  paste(after_stat(eq.label),
                                                   after_stat(adj.rr.label), sep = "~~~~")), size = 4) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ model)


# TEST creating ggplot object for visualization
models_train <- df_train %>%
  select(discharging_edm_count) %>%
  bind_cols(predict(model_reg_lm, df_train) %>% 
              enframe(name = NULL, value = "full_lm")
  ) %>%
  bind_cols(predict(model_reg_lm_simplified, df_train) %>% 
              enframe(name = NULL, value = "simplified_lm")
  ) %>%
  bind_cols(stats::predict(model_reg_rf, data = df_train)$predictions) %>%
  rename(Random_Forest = ...4) %>%
  bind_cols(predict(model_reg_xgboost, df_train)) %>%
  rename(XGBoost = .pred)

models_test %>%
  mutate(data_source = "test_df") %>%
  bind_rows(
    models_train %>%
      mutate(data_source = "train_df")
  ) %>%
  pivot_longer(-c(discharging_edm_count, data_source), names_to = "model", values_to = "estimate") %>%
  ggplot(aes(x = discharging_edm_count, y = estimate, colour = data_source)) +
  geom_point() +
  ggpubr::stat_regline_equation(aes(label =  paste(after_stat(eq.label),
                                                   after_stat(adj.rr.label), sep = "~~~~")), size = 4) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ model)

### Variable Importance
# r variable importance  for each regression model}

vip::vi(model_reg_lm) %>% mutate(Model = "full lm", .before = 1) %>%
  bind_rows(
    vip::vi(model_reg_lm_simplified) %>% mutate(Model = "simplified lm", .before = 1)
  ) %>%
  bind_rows(
    vip::vi(model_reg_rf) %>% mutate(Model = "Random Forest", .before = 1)
  ) %>%
  bind_rows(
    vip::vi(model_reg_xgboost) %>% mutate(Model = "XGBoost", .before = 1)
  ) %>%
  mutate(Sign = coalesce(Sign, "Undefined")) %>%
  mutate(Importance = ifelse(Sign == "NEG", 0-Importance, Importance)) %>%
  ggplot(aes(x = Variable, y = Importance, fill = Sign)) +
  geom_col() +
  coord_flip() +
  facet_wrap( ~ Model, scales = "free_x") +
  labs(title = "This plot shows what each model relied on to estimate Petal Length",
       subtitle = "The linear model models work quite differently to the tree-based ones")


# tuning xgboost
library(tidymodels)
edm_rec <- 
  recipe(discharging_edm_count ~ ., data = df_train) %>%
  step_rm(date)


xgb_spec <-
  boost_tree(
    trees = tune(),
    min_n = tune(),
    mtry = tune(),
    learn_rate = 0.01
  ) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

xgb_wf <- workflow(edm_rec, xgb_spec)

# fine tuning

library(finetune)
doParallel::registerDoParallel()

set.seed(345)
tictoc::tic()
xgb_rs <- tune_race_anova(
  xgb_wf,
  resamples = df_folds,
  grid = 15, #  27:30 in a production environment you'd use more
  control = control_race(verbose_elim = TRUE)
)
tictoc::toc()

xgb_rs

collect_metrics(xgb_rs)

plot_race(xgb_rs)

xgb_last <- xgb_wf %>%
  finalize_workflow(select_best(xgb_rs, "rmse")) %>%
  last_fit(df_split)

xgb_last

collect_metrics(xgb_last)


collect_predictions(xgb_last) %>%
  ggplot() +
  geom_point(aes(discharging_edm_count, .pred))

library(vip)
xgb_last %>%
  extract_fit_engine() %>%
  vip()

if(F) {
  model_reg_xgboost %>%
    predict(df_test)

  xgb_last %>%
    extract_fit_parsnip() %>%
  predict(
    edm_rec %>%
      prep() %>%
      bake(df_test) %>%
      select(-discharging_edm_count)
  )

}


df_test %>%
  select(discharging_edm_count) %>%
  bind_cols(predict(model_reg_lm, df_test) %>% 
              enframe(name = NULL, value = "full_lm")
  ) %>%
  bind_cols(predict(model_reg_lm_simplified, df_test) %>% 
              enframe(name = NULL, value = "simplified_lm")
  ) %>%
  # add ranger model
  bind_cols(pred.edm_rf$predictions) %>% rename(Random_Forest = ...4) %>%
  # add unoptimised xgboost
  bind_cols(predict(model_reg_xgboost, df_test) %>% rename(XGBoost = .pred)) %>%
  # add optimised xgboost
  bind_cols(
    xgb_last %>%
      extract_fit_parsnip() %>%
      predict(
        edm_rec %>%
          prep() %>%
          bake(df_test) %>%
          select(-discharging_edm_count)
      ) %>% rename(XGBoost_optimised = .pred)
    ) %>%
  pivot_longer(-c(discharging_edm_count), names_to = "model", values_to = "estimate") %>%
  ggplot(aes(discharging_edm_count, estimate)) +
  geom_point() +
  ggpubr::stat_regline_equation(aes(label =  paste(after_stat(eq.label),
                                                   after_stat(adj.rr.label), sep = "~~~~")), size = 4) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ model)
