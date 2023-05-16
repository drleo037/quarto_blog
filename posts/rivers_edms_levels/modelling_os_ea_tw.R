library(tidymodels) # we will be fitting using tidy models

# we need data that gets loaded in ea_tw_ts
source(here::here("posts/rivers_edms_levels/exploring_os_ea_tw.R"), local = TRUE)

# double check what align does. ----
# (I don't want to accidentally introduce information leakage
# e.g. somewhere in an average or a rolling mean)
# align right means it only uses data to the left (earlier in time)
# note:I'm happy to leave rain_ea_mm in as rainfall is forecast-able)
ea_tw_ts %>%
  mutate(moving_avg = zoo::rollmean(rain_ea_mm, k = 3, fill = NA, align = "right")) %>%
  filter(date >= ymd("2023-01-01")) %>%
  select(date, rain_ea_mm, moving_avg) %>%
  pivot_longer(-c(date), names_to = "variable", values_to = "value") %>%
  ggplot() +
  geom_line(aes(x = date, y = value, colour = variable))  +
  labs(title = "Sanity check for information leakage when building a moving average",
       subtitle = "This plot is only top make sure the movign average doesnt include future data",
       caption = "EA data from: https://environment.data.gov.uk/flood-monitoring/archive",
       x = "Date", y = "rainfall (mm)")


# create a data-frame for modelling ----
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

# what we stated with
ea_tw_ts %>%
  filter(date >= ymd("2023-01-01")) %>%
  pivot_longer(cols = c(stage_level_m, rain_ea_mm, rain_mo_mm, discharging_edm_count, discharging_edm_OL_count),
               names_to = "measurand",
               values_to = "reading") %>%
  ggplot() +
  geom_line(aes(x = date, y = reading, colour = measurand)) +
  facet_wrap( ~ measurand, ncol = 1, scales = "free") +
  labs(title = "Historic rainfall and river levels", subtitle = "EA data filtered and summarised", caption = "data from EA historic archives", x = "date", y = "")

# View(ea_tw_ts_features)
names(ea_tw_ts_features)
# explore what we've reformatted as features ----
ea_tw_ts_features %>%
  filter(date >= ymd("2023-01-01")) %>%
  pivot_longer(cols = -c(date),
               names_to = "measurand",
               values_to = "reading") %>%
  ggplot() +
  geom_line(aes(x = date, y = reading, colour = measurand)) +
  facet_wrap( ~ measurand, ncol = 1, scales = "free") +
  labs(title = "Fearures that will be considered while modelling",
       subtitle = "many of these are highly correlated",
       caption = "data from EA historic archives and met-office",
       x = "date", y = "")

# split the data into training (inc v-folds for model validation) and testing ----
set.seed(037)
df_split_rnd <- rsample::initial_split(ea_tw_ts_features, strata = discharging_edm_count) # note: stratify by Species
df_train_rnd <- rsample::training(df_split_rnd)
df_test_rnd  <- rsample::testing(df_split_rnd)
set.seed(037)
df_split <- rsample::initial_time_split(ea_tw_ts_features, strata = discharging_edm_count) # note: stratify by Species
df_train <- rsample::training(df_split)
df_test  <- rsample::testing(df_split)

# take the training data and make "copies"
# using V(=k) fold cross validation
set.seed(234)
df_folds <- vfold_cv(df_train, strata = discharging_edm_count)
# could use validation_split()

# explore the splits between training and testing datasets  ----
bind_rows(
    df_train %>% mutate(source = "train"),
    df_test %>% mutate(source = "test")
  ) %>%
  mutate(method = "splitting by time") %>%
  bind_rows(
    bind_rows(
      df_train_rnd %>% mutate(source = "train"),
      df_test_rnd %>% mutate(source = "test")
      ) %>%
      mutate(method = "splitting randomly") 
    ) %>%
  ggplot() +
  geom_line(aes(x = date, y = discharging_edm_count), colour = "lightgrey") +
  geom_point(aes(x = date, y = discharging_edm_count, colour = source)) +
  facet_wrap( ~ method) + 
  labs(title = "This modelling excludes the most recent data from training",
       subtitle = "The performance on the test data is more like real-world performance",
       caption = "Data from Thames Water {api_root}/data/STE/v1/DischargeAlerts",
       x = "Date", y = "Count of EDMs discharging")


# linear model of all parameters (probably degenerate) -----
model_reg_lm <- lm(discharging_edm_count ~ . - date, data=df_train)
# r list the full models parameters in descending significance }
model_reg_lm %>%
  broom::tidy() %>% arrange(p.value)

# simplified linear model using fewer paramewters (still probably degenerate) -----
model_reg_lm_simplified <- MASS::stepAIC(model_reg_lm, direction = "both") #, trace = FALSE)
# r list the simplified models parameters in descending significance}
model_reg_lm_simplified %>%
  broom::tidy() %>% arrange(p.value)

# even simpler LM with less degeneracy
model_reg_lm_very_simplified <- lm(discharging_edm_count ~ rain_ea_ma_mm + rain_ea_mm + rain_ea_tm1_mm + rain_ea_tm2_mm, data=df_train)
# r list the full models parameters in descending significance }
model_reg_lm_very_simplified %>%
  broom::tidy() %>% arrange(p.value)

# r  compare the performance of the lms}
# low AIC is good, high BIC is good
performance::compare_performance(model_reg_lm, model_reg_lm_simplified, model_reg_lm_very_simplified, rank = TRUE)

## Regression using ML (Machine Learning) --------
# r decision tree regression directly using ranger} -----
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
if(F) {
  bind_cols(df_test$discharging_edm_count, pred.edm_rf$predictions) %>%
    rename(actual = ...1, pred = ...2) %>%
    ggplot(aes(actual, pred)) +
    geom_point() +
    ggpubr::stat_regline_equation(aes(label =  paste(after_stat(eq.label),
                                                     after_stat(adj.rr.label), sep = "~~~~")), size = 4) + 
    geom_smooth(method = "lm") +
    geom_abline(slope = 1, intercept = 0)
}

# r variable importance in the 1st go of ML RF model }
# model_reg_rf %>% vip::vip(num_features = 20,  aesthetics = list(color = "grey50", fill = "lightblue"))


## Regression using XGBOOST

# r fit an xgboost regression model }
# xgboost regression (works,  but the SQL needs extra work to convert into excel) ----

model_reg_xgboost <- boost_tree(mode = "regression") %>%
  set_engine("xgboost") %>%
  fit(discharging_edm_count ~ . - date, data = df_train)

# r important variables according to the xgboost model}
# model_reg_xgboost %>%  vip::vip(num_features = 20,  aesthetics = list(color = "grey50", fill = "lightblue"))

# r scatterplot ofxgboost model, warning=FALSE, message=FALSE}
# then do a quick check on the output
pred.edm_xgb <- predict(model_reg_xgboost, df_test)
if(F) {
  bind_cols(df_test$discharging_edm_count, pred.edm_xgb) %>%
    rename(actual = ...1, pred = .pred) %>%
    ggplot(aes(actual, pred)) +
    geom_point() +
    ggpubr::stat_regline_equation(aes(label =  paste(after_stat(eq.label),
                                                     after_stat(adj.rr.label), sep = "~~~~"))) + 
    geom_smooth(method = "lm") +
    geom_abline(slope = 1, intercept = 0) 
}
## Summary of regression models

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
  set_engine("xgboost") %>% # can add validattio = 0.2 ,  see: https://www.youtube.com/watch?v=OMn1WCNufo8
  set_mode("regression")

# or tune all the parameters
xgb_spec <-
  boost_tree(
    trees = tune(),
    tree_depth = tune(), min_n = tune(),
    loss_reduction = tune(),                     ## first three: model complexity
    sample_size = tune(), mtry = tune(),         ## randomness
    learn_rate = tune()                          ## step size
  ) %>%
  set_engine("xgboost") %>% # can add validattio = 0.2 ,  see: https://www.youtube.com/watch?v=OMn1WCNufo8
  set_mode("regression")


xgb_wf <- workflow(edm_rec, xgb_spec)

# fine tuning

library(finetune)
doParallel::registerDoParallel()
RACE_GRID_SIZE <- 50
SEARCH_GRID_SIZE <- 50
set.seed(345)
message("race optimisation takes about 60 seconds....")
if(T) {
  tictoc::tic()
  xgb_rs <- tune_race_anova(
    xgb_wf,
    resamples = df_folds,
    grid = RACE_GRID_SIZE, #  27:30 in a production environment you'd use more
    control = control_race(verbose_elim = TRUE)
  )
  tictoc::toc() # maybe 20 seconds?
  # have a look at how the tuning went
  # and how some eavenues were truncated early
  plot_race(xgb_rs)
  
  # get the best model
  xgb_best_race <- xgb_wf %>%
    # finalise the wf to stop being tunable
    # and use the parameters from the best model
    finalize_workflow(select_best(xgb_rs, "rmse")) %>%
    # last_fit includes all the training data
    # rather than all the vfolds
    last_fit(df_split)
  
  xgb_best_race
}

# if you want to tune grid (and the optimise)
if(T) {
  message("grid search optimisation takes about 90 seconds....")
  tictoc::tic()
  xgb_rs <- tune_grid( # or race
    xgb_wf,
    resamples = df_folds,
    grid = SEARCH_GRID_SIZE, #  27:30 in a production environment you'd use more
    control = control_race(verbose_elim = TRUE)
  )
  tictoc::toc()
  # with grid = 15: 19.41 sec elapsed for simple tuner, 20.64 sec elapsed for full
  # with grid = 30: 42.93 sec elapsed
  
  xgb_rs
  
  # check what hyperparameters been investigated ----
  autoplot(xgb_rs)
  # or: https://juliasilge.com/blog/xgboost-tune-volleyball/
  my_autoplot <- function(rs, rmse = "rmse") {
    rs %>%
      collect_metrics() %>%
      filter(.metric == rmse) %>%
      # select(mean, mtry:min_n) %>%
      # pivot_longer(mtry:min_n,
      select(mean, mtry:sample_size) %>%
      pivot_longer(mtry:sample_size,
                   values_to = "value",
                   names_to = "parameter"
      ) %>%
      ggplot(aes(value, mean, color = parameter)) +
      geom_point(alpha = 0.8, show.legend = FALSE) +
      facet_wrap(~parameter, scales = "free_x") +
      labs(x = NULL, y = "RMSE")
  }
  
  my_autoplot(xgb_rs)
  
  # we could retune grid on the more promising areas of hyper-parameter space
  # https://www.r-bloggers.com/2020/05/using-xgboost-with-tidymodels/
  
  xgb_grid <-
    grid_latin_hypercube(
      mtry(c(2, 5)), # rand selected predictors
      trees(c(400, 450)), # # Trees
      learn_rate(c(-2.5, -1.0)),
      min_n(c(8, 20)), # Minimum node size
      loss_reduction(), # min loss reduction
      sample_prop(c(0.75, 1.0)), # prop obvs samples
      tree_depth(), # Tree Depth
      size = SEARCH_GRID_SIZE
    )
  
  tictoc::tic()
  xgb_rs <- tune_grid( # or tune_grid
    xgb_wf,
    resamples = df_folds,
    grid = xgb_grid, #  27:30 in a production environment you'd use more
    control = control_race(verbose_elim = TRUE)
  )
  tictoc::toc()
  # with size = 50 , takes 76 seconds
  autoplot(xgb_rs)
  
  
  if(F) {
    # grid specification
    xgboost_params <- 
      dials::parameters(
        min_n(),
        tree_depth(),
        learn_rate(),
        loss_reduction()
      )
    #  Next we set up the grid space. The dails::grid_* functions support several methods for defining the grid space. We are using the dails::grid_max_entropy() function which covers the hyperparameter space such that any portion of the space has an observed combination that is not too far from it.
    xgb_grid <- 
      dials::grid_max_entropy(
        xgboost_params, 
        size = 60
      )
    knitr::kable(head(xgb_grid))
    
    xgb_grid <-
      grid_latin_hypercube(
        trees(),
        tree_depth(),
        min_n(),
        loss_reduction(),
        sample_size = sample_prop(),
        finalize(mtry(), df_train),
        learn_rate(),
        size = 50
      )
    
    tictoc::tic()
    xgb_rs <- tune_grid( # or tune_grid
      xgb_wf,
      resamples = df_folds,
      grid = xgb_grid, #  27:30 in a production environment you'd use more
      control = control_race(verbose_elim = TRUE)
    )
    tictoc::toc()
    # with size = 50 , takes 76 seconds
    
    my_autoplot(xgb_rs) # remmeber no log
  }
  
  show_best(xgb_rs, metric = "rmse")
  
  # get the best model
  xgb_best_grid <- xgb_wf %>%
    # finalise the wf to stop being tunable
    # and use the parameters from the best model
    finalize_workflow(select_best(xgb_rs, "rmse")) %>%
    # last_fit includes all the training data
    # rather than all the vfolds
    last_fit(df_split)
  
  xgb_best_grid
  
  collect_metrics(xgb_best_grid)
  
  collect_predictions(xgb_best_grid) %>%
    ggplot() +
    geom_point(aes(discharging_edm_count, .pred))
}

library(vip)
if(F) {
  xgb_best_grid %>%
    extract_fit_engine() %>%
    vip()
  # in this case, extract_fit_parsnip is the same...
  xgb_best_grid %>%
    extract_fit_parsnip() %>%
    vip() # could add num_features and/or geom = "point")
}

# this could be deployed using vetiver (34:00)
if(F) {
  # example of how to get at the predictions by feeding baked data
  model_reg_xgboost %>%
    predict(df_test)

  xgb_best_grid %>%
    extract_fit_parsnip() %>%
    predict(
      edm_rec %>%
        prep() %>%
        bake(df_test) %>%
        select(-discharging_edm_count)
    )
}

if(F) {
  df_test %>%
    select(discharging_edm_count) %>%
    bind_cols(predict(model_reg_lm, df_test) %>% 
                enframe(name = NULL, value = "lm")
    ) %>%
    bind_cols(predict(model_reg_lm_simplified, df_test) %>% 
                enframe(name = NULL, value = "lm_simplified")
    ) %>%
    # model_reg_lm_very_simplified
    bind_cols(predict(model_reg_lm_very_simplified, df_test) %>% 
                enframe(name = NULL, value = "lm_very_simple")
    ) %>%
    # add ranger model
    bind_cols(pred.edm_rf$predictions %>% enframe(name = NULL, value = "Random_Forest")) %>%
    # add unoptimised xgboost
    bind_cols(predict(model_reg_xgboost, df_test) %>% rename(XGBoost = .pred)) %>%
    # add optimised xgboost
    bind_cols(
      xgb_best_grid %>%
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
}

### Variable Importance
# r variable importance  for each regression model}
vip::vi(model_reg_lm) %>% mutate(Model = "Full lm", .before = 1) %>%
  bind_rows(
    vip::vi(model_reg_lm_simplified) %>% mutate(Model = "Simplified lm", .before = 1)
  ) %>%
  bind_rows(
    vip::vi(model_reg_lm_very_simplified) %>% mutate(Model = "Very simple lm", .before = 1)
  ) %>%
  bind_rows(
    vip::vi(model_reg_rf) %>% mutate(Model = "Random Forest", .before = 1)
  ) %>%
  bind_rows(
    vip::vi(model_reg_xgboost) %>% mutate(Model = "XGBoost", .before = 1)
  ) %>%
  bind_rows(
    vip::vi(xgb_best_grid %>% extract_fit_parsnip()) %>% mutate(Model = "XGBoost (grid optimised)", .before = 1)
  ) %>%
  bind_rows(
    vip::vi(xgb_best_race %>% extract_fit_parsnip()) %>% mutate(Model = "XGBoost (race optimised)", .before = 1)
  ) %>%
  mutate(Sign = coalesce(Sign, "Undefined")) %>%
  mutate(Importance = ifelse(Sign == "NEG", 0-Importance, Importance)) %>%
  ggplot(aes(x = Variable, y = Importance, fill = Sign)) +
  geom_col() +
  coord_flip() +
  facet_wrap( ~ Model, scales = "free_x") +
  labs(title = "This plot shows what each model relied on to estimate discharges",
       subtitle = "The linear models work quite differently to the tree-based ones")

### Plots of actual and modelled estimates of discharging_edm_count
# r compare modelled and actual for each regression model ----
# wrapped in an if() to make it easier to run all together
if(T) {
  # TEST creating ggplot object for visualization
  models_test <- df_test %>%
#    select(discharging_edm_count) %>%
    bind_cols(predict(model_reg_lm, df_test) %>% 
                enframe(name = NULL, value = "lm")
    ) %>%
    bind_cols(predict(model_reg_lm_simplified, df_test) %>% 
                enframe(name = NULL, value = "lm_simplified")
    ) %>%
    bind_cols(predict(model_reg_lm_very_simplified, df_test) %>% 
                enframe(name = NULL, value = "lm_very_simple")
    ) %>%
    bind_cols(stats::predict(model_reg_rf, data = df_test)$predictions %>% enframe(name = NULL, value = "Random_Forest")) %>%
    bind_cols(predict(model_reg_xgboost, df_test) %>% rename(XGBoost = .pred)) %>%
    # add optimised xgboost
    bind_cols(
      xgb_best_grid %>%
        extract_fit_parsnip() %>%
        predict(
          edm_rec %>%
            prep() %>%
            bake(df_test) %>%
            select(-discharging_edm_count)
        ) %>% rename(XGBoost_grid_optimised = .pred)
    ) %>%
    bind_cols(
      xgb_best_race %>%
        extract_fit_parsnip() %>%
        predict(
          edm_rec %>%
            prep() %>%
            bake(df_test) %>%
            select(-discharging_edm_count)
        ) %>% rename(XGBoost_race_optimised = .pred)
    )
    
  # TEST creating ggplot object for visualization
  models_train <- df_train %>%
#    select(discharging_edm_count) %>%
    bind_cols(predict(model_reg_lm, df_train) %>% 
                enframe(name = NULL, value = "lm")
    ) %>%
    bind_cols(predict(model_reg_lm_simplified, df_train) %>% 
                enframe(name = NULL, value = "lm_simplified")
    ) %>%
    bind_cols(predict(model_reg_lm_very_simplified, df_train) %>% 
                enframe(name = NULL, value = "lm_very_simple")
    ) %>%
    bind_cols(stats::predict(model_reg_rf, data = df_train)$predictions %>% enframe(name = NULL, value = "Random_Forest")) %>%
    bind_cols(predict(model_reg_xgboost, df_train) %>% rename(XGBoost = .pred)) %>%
    # add optimised xgboost
    bind_cols(
      xgb_best_grid %>%
        extract_fit_parsnip() %>%
        predict(
          edm_rec %>%
            prep() %>%
            bake(df_train) %>%
            select(-discharging_edm_count)
        ) %>% rename(XGBoost_grid_optimised = .pred)
    ) %>%
    bind_cols(
      xgb_best_race %>%
        extract_fit_parsnip() %>%
        predict(
          edm_rec %>%
            prep() %>%
            bake(df_train) %>%
            select(-discharging_edm_count)
        ) %>% rename(XGBoost_race_optimised = .pred)
    )
  
models_all <- models_test %>%
    mutate(data_source = "test_df") %>%
    bind_rows(
      models_train %>%
        mutate(data_source = "train_df")
    )

models_all %>%
  pivot_longer(-c(date:rain_ea_ma_mm, data_source), names_to = "model", values_to = "estimate") %>%
  ggplot(aes(x = discharging_edm_count, y = estimate, colour = data_source)) +
  geom_point() +
  ggpubr::stat_regline_equation(aes(label =  paste(after_stat(eq.label),
                                                   after_stat(adj.rr.label), sep = "~~~~")), size = 4) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ model) +
  labs(title = "All the models overpredicted the number of recent events (test data)",
       subtitle = "The performance on the test data is more like real-world performance",
       caption = "TW data {api_root}/data/STE/v1/DischargeAlerts, \nEA data from: https://environment.data.gov.uk/flood-monitoring/archive",
       x = "Date", y = "Count of EDMs discharging")

models_all %>%
  pivot_longer(-c(date:rain_ea_ma_mm, data_source), names_to = "model", values_to = "estimate") %>%
  ggplot() +
  geom_line(aes(x = date, y = discharging_edm_count), colour = "grey") +
  geom_line(aes(x = date, y = estimate, colour = data_source)) +
  facet_wrap( ~ model) +
  labs(title = "1 day ahead forecast of number of sites discharging",
       subtitle = "based on recent rainfall, river levels and some other data from the EA",
       caption = "TW data {api_root}/data/STE/v1/DischargeAlerts, \nEA data from: https://environment.data.gov.uk/flood-monitoring/archive",
       x = "Date", y = "Count of EDMs discharging")

}


