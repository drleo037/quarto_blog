#| https://cran.r-project.org/web/packages/tidypredict/vignettes/ranger.html
#| try to get the logic into excel
#| classification then regression
# build a model
library(magrittr)
library(tidyverse)
library(tidypredict)
library(tidymodels)
library(rsample)
library(parsnip)
library(recipes)
library(ranger)
source(here::here("posts/ML_in_excel/ml_excel_functions.R"))
ntrees <- 300 #  we're building 300 trees in the random forests

# iris_n_noise ----
# create (augment) the dataset we'll be using through this piece
str(iris)
levels(iris$Species)
iris_n_noise <- iris %>%
  # as_tibble() %>%
  # mutate(Species = fct_relevel(Species, c("setosa", "versicolor", "virginica"))) %>%
#  mutate(Species = as.character(Species)) %>%
  add_column (Noise = runif (nrow (.))) %>%
  add_column (other_noise = runif (nrow (.))) %>%
  mutate(Noisy.Sepal.Length = Sepal.Length + 10.0*other_noise) %>%
  select(-other_noise)

iris_n_noise <- iris
levels(iris_n_noise$Species)
# exploration of the dataset preliminary EDA ----
if(F) {
  GGally::ggpairs(iris_n_noise, mapping = aes(color = Species))
}
#| I'm going to try to:
#| CLASSIFY Species
#| (it looks like Petals are the dimension that helps most here)
#| REGRESS (model) Petal.length
#| (It looks like Petal.width) is the best at this (basically size!)
#| note: the two random variables I've added has little & no relation to either

# split the dataset into training and test ----
if(T) {
  df_split <- initial_split(iris_n_noise) # we could group by Species
  (df_train <- training(df_split))
  (df_test <- testing(df_split))
  
  levels(iris_n_noise$Species)
  levels(df_train$Species)
  levels(df_test$Species)
  
  if(F) {
    GGally::ggpairs(df_train, mapping = aes(color = Species))
    GGally::ggpairs(df_test, mapping = aes(color = Species))
  }
}

# classification using random forests (lots of decision trees) ----
if(T) {
  if(T) {
    # I'm forcing Species to characher as there seems to be a bug on the levels for extract_fit
    # this means that ranges will fall over (it wants factors)
    message("Building classification example just using ranger...")
    model_clas <- ranger(Species ~ ., # we're modelling Species as a function of everything
                         data = df_train, # modelling data held in iris_n_noise
                         num.trees = ntrees,
                         importance = "impurity" # I've added the optional impurity so I check variable importance later
    )
    message("how good is the model?")
    pred.iris <- predict(model_clas, data = df_test)
    table(df_test$Species, pred.iris$predictions)
    
    message("inspect the variable importance")
    #  the importance is here: model_clas$variable.importance
    model_clas %>% 
      vip::vip(num_features = 20,  aesthetics = list(color = "grey50", fill = "lightblue"))
  } else {
    message("Building classification example just using tidymodels")
    rf_model <- rand_forest(mode = "classification", trees = ntrees) %>% 
      set_engine("ranger", importance = "impurity", probability = FALSE) %>%
      set_mode("classification")
    
    rf_recipe <- recipe(Species ~ ., data = df_train,
                        num.trees = ntrees,
                        importance = "impurity"
             ) 
    
    rf_workflow <- workflow() %>% 
      add_model(rf_model) %>% 
      add_recipe(rf_recipe)
    
    model_clas <- rf_workflow %>% 
      fit(df_train) %>% 
      extract_fit_engine() # extract_fit_engine returns the engine class,. extract_fit_parsnip returns a parsnip object
    
    message("how good is the model?")
    pred.iris <- predict(model_clas, data = df_test)
    table(df_test$Species, pred.iris$predictions)
    message("inspect the variable importance")
    model_clas %>% 
      vip::vip(num_features = 20,  aesthetics = list(color = "grey50", fill = "lightblue"))
  }

}

# teaser:  extract all the rules used in the trees
if(T) {
  #| have a look at what we've got:
  treeInfo(model_clas) %>%
    head()
  
  if(F) {
    #| look at a single tree as dplyr syntax
    tidypredict::tidypredict_fit(model_clas)[1] # DPLYR
    #| look at a single tree as SQL syntax
    tidypredict::tidypredict_sql(model_clas, dbplyr::simulate_dbi())[1] # SQL
  }
  
  #| see: https://github.com/tidymodels/tidypredict
  #| The output from parse_model() is transformed into a
  #| dplyr, a.k.a Tidy Eval, formula.
  #| The entire decision tree becomes one
  #| dplyr::case_when() statement
  #| look at all the trees
  if(F) {
    #| look at a single tree as dplyr syntax
    tidypredict::tidypredict_fit(model_clas)[1]
    #| so to use...
    #| The readme (.rmd) says you can do this but it doesnt work for me
    if(F) {
      iris_n_noise %>%
        mutate(this  = !! tidypredict_fit(model_clas)[1]) %>% View()
    }
    #| which actually does this:
    #iris_n_noise %>%
    if(F) {
      df_test %>%
        mutate(prediction =
                 case_when(Petal.Width < 0.8 ~ "versicolor", Petal.Width >= 1.75 & 
                             Petal.Width >= 0.8 ~ "virginica", Noisy.Sepal.Length < 15.551182267908 & 
                             Petal.Width < 1.75 & Petal.Width >= 0.8 ~ "setosa", Noisy.Sepal.Length >= 
                             15.551182267908 & Petal.Width < 1.75 & Petal.Width >= 0.8 ~ 
                             "virginica")
        ) %>%
        mutate(prediction =
                 case_when(Petal.Width < 0.8 & Sepal.Length < 5.35 & Sepal.Length < 
                             5.45 ~ "setosa", Petal.Width >= 0.8 & Sepal.Length < 5.35 & 
                             Sepal.Length < 5.45 ~ "versicolor", Noisy.Sepal.Length < 
                             14.9154362823814 & Sepal.Length >= 5.35 & Sepal.Length < 
                             5.45 ~ "versicolor", Noisy.Sepal.Length >= 14.9154362823814 & 
                             Sepal.Length >= 5.35 & Sepal.Length < 5.45 ~ "setosa", Petal.Width < 
                             1.55 & Sepal.Length >= 6.45 & Sepal.Length >= 5.45 ~ "versicolor", 
                           Petal.Length < 2.7 & Petal.Width < 1.7 & Sepal.Length < 6.45 & 
                             Sepal.Length >= 5.45 ~ "setosa", Petal.Length >= 2.7 & 
                             Petal.Width < 1.7 & Sepal.Length < 6.45 & Sepal.Length >= 
                             5.45 ~ "versicolor", Noise >= 0.15800827939529 & Petal.Width >= 
                             1.7 & Sepal.Length < 6.45 & Sepal.Length >= 5.45 ~ "virginica", 
                           Noisy.Sepal.Length < 8.6015364442952 & Petal.Width >= 1.55 & 
                             Sepal.Length >= 6.45 & Sepal.Length >= 5.45 ~ "versicolor", 
                           Noisy.Sepal.Length >= 8.6015364442952 & Petal.Width >= 1.55 & 
                             Sepal.Length >= 6.45 & Sepal.Length >= 5.45 ~ "virginica", 
                           Petal.Length < 5.05 & Noise < 0.15800827939529 & Petal.Width >= 
                             1.7 & Sepal.Length < 6.45 & Sepal.Length >= 5.45 ~ "versicolor", 
                           Petal.Length >= 5.05 & Noise < 0.15800827939529 & Petal.Width >= 
                             1.7 & Sepal.Length < 6.45 & Sepal.Length >= 5.45 ~ "virginica")
        ) %>%
        count(Species, prediction) %>%
        pivot_wider(names_from = prediction, values_from = n)
    }
  }
  
  message("Finished building classification example.")
}

# load the electricity demand data ----
if(F) {
  message("Loading electricity demand data...")
  # load some data
  source("C:/Users/leoki/CODE/R-Home/blog_pending/posts/weather_plots/0. prepare_weather_df.R", local = TRUE)
  reg_df <- joint_df %>%
    select(-c(date, season, day_of_week, Monthly, Daily, Yearly)) %>%
    mutate(month = as.character(month)) %>%
    relocate(total_load_actual, .after = last_col())
}

# lm regression ----
if(T) {
  message("Building LM example...")
  
  model_lm <- lm(Petal.Length ~ ., data=iris_n_noise)
  simplified_model <- MASS::stepAIC(model_lm, direction = "both")
  model_lm %>%
    broom::tidy() %>% arrange(p.value)
  simplified_model %>%
    broom::tidy() %>% arrange(p.value)
  
  broom::tidy(model_lm)
  broom::tidy(simplified_model)
  
  anova(model_lm, simplified_model)
  performance::compare_performance(model_lm, simplified_model, rank = TRUE)
  
  
  # this emulates SQL (will come in handy when getting it into excel, see later)
  tidypredict_sql(model_lm, dbplyr::simulate_dbi())
  # tidypredict_sql is fine if you're pushing the work into a database
  # but tidypredict_fit can be used directly in R
  tidypredict_fit(model_lm)
  tidypredict_fit(simplified_model)
  
  # the outputs of tidypredict_fit:
  # can be used directly in R
  library(ggpubr)
#  library(GGally)
  if(F) {
    iris_n_noise %>%
      as_tibble() %>%
      mutate(full_model  = 
               -1.10857841592906 + (Sepal.Length * 0.603455429434109) + (Sepal.Width * 
                                                                           -0.183512927108715) + (Petal.Width * 0.603588872323013) + 
               (ifelse(Species == "versicolor", 1, 0) * 1.46215261559484) + 
               (ifelse(Species == "virginica", 1, 0) * 1.97190265359308) + 
               (Noise * -0.0111438132797317) + (Noisy.Sepal.Length * 0.0036781863997925)
      ) %>%
      mutate(step_model =
               -1.11098875342281 + (Sepal.Length * 0.608005816988775) + (Sepal.Width * 
                                                                           -0.180523585414275) + (Petal.Width * 0.602221481207869) + 
               (ifelse(Species == "versicolor", 1, 0) * 1.46337087129335) + 
               (ifelse(Species == "virginica", 1, 0) * 1.97422285319567)
      ) %>%
      select(Petal.Length, Species, full_model, step_model) %>%
      pivot_longer(-c(Petal.Length, Species), names_to = "model", values_to = "estimate") %>%
      ggplot(aes(Petal.Length, estimate)) +
      geom_point(aes(colour = Species)) +
      stat_regline_equation(aes(label =  paste(after_stat(eq.label), after_stat(adj.rr.label), sep = "~~~~"))) + 
      geom_smooth(method = "lm") +
      facet_wrap( ~ model)
  }
  
  message("Finished building LM example.")
}

# xgboost regression (works,  but the SQL needs extra work to convert into excel) ----
if(F) {
  library(parsnip)
  
  model_reg_xgboost <- boost_tree(mode = "regression") %>%
    set_engine("xgboost") %>%
    fit(Petal.Length ~ ., data = iris_n_noise)
  tidypredict_test(model_lm_xgboost, iris_n_noise, xg_df = xgb_bin_data)
  tidypredict_fit(model_lm_xgboost)
  
  tictoc::tic()
  trees_df_xgboost_reg <- tidypredict::tidypredict_sql(model_reg_xgboost, dbplyr::simulate_dbi()) %>%
    tibble::enframe(name = NULL, value = "instruction") %>%
    mutate(instruction = unlist(instruction))
  tictoc::toc()
}

# random forest regression for iris ----
if(T) {
  message("Building regression example for iris ...")
  
  #| https://www.tidymodels.org/start/case-study/
  (cores <- parallel::detectCores())

  #| https://www.jottr.org/2022/12/05/avoid-detectcores/
  (cores <- parallelly::availableCores())
  
  message(glue::glue("Running in parallel on {cores} availableCores rather than {parallel::detectCores()} detectCores"))
  
  set.seed(234)
  message("creating training and validation splits at 80%")
  val_set <- validation_split(iris_n_noise, 
                              strata = Petal.Length, 
                              prop = 0.80)
  val_set
  
  message("building model form (ranger regession)")
  rf_mod <- 
    rand_forest(mtry = tune(), min_n = tune(), trees = 20) %>% 
    set_engine("ranger", num.threads = cores) %>% 
    set_mode("regression")
  if(F) {
    # summary of the model
    rf_mod
    extract_parameter_set_dials(rf_mod)
  }
  
  
  message("building data transformation pipeline (minimal)") # todo: exclude datetime
  rf_recipe <-recipe(Petal.Length ~ ., data = iris_n_noise)

  message("construct the full workflow with the model & recipe")
  rf_workflow <- 
    workflow() %>% 
    add_model(rf_mod) %>% 
    add_recipe(rf_recipe)
  
  
  set.seed(345)
  message("tuning the hyper-parameters using tune_grid...(shoud this be val_set?)")
  rf_res <- 
    rf_workflow %>% 
    tune_grid(val_set,
              grid = 25,
              control = control_grid(save_pred = TRUE),
              metrics = metric_set(mape))
  #> i Creating pre-processing data to finalize unknown parameter: mtry
  #> 
  message("Top 10 best RF models: ")
  rf_res %>% 
    show_best(metric = "mape", n = 10)
  
  message("Hyper parameter parameter sensitivity:")
  rf_res %>%
    collect_metrics() %>%
    ggplot(aes(min_n, mean, colour = as_factor(mtry))) +
    geom_line(linewidth = 1.5, alpha = 0.6) +
    geom_point(size = 2) +
    facet_wrap(~ .metric, scales = "free", nrow = 2) +
    scale_x_log10(labels = scales::label_number()) +
    scale_color_viridis_d(option = "plasma", begin = .9, end = 0)
  
  message("Extracting the best model")
  rf_best <- 
    rf_res %>% 
    select_best(metric = "mape")
  
  rf_best
  
  message(glue::glue("Best model has mtry={rf_best$mtry} and min_n={rf_best$min_n}"))
  #| min_n	is An integer for the minimum number of data points in a node
  #| that are required for the node to be split further.
  #| very small min_n ends up splitting the trees many times
  #| leading to huge trees (too big for excel at least)
  # if(rf_best$min_n < 10) {
  #   stop("models might be too big")
  # }
  
  message("Example of getting the predictions from the best model")
  rf_res %>% 
    collect_predictions(parameters = rf_best) %>%
    ggplot(aes(Petal.Length, .pred)) +
    geom_point() +
    stat_regline_equation(aes(label =  paste(after_stat(eq.label), after_stat(adj.rr.label), sep = "~~~~"))) + 
    geom_smooth(method = "lm") 
    
  
  
  # the last regression model ----
  message("starting again but this time using the best hyper-parameters...")
  last_rf_mod <- 
    rand_forest(mtry = rf_best$mtry, min_n = rf_best$min_n, trees = ntrees) %>% 
    set_engine("ranger", num.threads = cores, importance = "impurity") %>% 
    set_mode("regression")
  
  # the last workflow
  message("updating the workflow to include the chosen model form")
  last_rf_workflow <- 
    rf_workflow %>% 
    update_model(last_rf_mod)
  
  # creating simple training / test splits
  message("creating simple training / test splits")
  set.seed(123)
  splits <- initial_split(iris_n_noise, strata = Petal.Length)

  # the last fit
  set.seed(345)
  last_rf_fit <- 
    last_rf_workflow %>% 
    last_fit(splits)
  
  last_rf_fit
  
  message("Example of getting the predictions from the best model")
  last_rf_fit %>% 
    collect_predictions(parameters = rf_best) %>%
    ggplot(aes(Petal.Length, .pred)) +
    geom_point() +
    stat_regline_equation(aes(label =  paste(after_stat(eq.label), after_stat(adj.rr.label), sep = "~~~~"))) + 
    geom_smooth(method = "lm") 
  
  
  last_rf_fit %>% 
    collect_metrics()
  
  message("Collect the final model")
  model_reg <- last_rf_fit %>% 
    extract_fit_parsnip()
  
  message("Example of getting the predictions from the best model")
  iris_n_noise %>%
    add_column (.pred = model_reg %>% 
                  predict(new_data = iris_n_noise)) %>%
    ggplot(aes(Petal.Length, .pred$.pred)) +
    geom_point() +
    stat_regline_equation(aes(label =  paste(after_stat(eq.label), after_stat(adj.rr.label), sep = "~~~~"))) + 
    geom_smooth(method = "lm") 
  
  message("inspect the variable importance")
  model_reg %>% 
    vip::vip(num_features = 20)

  message("Finished building regression example.")
}

# regression for energy ----
if(F) {
  message("Building regression example for ...")
  
  #| https://www.tidymodels.org/start/case-study/
  (cores <- parallel::detectCores())
  
  #| https://www.jottr.org/2022/12/05/avoid-detectcores/
  (cores <- parallelly::availableCores())
  
  message(glue::glue("Running in parallel on {cores} cores based on availableCores rather than detectCores"))
  
  if(F) {
    # now for the regression model... 
    treeInfo(model_reg_test) %>% as_tibble()
    tictoc::tic()
    trees_df_reg <- tidypredict::tidypredict_sql(model_reg_test, dbplyr::simulate_dbi()) %>%
      tibble::enframe(name = NULL, value = "instruction") %>%
      mutate(instruction = unlist(instruction))
    tictoc::toc()
  }
  
  # build a model
  library(tidymodels)
  library(rsample)
  library(parsnip)
  library(recipes)
  
  set.seed(234)
  
  message("creating training and validation splits at 80%")
  val_set <- validation_split(reg_df, 
                              strata = total_load_actual, 
                              prop = 0.80)
  val_set
  
  message("building model form (ranger regession)")
  rf_mod <- 
    rand_forest(mtry = tune(), min_n = tune(), trees = 20) %>% 
    set_engine("ranger", num.threads = cores) %>% 
    set_mode("regression")
  if(F) {
    # summary of the model
    rf_mod
    extract_parameter_set_dials(rf_mod)
  }
  
  
  message("building data transformation pipeline (minimal)") # todo: exclude datetime
  if(T) {
    rf_recipe <- 
      recipe(total_load_actual ~ ., data = reg_df)
  } else {
    rf_recipe <- 
      recipe(total_load_actual ~ ., data = reg_df) %>% 
      step_date(date) %>% 
      step_holiday(date) %>% 
      step_rm(date)
  }
  
  message("construct the full workflow with the model & recipe")
  rf_workflow <- 
    workflow() %>% 
    add_model(rf_mod) %>% 
    add_recipe(rf_recipe)
  
  
  set.seed(345)
  message("tuning the hyper-parameters using tune_grid...(shoud this be val_set?)")
  rf_res <- 
    rf_workflow %>% 
    tune_grid(val_set,
              grid = 25,
              control = control_grid(save_pred = TRUE),
              metrics = metric_set(mape))
  #> i Creating pre-processing data to finalize unknown parameter: mtry
  #> 
  message("Best RF models: ")
  rf_res %>% 
    show_best(metric = "mape", n = 10)
  
  message("Hyper parameter parameter sensitivity:")
  
  message("Extracting the best model")
  rf_best <- 
    rf_res %>% 
    select_best(metric = "mape")
  
  rf_best
  
  message(glue::glue("Best model has mtry={rf_best$mtry} and min_n={rf_best$min_n}"))
  #| min_n	is An integer for the minimum number of data points in a node
  #| that are required for the node to be split further.
  #| very small min_n ends up splitting the trees many times
  #| leading to huge trees (too big for excel at least)
  if(rf_best$min_n < 30) {
    stop("models will be too big")
  }
  
  message("Example of getting the predictions from the best model")
  rf_res %>% 
    collect_predictions(parameters = rf_best)
  
  
  # the last regression model ----
  message("starting again but this time using the best hyper-parameters...")
  last_rf_mod <- 
    rand_forest(mtry = rf_best$mtry, min_n = rf_best$min_n, trees = ntrees) %>% 
    set_engine("ranger", num.threads = cores, importance = "impurity") %>% 
    set_mode("regression")
  
  # the last workflow
  message("updating the workflow to include the chosen model form")
  last_rf_workflow <- 
    rf_workflow %>% 
    update_model(last_rf_mod)
  
  # creating simple training / test splits
  message("creating simple training / test splits")
  set.seed(123)
  splits <- initial_split(reg_df, strata = total_load_actual)
  
  # the last fit
  set.seed(345)
  last_rf_fit <- 
    last_rf_workflow %>% 
    last_fit(splits)
  
  last_rf_fit
  
  last_rf_fit %>% 
    collect_metrics()
  
  message("Collect the final model")
  model_reg <- last_rf_fit %>% 
    extract_fit_parsnip()
  
#  tidypredict::tidypredict_sql(model_reg, dbplyr::simulate_dbi())[1]
  
  message("inspect the variable importance")
  model_reg %>% 
    vip::vip(num_features = 20)
  
  # the rules are stored here:
  # now for the regression model...
  #  treeInfo(model_reg) %>% as_tibble()
  
  # extract the rules
  if(F) {
    # tidypredict::tidypredict_fit(model_regression)
    tictoc::tic()
    message("extrating the regression rules (takes a while 405.21 sec elapsed, 590 on battery)")
    trees_df_reg <- tidypredict::tidypredict_sql(model_reg, dbplyr::simulate_dbi()) %>%
      tibble::enframe(name = NULL, value = "instruction") %>%
      mutate(instruction = unlist(instruction)) 
    tictoc::toc()
    saveRDS(trees_df_reg, file = here::here("posts/ML_in_excel/trees_df_reg.Rds"))
  } else {
    trees_df_reg = readRDS(here::here("posts/ML_in_excel/trees_df_reg.Rds"))
  }
  
  if(F) {
    trees_df_reg %>%
      mutate(rule_n = row_number()) %>%
      # unpack so we can get the variables
      unnest_tokens(word, output, token = "regex", pattern = "THEN") %>%
      select(rule_n, word) %>%
      # sort by the number
      
      # repack into whole functions
      group_by(rule_n) %>%
      summarise(output = paste0(new_word, collapse = '')) 
  }
  
  randforest_reg <- sql_to_excel(trees_df = trees_df_reg, input_df = reg_df, n_sf = 3)
  
  model_output_reg <- augment_df_with_rules(randforest_reg,
                                            #                                            reg_df,
                                            reg_df %>% sample_n(200),
                                            method = "regression")
  
  model_output_reg[1,"tree_best"]
  model_output_reg[1,"tree_1"]
  
  str_length(model_output_reg[1,"tree_1"])
  
  str_count(model_output_reg[1,"tree_1"], "=i")
  
  message("Finished building regression example.")
}

# prepare outputs for excel ----
if(T) {
  #| turn the tidypredict_sql for all the trees into a tidy dataframe

  #| random forest classification in excel format ----
  tictoc::tic()
  trees_df_clas <- tidypredict::tidypredict_sql(model_clas, dbplyr::simulate_dbi()) %>%
    tibble::enframe(name = NULL, value = "instruction") %>%
    mutate(instruction = unlist(instruction))
  tictoc::toc()
  
  randforest_clas <- sql_to_excel(trees_df = trees_df_clas, input_df = iris_n_noise)
  model_output_clas <- augment_df_with_rules(models = randforest_clas,
                                             in_df = iris_n_noise,
                                             target = "Species",
                                             method = "classification")
  trees_df_clas[1,]
  model_output_clas[1,"tree_1"]

  # this emulates LM in excel format ----
  trees_df_lm <- tidypredict_fit(model_lm)[2] %>% as.character() %>%
    tibble::enframe(name = NULL, value = "instruction") %>%
    mutate(instruction = unlist(instruction)) %>%
    mutate(instruction = as.character(instruction))
  # make excel-friendly
  lm_excel <- fit_to_excel(trees_df = trees_df_lm, input_df = iris_n_noise, n_sf = 3)
  model_output_lm <- augment_df_with_rules(models = lm_excel,
                                           in_df = iris_n_noise,
                                           target = "Petal.Length",
                                           method = "regession")
  
  
  if(F) {
    xgboost_excel <- sql_to_excel(trees_df = trees_df_xgboost_reg, input_df = iris_n_noise)
    model_output_xgboost <- augment_df_with_rules(models = xgboost_excel, in_df = iris_n_noise, method = "regession")
    # doesnt work yet as the "or(A3isnull)" in trees_df_xgboost_reg SQL bits arent parsed correctly YET
  }
  
  # random forest regression in excel format ----
  # get the rules
  trees_df_iris_reg <- tidypredict::tidypredict_sql(model_reg, dbplyr::simulate_dbi()) %>%
    tibble::enframe(name = NULL, value = "instruction") %>%
    mutate(instruction = unlist(instruction)) 
  # convert the rules to excel format 
  randforest_reg <- sql_to_excel(trees_df = trees_df_iris_reg, input_df = iris_n_noise)
  dim(randforest_reg) #one row but many columns (one per tree)
  model_output_iris_reg <- augment_df_with_rules(models = randforest_reg,
                                                 in_df = iris_n_noise,
                                                 target = "Petal.Length",
                                                 method = "regression")
  
  dim(model_output_iris_reg) # many rows (one per example) and many columns (one per tree)
  model_output_iris_reg[1,]$tree_best
  model_output_iris_reg[1,]$tree_confidence
  model_output_iris_reg[1,]$tree_match
  # example rule
  model_output_iris_reg[1,]$tree_1
  # the trees can be quite long as every condition is defined as "IF",
  # most of the conditions for the next case are really just "ELSE"
  # but this way makes things much easier to read as a human
  # excel's nested if(this, that, if(other...)) quickly become impenetrable
  str_length(model_output_iris_reg[1,"tree_1"])
}

#| write a spreadsheet holding the model -----
if(T) {
  message("Creating excel workbook...")  
  output_wb <- openxlsx::createWorkbook(creator = "Leo Kiernan", subject = paste0("Ranger random forest model ", lubridate::now()))
  openxlsx::addWorksheet(output_wb, "README", tabColour = "blue")
  openxlsx::addWorksheet(output_wb, "lm", tabColour = "green")
  openxlsx::addWorksheet(output_wb, "lmRules", tabColour = "green")
  openxlsx::addWorksheet(output_wb, "regression", tabColour = "red")
  openxlsx::addWorksheet(output_wb, "regressionRules", tabColour = "red")
  openxlsx::addWorksheet(output_wb, "classification", tabColour = "orange")
  openxlsx::addWorksheet(output_wb, "classificationRules", tabColour = "orange")
  
  message("Populating excel workbook...")  
  
  tribble(
    ~from, ~tab, ~info,
    "LAK", "README", "This sheet",
    "LAK", "lm", "example of a regression using a linear model",
    "LAK", "lmRules", "example of the internal equations in the linear model",
    "LAK", "classification", "example of a classification random forest",
    "LAK", "classificationRules", "example of one rule from the classification random forest",
    "LAK", "regression", "example of a regression random forest",
    "LAK", "regressionRules", "example of one rule from the regression random forest"
  ) %>%
    mutate(Date = lubridate::now(), .before = 1) %>%
    openxlsx::writeDataTable( wb = output_wb,
                              sheet = "README",
                              x = .,
                              startCol = 1,
                              startRow = 1,
                              tableStyle = "TableStyleLight9",
                              tableName = "README")
  
  # lm ----
  model_output_lm %>%
    openxlsx::writeDataTable( wb = output_wb,
                              sheet = "lm",
                              x = .,
                              startCol = 1,
                              startRow = 1,
                              tableStyle = "TableStyleLight9",
                              tableName = "lm")
  
  trees_df_lm  %>%
    openxlsx::writeDataTable( wb = output_wb,
                              sheet = "lmRules",
                              x = .,
                              startCol = 1,
                              startRow = 1,
                              tableStyle = "TableStyleLight9",
                              tableName = "lmRules")

  # classification tree ----
  model_output_clas %>%
    openxlsx::writeDataTable( wb = output_wb,
                              sheet = "classification",
                              x = .,
                              startCol = 1,
                              startRow = 1,
                              tableStyle = "TableStyleLight9",
                              tableName = "classification")
  
  trees_df_clas  %>%
    openxlsx::writeDataTable( wb = output_wb,
                              sheet = "classificationRules",
                              x = .,
                              startCol = 1,
                              startRow = 1,
                              tableStyle = "TableStyleLight9",
                              tableName = "classificationRules")

  # regression tree ----
  model_output_iris_reg %>%
    openxlsx::writeDataTable( wb = output_wb,
                              sheet = "regression",
                              x = .,
                              startCol = 1,
                              startRow = 1,
                              tableStyle = "TableStyleLight9",
                              tableName = "regression")
  trees_df_iris_reg  %>%
    openxlsx::writeDataTable( wb = output_wb,
                              sheet = "regressionRules",
                              x = .,
                              startCol = 1,
                              startRow = 1,
                              tableStyle = "TableStyleLight9",
                              tableName = "regressionRules")
  
  
  message("Saving excel workbook...")  
  openxlsx::saveWorkbook(wb = output_wb,
                         file = here::here(stringr::str_c("posts/ML_in_excel/ml_in_excel.xlsx")),
                         overwrite = T)
  
  message("Done")
  beepr::beep()
}

