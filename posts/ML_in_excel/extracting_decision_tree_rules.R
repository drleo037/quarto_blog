#| https://cran.r-project.org/web/packages/tidypredict/vignettes/ranger.html
#| try to get the logic into excel
#| classification then regression

library(magrittr)
library(tidyverse)
library(tidypredict)
library(ranger)
library(tidymodels)
source(here::here("posts/ML_in_excel/ml_excel_functions.R"))
ntrees = 300


# classification ----
if(T) {
  message("Building classification example...")
  # iris_tbl <- janitor::clean_names(iris) %>% as_tibble()
#  model_clas <- ranger(Species ~ .,data = iris, num.trees =  100)
  model_clas <- ranger(Species ~ .,data = iris)
  
  #| have a look at what we've got:
  
  treeInfo(model_clas) %>%
    head()
  
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
    mutate(iris, !! tidypredict_fit(model_clas)[1])
    #| which actually does this:
    iris %>%
      mutate(prediction = 
               case_when(Petal.Length < 2.45 ~ "setosa", Petal.Length < 4.85 & 
                           Petal.Width >= 1.75 & Petal.Length >= 2.45 ~ "virginica", 
                         Petal.Length >= 4.85 & Petal.Width >= 1.75 & Petal.Length >= 
                           2.45 ~ "virginica", Petal.Width < 1.65 & Petal.Length < 
                           4.95 & Petal.Width < 1.75 & Petal.Length >= 2.45 ~ "versicolor", 
                         Petal.Width >= 1.65 & Petal.Length < 4.95 & Petal.Width < 
                           1.75 & Petal.Length >= 2.45 ~ "virginica", Petal.Length >= 
                           5.05 & Petal.Length >= 4.95 & Petal.Width < 1.75 & Petal.Length >= 
                           2.45 ~ "virginica", Sepal.Length < 6.35 & Petal.Length < 
                           5.05 & Petal.Length >= 4.95 & Petal.Width < 1.75 & Petal.Length >= 
                           2.45 ~ "virginica", Sepal.Length >= 6.35 & Petal.Length < 
                           5.05 & Petal.Length >= 4.95 & Petal.Width < 1.75 & Petal.Length >= 
                           2.45 ~ "versicolor")
      ) %T>%
      clipr::write_clip() %>%
      count(Species, prediction)
  }

  # SQL is easier to convert into excel logic  
  if(F) {
    #| look at a single tree as sql syntax
    tidypredict::tidypredict_sql(model_clas, dbplyr::simulate_dbi())[1]
    #| looks like this
    # <SQL> CASE
    # WHEN (`Petal.Width` < 0.8) THEN 'setosa'
    # WHEN (`Petal.Length` >= 5.35 AND `Petal.Width` < 1.65 AND `Petal.Width` >= 0.8) THEN 'virginica'
    # WHEN (`Petal.Length` >= 4.85 AND `Petal.Width` >= 1.65 AND `Petal.Width` >= 0.8) THEN 'virginica'
    # WHEN (`Petal.Length` < 4.95 AND `Petal.Length` < 5.35 AND `Petal.Width` < 1.65 AND `Petal.Width` >= 0.8) THEN 'versicolor'
    # WHEN (`Petal.Width` < 1.75 AND `Petal.Length` < 4.85 AND `Petal.Width` >= 1.65 AND `Petal.Width` >= 0.8) THEN 'virginica'
    # WHEN (`Petal.Width` >= 1.75 AND `Petal.Length` < 4.85 AND `Petal.Width` >= 1.65 AND `Petal.Width` >= 0.8) THEN 'virginica'
    # WHEN (`Petal.Width` < 1.55 AND `Petal.Length` >= 4.95 AND `Petal.Length` < 5.35 AND `Petal.Width` < 1.65 AND `Petal.Width` >= 0.8) THEN 'virginica'
    # WHEN (`Petal.Width` >= 1.55 AND `Petal.Length` >= 4.95 AND `Petal.Length` < 5.35 AND `Petal.Width` < 1.65 AND `Petal.Width` >= 0.8) THEN 'versicolor'
    # END
    
  }
  
  #| turn the tidypredict_sql for all the trees into a tidy dataframe
  #| takes 15.2s for this classification model with 500 trees
  tictoc::tic()
  trees_df_clas <- tidypredict::tidypredict_sql(model_clas, dbplyr::simulate_dbi()) %>%
    tibble::enframe(name = NULL, value = "instruction") %>%
    mutate(instruction = unlist(instruction))
  tictoc::toc()
  
  right_way_clas <- sql_to_excel(trees_df_clas, iris)
  
  model_output_clas <- augment_df_with_rules(right_way_clas, iris)
  
  model_output_clas[2,"tree_1"]
  # try to embed the trees as sub-graphs to make traversal from root to end faster
  if(F) {
    
    treeInfo(model_reg, 3) %>% as_tibble()
    
    hotspot_split_graph <- hotspots_to_splits %>%
      #  filter(HOTSPOT_ID %in%c(187, 233, 2483)) %>%
      igraph::graph.data.frame(directed = FALSE)
    
    # make an igraph to make te associations easier
    cHW <- igraph::clusters(hotspot_split_graph)
    
    # extract the sub-graphs from te full graph (i.e the groupings)
    hotspot_split_grouping <- cHW$membership %>% enframe(name = "node_id", value = "HS_GROUP_ID")
    
  }
  message("Finished building classification example.")
}


if(F) {
  iris  %>%
    clipr::write_clip()
  #| parsnip
  #| tidypredict also supports ranger model objects fitted via the parsnip package.
  
  library(parsnip)
  
  parsnip_model <- rand_forest(mode = "classification") %>%
    set_engine("ranger") %>%
    fit(Species ~ ., data = iris)
  
  tidypredict_fit(parsnip_model)[[1]]
}

# load the electricity demand data ----
if(T) {
  message("Loading electricity demand data...")
  # load some data
  source("C:/Users/leoki/CODE/R-Home/blog_pending/posts/weather_plots/0. prepare_weather_df.R", local = TRUE)
  reg_df <- joint_df %>%
    select(-c(date, season, day_of_week, Monthly, Daily, Yearly)) %>%
    mutate(month = as.character(month)) %>%
    relocate(total_load_actual, .after = last_col())
}


# lm ----
if(T) {
  message("Building LM example...")
  
  model_lm <- lm(mpg ~ wt + am + cyl, data = mtcars)
  model_lm <- lm(total_load_actual ~ ., data = reg_df)
  
  # this emulates SQL (will come in handy when getting it into excel)
  trees_df_lm <- tidypredict_sql(model_lm, dbplyr::simulate_dbi()) %>%
    tibble::enframe(name = NULL, value = "instruction") %>%
    mutate(instruction = unlist(instruction)) %>%
    mutate(instruction = as.character(instruction))
  
  # this emulates SQL (will come in handy when getting it into excel)
  trees_df_lm <- tidypredict_fit(model_lm)[2] %>% as.character() %>%
    tibble::enframe(name = NULL, value = "instruction") %>%
    mutate(instruction = unlist(instruction)) %>%
    mutate(instruction = as.character(instruction))
  
  # the output of this:
  tidypredict_fit(model_lm)
  
  # can be used directly in R
  reg_df %>%
    mutate(total_load_forecast = 29708.1154784826 + (temp * 2.6518224666592) + (pressure * 0.020961937013429) + 
            (rain_1h * 6.19339099163478) + (clouds_all * 5.04411820348654) + 
            (wind_speed * 48.2544242235996) + (wind_deg * -6.49043042895947) + 
            (ifelse(weekend == "TRUE", 1, 0) * -2576.29683911968) + (ifelse(month == 
            "Aug", 1, 0) * 1596.01231011679) + (ifelse(month == "Dec", 
            1, 0) * 2358.12620432008) + (ifelse(month == "Feb", 1, 0) * 
            1620.3108483952) + (ifelse(month == "Jan", 1, 0) * 1847.88181585017) + 
            (ifelse(month == "Jul", 1, 0) * 2777.94235287699) + (ifelse(month == 
            "Jun", 1, 0) * 1734.89625323495) + (ifelse(month == "Mar", 
            1, 0) * 911.088067323669) + (ifelse(month == "May", 1, 0) * 
            148.219715560581) + (ifelse(month == "Nov", 1, 0) * 2666.01573158301) + 
            (ifelse(month == "Oct", 1, 0) * 989.613054537105) + (ifelse(month == 
            "Sep", 1, 0) * 1559.28647837385) + (Weekly * -38.8763943063095)
           ) %>%
    ggplot(aes(total_load_actual, total_load_forecast)) +
    geom_point(color = 'red')
  
  broom::tidy(model_lm)
  broom::augment(model_lm) %>%
    rename(total_load_forecast = .fitted) %>%
    ggplot(aes(total_load_actual, total_load_forecast)) +
    geom_point(color = 'green')
  
lm_excel <- sql_to_excel(trees_df = trees_df_lm, input_df = reg_df, n_sf = 1, squishit = T)

model_output_lm <- augment_df_with_rules(lm_excel, reg_df %>% sample_n(140), method = "regession")


  message("Finished building LM example.")
}

# regression ----
if(T) {
  message("Building regression example...")
  
  #| https://www.tidymodels.org/start/case-study/
  (cores <- parallel::detectCores())

  #| https://www.jottr.org/2022/12/05/avoid-detectcores/
  (cores <- parallelly::availableCores())
  
  message(glue::glue("Running in parallel on {cores} cores based on availableCores rather than detectCores"))
  
  if(F) {
    treeInfo(model_clas) %>% as_tibble()
    tictoc::tic()
    # this can take ages because regression models can be very deep
    tidypredict::tidypredict_sql(model_clas, dbplyr::simulate_dbi())[1]
    tictoc::toc()

    model_reg_test <- ranger(total_load_actual ~ ., data = reg_df,
                        num.trees =  ntrees, num.threads = cores, importance = "impurity")
    model_reg_test %>%
      vip::vip(num_features = 20)
    
    reg_df
    
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

  right_way_reg <- sql_to_excel(trees_df = trees_df_reg, input_df = reg_df, n_sf = 1, squishit = T)
  
  model_output_reg <- augment_df_with_rules(right_way_reg, reg_df %>% sample_n(140), method = "regression")

  model_output_reg[1,"tree_best"]
  model_output_reg[1,"tree_1"]
  
  str_length(model_output_reg[1,"tree_1"])
  
  str_count(model_output_reg[1,"tree_1"], "=i")
  
  message("Finished building regression example.")
}

#| write a spreadsheet holding the model-----
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
    "LAK", "regression", "example of a regression random forest",
    "LAK", "classification", "example of a classifcation random forest"
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
  if(T) {
    model_output_lm %>%
      openxlsx::writeDataTable( wb = output_wb,
                                sheet = "lm",
                                x = .,
                                startCol = 1,
                                startRow = 1,
                                tableStyle = "TableStyleLight9",
                                tableName = "lm")
    
    trees_df_lm  %>%
      # not strictly needed here now as this is also done in sql_to_excel 
      transmute(rule = str_replace_all(instruction, "\\d+\\.\\d+", function(x) as.character(round(as.numeric(x), 2)))) %>%
      openxlsx::writeDataTable( wb = output_wb,
                                sheet = "lmRules",
                                x = .,
                                startCol = 1,
                                startRow = 1,
                                tableStyle = "TableStyleLight9",
                                tableName = "lmRules")
  }
  
  # classification tree ----
  if(T) {
    model_output_clas %>%
      openxlsx::writeDataTable( wb = output_wb,
                                sheet = "classification",
                                x = .,
                                startCol = 1,
                                startRow = 1,
                                tableStyle = "TableStyleLight9",
                                tableName = "classification")
    
    trees_df_clas  %>%
      # not strictly needed here now as this is also done in sql_to_excel 
      transmute(rule = str_replace_all(instruction, "\\d+\\.\\d+", function(x) as.character(round(as.numeric(x), 2)))) %>%
      openxlsx::writeDataTable( wb = output_wb,
                                sheet = "classificationRules",
                                x = .,
                                startCol = 1,
                                startRow = 1,
                                tableStyle = "TableStyleLight9",
                                tableName = "classificationRules")
  }
  
  # regression tree ----
  if(F) {
    model_output_reg %>%
      openxlsx::writeDataTable( wb = output_wb,
                                sheet = "regression",
                                x = .,
                                startCol = 1,
                                startRow = 1,
                                tableStyle = "TableStyleLight9",
                                tableName = "regression")
  }
  trees_df_reg  %>%
    # not strictly needed here now as this is also done in sql_to_excel 
    transmute(rule = str_replace_all(instruction, "\\d+\\.\\d+", function(x) as.character(round(as.numeric(x), 2)))) %>%
    openxlsx::writeDataTable( wb = output_wb,
                              sheet = "regressionRules",
                              x = .,
                              startCol = 1,
                              startRow = 1,
                              tableStyle = "TableStyleLight9",
                              tableName = "regressionRules")
  
  
  message("Saving excel workbook...")  
  openxlsx::saveWorkbook(wb = output_wb,
                         file = here::here(stringr::str_c("posts/ML_in_excel/", "testing_", Sys.Date(), ".xlsx")),
                         overwrite = T)
  
  message("Done")
  beepr::beep()
}

