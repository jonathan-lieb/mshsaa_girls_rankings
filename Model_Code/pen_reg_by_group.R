# Jonathan Lieb
# 2/11/25

# This script is designed to repeat a glmnet tuning process for each 
# combination of g and g_opp. The goal is to allow for different models
# based on how much information is available.

# Libraries
library(tidymodels)
library(tidyverse)
library(arrow)
library(future)
library(butcher)

basic_file_path <- gsub("/", "\\\\", file.path(Sys.getenv("USERPROFILE"), "OneDrive",
                                               "Documents", "mshsaa_girls_rankings"))
miceadds::source.all(paste(basic_file_path, "Functions\\", sep = "\\\\"))

# Full Data
data <- read_parquet("Data/modeling_data.parquet") |> 
  filter(season >= 2015) |> 
  impute_and_prep_data()

# Data Split by g and g_opp
data_groups <- data |> 
  mutate(grouping = case_when(
    g == 0 & g_opp == 0 ~ "0-0",
    g == 0 & g_opp == 1 ~ "0-1",
    g == 0 & g_opp %in% 2:4 ~ "0-2to4",
    g == 0 & g_opp %in% 5:9 ~ "0-5to9",
    g == 0 & g_opp >= 10 ~ "0-10plus",
    g == 1 & g_opp == 0 ~ "1-0",
    g == 1 & g_opp == 1 ~ "1-1",
    g == 1 & g_opp %in% 2:4 ~ "1-2to4",
    g == 1 & g_opp %in% 5:9 ~ "1-5to9",
    g == 1 & g_opp >= 10 ~ "1-10plus",
    g %in% 2:4 & g_opp == 0 ~ "2to4-0",
    g %in% 2:4 & g_opp == 1 ~ "2to4-1",
    g %in% 2:4 & g_opp %in% 2:4 ~ "2to4-2to4",
    g %in% 2:4 & g_opp %in% 5:9 ~ "2to4-5to9",
    g %in% 2:4 & g_opp >= 10 ~ "2to4-10plus",
    g %in% 5:9 & g_opp == 0 ~ "5to9-0",
    g %in% 5:9 & g_opp == 1 ~ "5to9-1",
    g %in% 5:9 & g_opp %in% 2:4 ~ "5to9-2to4",
    g %in% 5:9 & g_opp %in% 5:9 ~ "5to9-5to9",
    g %in% 5:9 & g_opp >= 10 ~ "5to9-10plus",
    g >= 10 & g_opp == 0 ~ "10plus-0",
    g >= 10 & g_opp == 1 ~ "10plus-1",
    g >= 10 & g_opp %in% 2:4 ~ "10plus-2to4",
    g >= 10 & g_opp %in% 5:9 ~ "10plus-5to9",
    g %in% 10:19 & g_opp %in% 10:19 ~ "10to19-10to19",
    g %in% 10:19 & g_opp >= 20 ~ "10to19-20plus",
    # g >= 20 & g_opp %in% 10:19 ~ "20plus-10to19",
    g >= 20 & g_opp >= 20 ~ "20plus-20plus",
    T ~ NA
  ))

errs <- data_groups |> 
  filter(is.na(grouping)) |> 
  count(g, g_opp)

counts <- data_groups |> 
  group_by(grouping) |> 
  summarize(n = n()) |> 
  arrange(desc(n))

data_group_splits <- data_groups |> 
  group_split(grouping)

set.seed(77)

tune_glm_model <- function(data){
  data <- data |> select(-grouping)
  # Data Split
  data_split <- group_initial_split(data, prop = 0.8, group = game_id)
  train <- training(data_split)
  test <- testing(data_split)
  
  cv_folds <- group_vfold_cv(train, v = 3, group = game_id)
  
  
  # Metric Set
  metrics <- metric_set(rmse, rsq, mae, mape)
  
  # Recipe
  reg_rec <- recipe(score ~ ., data = train) |> 
    update_role(school, opp, game_id, date, new_role = "ID") |>
    step_dummy(h_a_n) |> 
    step_zv(all_predictors()) |> 
    step_normalize(all_predictors())

  # Model Specification
  reg_spec <- linear_reg(penalty = tune(), mixture = tune()) |>
    set_engine("glmnet") |> 
    translate()
  
  reg_wf <- workflow() |> 
    add_recipe(reg_rec) |> 
    add_model(reg_spec)
  
  # Extract parameter set
  reg_params <- reg_wf |>
    extract_parameter_set_dials() 
  
  plan(multisession)
  
  # Initial results
  ctrl <- control_resamples(save_pred = TRUE, parallel_over = "everything")
  reg_init_res <-
    reg_wf |> 
    tune_grid(
      resamples = cv_folds,
      grid = nrow(reg_params) + 8,
      param_info = reg_params,
      metrics = metrics,
      control = ctrl
    )
  
  reg_init_metrics <- collect_metrics(reg_init_res)
  
  # Use Bayesian optimization to search for the best hyperparameters
  ctrl_bayes <- control_bayes(verbose_iter = TRUE, parallel_over = "everything")
  reg_bayes_res <-
    reg_wf |> 
    tune_bayes(
      resamples = cv_folds,
      initial = reg_init_res,
      param_info = reg_params,
      iter = 20,
      metrics = metrics,
      control = ctrl_bayes
    )
  
  show_best(reg_bayes_res, metric = "rmse") |> select(-.estimator) 
  
  
  # Select best parameters
  reg_best_params <- select_best(reg_bayes_res, metric = "rmse")
  
  # Finalize the workflow
  reg_final_wf <- finalize_workflow(reg_wf, reg_best_params)
  
  # Last Fit
  final_reg_res <- reg_final_wf |> 
    last_fit(split = data_split, metrics = metrics)
  
  # Show the results
  print(final_reg_res |> collect_metrics())
  
  
  # final_reg_res |> collect_metrics() |> mutate(mod_type = "glmnet_reg") |> 
  #   write_rds("Data/reg_metrics.rds")
  
  # Augment the results
  final_reg_res_aug <- augment(final_reg_res)

  # Save the model
  butcher(final_reg_res)
}

df_models <- map(data_group_splits, tune_glm_model)
names <- map_chr(data_group_splits, ~ paste(.x$grouping[1])) |> 
  str_replace_all("NA", "20plus-10to19")

model_list <- set_names(df_models, names)

model_list |> map( ~ .x |> collect_metrics() |> filter(.metric == "rsq") |> 
                     pull(.estimate))

model_list |> map(~.x |> collect_predictions() |> pull(score) |> sd() -
                    .x |> collect_metrics() |> filter(.metric == "rmse") |> pull(.estimate))


write_rds(model_list, "Models/pen_reg_group_models.rds")

predict_grouping_model <- function(new_data, model_list) {
  new_data %>%
    impute_and_prep_data() %>%
    add_groupings() %>%
    rowwise() %>%
    mutate(
      # Ensure we select only the relevant columns (based on model's expected columns)
      .pred = {
        model <- model_list[[grouping]]
        
        # Extract the names of the columns the model expects
        model_columns <- model$preprocessor$trained$x %>% colnames()
        
        # Select only the necessary columns from the new_data to match the model's expected columns
        selected_data <- cur_data() %>%
          select(all_of(model_columns))
        
        # Use augment with selected data
        augment(extract_fit_parsnip(model), new_data = selected_data)$.fitted
      }
    ) %>%
    ungroup() 
}

