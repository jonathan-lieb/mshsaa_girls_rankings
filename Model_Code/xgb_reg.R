# Jonathan Lieb
# 2/5/25

# This script is designed to be used to find the best parameters for a
# xgboosted tree model. It uses tune_bayes and tidymodels

# Libraries
library(tidymodels)
library(tidyverse)
library(xgboost)
library(arrow)
library(future)

basic_file_path <- gsub("/", "\\\\", file.path(Sys.getenv("USERPROFILE"), "OneDrive",
                                               "Documents", "mshsaa_girls_rankings"))
miceadds::source.all(paste(basic_file_path, "Functions\\", sep = "\\\\"))
# Data
data <- read_parquet("Data/modeling_data.parquet") |> 
  filter(season >= 2015) |> 
  weight_history()

# Data Split
set.seed(77)
data_split <- group_initial_split(data, prop = 0.6, group = game_id)
train <- training(data_split)
test <- testing(data_split)

cv_folds <- group_vfold_cv(train, v = 3, group = game_id)

# Metric Set
metrics <- metric_set(rmse, rsq, mae, mape)


xgb_formula <- score ~ h_a_n_n + h_a_n_h  + rpi + rpi_opp + crpi + crpi_opp + 
  ppg + papg_opp + OWE + DWE_opp + o_rate + d_rate_opp + avg_pace + avg_pace_opp

# score ~ h_a_n + (1 + ppg + OWE + rpi + crpi | g) + 
# (1 + papg_opp + DWE_opp + rpi_opp + crpi_opp | g_opp) + 
# (1 | school_season_id)

xgb_preprocessing_formula <- score ~ h_a_n+ ppg + OWE + rpi + crpi + 
  papg_opp + DWE_opp + rpi_opp + crpi_opp + g + g_opp + o_rate + d_rate_opp + avg_pace + avg_pace_opp

# Recipe
xgb_rec <- recipe(xgb_preprocessing_formula, data = train) |> 
  step_dummy(h_a_n) |>
  step_normalize(all_numeric_predictors())
  # step_interact(terms = ~g_opp:papg_opp + g_opp:DWE_opp + g_opp:rpi_opp + g_opp:crpi_opp + 
  #                  g_opp:avg_pace_opp + g_opp:pace_control_opp) |>
  # step_interact(terms  = ~g:ppg + g:OWE + g:rpi + g:crpi + g:avg_pace + g:pace_control)

# Model Specification
xgb_spec <- boost_tree(trees = tune(), tree_depth = tune(), min_n = tune(),
                       learn_rate = tune(), mtry = tune()) |> 
  set_engine("xgboost") |> 
  set_mode("regression")

# Single xgboost model
# xgb_single_spec <- boost_tree() |>
#   set_engine("xgboost") |>
#   set_mode("regression")
# 
# train_processed <- bake(prep(xgb_rec), new_data = train)
# test_processed <- bake(prep(xgb_rec), new_data = test)
# 
# xgb_single_wf <- workflow() |>
#   add_recipe(xgb_rec) |>
#   add_model(xgb_single_spec, formula = xgb_formula)
# 
# res_single <- fit(xgb_single_wf, data = train)
# 
# single_fit_metrics <- augment(res_single, new_data = test)
# single_fit_metrics |>
#   rmse(truth = score, estimate = .pred)
# single_fit_metrics |>
#   rsq(truth = score, estimate = .pred)
# single_fit_metrics |>
#   mae(truth = score, estimate = .pred)
# single_fit_metrics |>
#   mape(truth = score, estimate = .pred)
# 
# by_games <- single_fit_metrics |> 
#   group_by(g) |>
#   summarise(n = n(),
#     r_squared = 1 - sum((score - .pred)^2) / sum((score - mean(score))^2),
#             avg_error = mean(abs(score - .pred)))
# 
# by_games |> 
#   ggplot(aes(x = g, y = avg_error)) +
#   geom_point()

# Create a workflow
xgb_wf <- workflow() |> 
  add_recipe(xgb_rec) |> 
  add_model(xgb_spec, formula = xgb_formula)

# Extract parameter set
xgb_params <- xgb_wf |>
  extract_parameter_set_dials() |> 
  update(mtry = mtry(range = c(2, 10)),
         min_n = min_n(range = c(10, 30)),
         tree_depth = tree_depth(range = c(10, 25)),
         learn_rate = learn_rate(range = c(-3, -.75)),
         trees = trees(range = c(25, 500)))


plan(multisession)

# Initial results
ctrl <- control_resamples(save_pred = TRUE, verbose = TRUE, parallel_over = "everything")
xgb_init_res <-
  xgb_wf |> 
  tune_grid(
    resamples = cv_folds,
    grid = nrow(xgb_params) + 8,
    param_info = xgb_params,
    metrics = metrics,
    control = ctrl
  )

xgb_init_metrics <- collect_metrics(xgb_init_res)

# Use Bayesian optimization to search for the best hyperparameters
ctrl_bayes <- control_bayes(verbose_iter = TRUE, parallel_over = "everything")
xgb_bayes_res <-
  xgb_wf |> 
  tune_bayes(
    resamples = cv_folds,
    initial = xgb_init_res,
    param_info = xgb_params,
    iter = 20,
    metrics = metrics,
    control = ctrl_bayes
  )

show_best(xgb_bayes_res, metric = "rmse") |> select(-.estimator) 

# Plot the results
autoplot(xgb_bayes_res, metric = "rmse") 
ggsave("Plots/xgb_bayes_res.png", width = 10, height = 8)

# Select best parameters
xgb_best_params <- select_best(xgb_bayes_res, metric = "rmse")

# Finalize the workflow
xgb_final_wf <- finalize_workflow(xgb_wf, xgb_best_params)

# Last Fit
final_xgb_res <- xgb_final_wf |> 
  last_fit(split = data_split, metrics = metrics)

# Show the results
final_xgb_res |> collect_metrics()
final_xgb_res |> collect_predictions() |> 
  mutate(mod_type = "xgb_reg") |>
  write_rds("Data/xgb_metrics.rds")

# Augment the results
final_xgb_res_aug <- augment(final_xgb_res)

# Check my school's games this year
calvary <- final_xgb_res_aug |> 
  filter(school == "Calvary Lutheran") |>
  arrange(desc(date))

# Plot the results by games played
final_xgb_res_aug |> 
  group_by(g) |>
  summarise(n = n(),
            avg_error = mean(abs(.resid))) |>
  ggplot(aes(x = g, y = avg_error)) +
  geom_point() +
  labs(title = "Average Error by Game: Test Data",
       x = "Game Number",
       y = "Average Error") +
  theme_minimal()

ggsave("Plots/xgb_avg_error_by_game.png", width = 10, height = 8)


library(vip)

vip(extract_workflow(final_xgb_res), n = 30)
ggsave("Plots/xgb_vip.png", width = 10, height = 8)

# Save the model
library(butcher)
write_rds(butcher(final_xgb_res), "Models/xgb_reg_model.rds")
