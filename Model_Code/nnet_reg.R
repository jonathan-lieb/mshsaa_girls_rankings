# Jonathan Lieb
# 2/9/2025

# This script is designed to find the best parameters for a 
# Neural Network model. It uses tune_bayes() and tidymodels

library(tidymodels)
library(tidyverse)
library(arrow)
library(future)
library(baguette)

basic_file_path <- gsub("/", "\\\\", file.path(Sys.getenv("USERPROFILE"), "OneDrive",
                                               "Documents", "mshsaa_girls_rankings"))
miceadds::source.all(paste(basic_file_path, "Functions\\", sep = "\\\\"))
# Data
data <- read_parquet("Data/modeling_data.parquet") |> 
  filter(season >= 2015) |> 
  impute_and_prep_data()

# Data Split
set.seed(77)
data_split <- group_initial_split(data, prop = 0.6, group = game_id)
train <- training(data_split)
test <- testing(data_split)

cv_folds <- group_vfold_cv(train, v = 3, group = game_id)

# Metric Set
metrics <- metric_set(rmse, rsq, mae, mape)

# Recipe
nnet_rec <- recipe(score ~ ., data = train) |> 
  update_role(school, opp, game_id, date, new_role = "ID") |>
  step_dummy(h_a_n) |>
  step_interact(terms = ~g_opp:papg_opp + g_opp:DWE_opp + g_opp:rpi_opp + g_opp:crpi_opp + 
                  g_opp:avg_pace_opp + g_opp:pace_control_opp) |>
  step_interact(terms  = ~g:ppg + g:OWE + g:rpi + g:crpi + g:avg_pace + g:pace_control) |> 
  step_normalize(all_predictors())

# Model Specification
nnet_spec <- bag_mlp(penalty = tune(), hidden_units = tune()) |> 
  set_engine("nnet") |> 
  set_mode("regression") |> 
  translate()

a = bake(prep(nnet_rec), new_data = test)

# Single fit 
# nnet_single_spec <- bag_mlp(hidden_units = 40) |>
#   set_engine("nnet") |> 
#   set_mode("regression") |> 
#   translate()
# 
# train_processed <- bake(prep(nnet_rec), new_data = train)
# test_processed <- bake(prep(nnet_rec), new_data = test)
# 
# nnet_single_wf <- workflow() |>
#   add_recipe(nnet_rec) |>
#   add_model(nnet_single_spec)
# 
# res_single <- fit(nnet_single_wf, data = train)
# 
# single_fit_metrics <- augment(res_single, new_data = test)
# single_fit_metrics |>
#   summarise(r_squared = 1 - sum((score - .pred)^2) / sum((score - mean(score))^2),
#             rmse = sqrt(mean(.resid^2)),
#             avg_error = mean(abs(.resid)))
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

nnet_wf <- workflow() |> 
  add_recipe(nnet_rec) |> 
  add_model(nnet_spec)

# Extract parameter set
nnet_params <- nnet_wf |>
  extract_parameter_set_dials() |> 
  update(hidden_units = hidden_units(range = c(1, 10)))


plan(multisession)


# Initial results
ctrl <- control_resamples(save_pred = TRUE, verbose = TRUE, parallel_over = "everything")
nnet_init_res <-
  nnet_wf |> 
  tune_grid(
    resamples = cv_folds,
    grid = nrow(nnet_params) + 8,
    param_info = nnet_params,
    metrics = metrics,
    control = ctrl
  )

nnet_init_metrics <- collect_metrics(nnet_init_res)

# Use Bayesian optimization to search for the best hyperparameters
ctrl_bayes <- control_bayes(verbose_iter = TRUE, parallel_over = "everything")
nnet_bayes_res <-
  nnet_wf |> 
  tune_bayes(
    resamples = cv_folds,
    initial = nnet_init_res,
    param_info = nnet_params,
    iter = 20,
    metrics = metrics,
    control = ctrl_bayes
  )

show_best(nnet_bayes_res, metric = "rmse") |> select(-.estimator) 

# Plot the results
autoplot(nnet_bayes_res, metric = "rmse") 
ggsave("Plots/nnet_bayes_res.png", width = 10, height = 8)

# Select best parameters
nnet_best_params <- select_best(nnet_bayes_res, metric = "rmse")

# Finalize the workflow
nnet_final_wf <- finalize_workflow(nnet_wf, nnet_best_params)

# Last Fit
final_nnet_res <- nnet_final_wf |> 
  last_fit(split = data_split, metrics = metrics)

# Show the results
final_nnet_res |> collect_metrics()


final_nnet_res |> collect_metrics() |> mutate(mod_type = "nnet_reg") |> 
  write_rds("Data/nnet_metrics.rds")

# Augment the results
final_nnet_res_aug <- augment(final_nnet_res)

# Check my school's games this year
calvary <- final_nnet_res_aug |> 
  filter(school == "Calvary Lutheran") |>
  arrange(desc(date))

# Plot the results by games played
final_nnet_res_aug |> 
  group_by(g) |>
  summarise(n = n(),
            avg_error = mean(abs(.resid))) |>
  ggplot(aes(x = g, y = avg_error)) +
  geom_point() +
  labs(title = "Average Error by Game: Test Data",
       x = "Game Number",
       y = "Average Error") +
  theme_minimal()

ggsave("Plots/nnet_avg_error_by_game.png", width = 10, height = 8)

# Save the model
library(butcher)
write_rds(butcher(final_nnet_res), "Models/nnet_nnet_model.rds")
