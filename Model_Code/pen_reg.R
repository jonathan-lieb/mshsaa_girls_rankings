# Jonathan Lieb
# 2/9/25

# This script is designed to find the best parameters for a regression model
# It uses tune_bayes and tidymodels

# Libraries
library(tidymodels)
library(tidyverse)
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

# Count nas in columns
sapply(train, function(x) sum(is.na(x)))

cv_folds <- group_vfold_cv(train, v = 3, group = game_id)

# Metric Set
metrics <- metric_set(rmse, rsq, mae, mape)


# Formula
reg_formula <- score ~ h_a_n_n + h_a_n_h  + rpi + rpi_opp + crpi + crpi_opp + 
  ppg + papg_opp + OWE + DWE_opp + o_rate + d_rate_opp + avg_pace + avg_pace_opp

reg_preprocessing_formula <- score ~ h_a_n+ ppg + OWE + rpi + crpi + 
  papg_opp + DWE_opp + rpi_opp + crpi_opp + g + g_opp + o_rate + d_rate_opp + avg_pace + avg_pace_opp

# Recipe
reg_rec <- recipe(score ~ ., data = train) |> 
  # update_role(school, opp, game_id, date, new_role = "ID") |>
  step_normalize(all_numeric_predictors()) |>
  step_dummy(h_a_n)
  # step_interact(terms = ~g_opp:papg_opp + g_opp:DWE_opp + g_opp:rpi_opp + g_opp:crpi_opp + 
  #                 g_opp:avg_pace_opp + g_opp:pace_control_opp) |>
  # step_interact(terms  = ~g:ppg + g:OWE + g:rpi + g:crpi + g:avg_pace + g:pace_control) |> 
  # step_normalize(all_predictors())

# Model Specification
reg_spec <- linear_reg(penalty = tune(), mixture = tune()) |>
  set_engine("glmnet") |> 
  translate()

reg_wf <- workflow() |> 
  add_recipe(reg_rec) |> 
  add_model(reg_spec, formula = reg_formula)

# Extract parameter set
reg_params <- reg_wf |>
  extract_parameter_set_dials() |> 
  update(penalty = penalty(range = c(-20, 0)))


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

# Plot the results
autoplot(reg_bayes_res, metric = "rmse") 
ggsave("Plots/reg_bayes_res.png", width = 10, height = 8)

# Select best parameters
reg_best_params <- select_best(reg_bayes_res, metric = "rmse")

# Finalize the workflow
reg_final_wf <- finalize_workflow(reg_wf, reg_best_params)

# Last Fit
final_reg_res <- reg_final_wf |> 
  last_fit(split = data_split, metrics = metrics)

# Show the results
final_reg_res |> collect_metrics()


final_reg_res |> collect_metrics() |> mutate(mod_type = "glmnet_reg") |> 
  write_rds("Data/reg_metrics.rds")

# Augment the results
final_reg_res_aug <- augment(final_reg_res)

# Check my school's games this year
calvary <- final_reg_res_aug |> 
  filter(school == "Calvary Lutheran") |>
  arrange(desc(date))

# Plot the results by games played
final_reg_res_aug |> 
  group_by(g) |>
  summarise(n = n(),
            avg_error = mean(abs(.resid))) |> 
  ggplot(aes(x = g, y = avg_error)) +
  geom_point() +
  labs(title = "Average Error by Game: Test Data",
       x = "Game Number",
       y = "Average Error") +
  theme_minimal()


ggsave("Plots/reg_avg_error_by_game.png", width = 10, height = 8)

# View the coefficients
final_reg_res |> 
  extract_fit_parsnip() |> 
  tidy() |> 
  arrange(desc(abs(estimate))) |> 
  ggplot(aes(x = reorder(term, abs(estimate)), y = estimate)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Coefficients",
       x = "Term",
       y = "Estimate") +
  theme_minimal()

ggsave("Plots/reg_coefficients.png", width = 10, height = 8)

# Save the model
library(butcher)
write_rds(butcher(final_reg_res), "Models/reg_reg_model.rds")
