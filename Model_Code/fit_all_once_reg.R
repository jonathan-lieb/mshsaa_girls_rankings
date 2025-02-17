# Jonathan Lieb
# 2/17/25

# This script tests a variety of different model types for to predict
# the score for the games. It does no hyperparameter tuning, and uses the
# default parameters for each model.

# It tests:
# Decision Tree
# Random Forest
# XGBoost Tree
# SVM
# Neural Network
# GLMNET Regression
# KNN
# Bayes Stan
# Poisson Regression
# Cubist Rules Model

# It uses the following metrics to evaluate the models:
# RMSE
# MAE

# Load the required libraries
library(tidyverse)
library(tidymodels)
library(xgboost)
library(kknn)
library(glmnet)
library(ranger)
library(arrow)
library(kernlab)
library(rstan)
library(poissonreg)
library(rules)

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

univ_formula <- score ~ h_a_n+ ppg + OWE + rpi + crpi + 
  papg_opp + DWE_opp + rpi_opp + crpi_opp + g + g_opp + o_rate +
  d_rate_opp + avg_pace + avg_pace_opp

# Preprocessing
rec <- recipe(univ_formula, data = train) |> 
  step_dummy(h_a_n) |>
  step_normalize(all_numeric_predictors())

# Model Specification
# Decision Tree
tree_spec <- decision_tree() |> 
  set_engine("rpart") |> 
  set_mode("regression")

# Random Forest
rf_spec <- rand_forest() |> 
  set_engine("ranger") |> 
  set_mode("regression")

# XGBoost Tree
xgb_spec <- boost_tree() |> 
  set_engine("xgboost") |> 
  set_mode("regression")

# SVM
svm_spec <- svm_rbf() |> 
  set_engine("kernlab") |> 
  set_mode("regression")

# Neural Network
nn_spec <- mlp() |> 
  set_engine("nnet") |> 
  set_mode("regression")

# GLMNET Regression
glmnet_spec <- linear_reg(penalty = 0.1, mixture = 0.5) |> 
  set_engine("glmnet") |> 
  set_mode("regression")

# KNN
knn_spec <- nearest_neighbor() |> 
  set_engine("kknn") |> 
  set_mode("regression")

# LM
lm_spec <- linear_reg() |> 
  set_engine("lm") |> 
  set_mode("regression")

# Bayes Stan
bayes_spec <- linear_reg() |> 
  set_engine("stan") |> 
  translate()

# Poisson Regression
poisson_spec <- poisson_reg(penalty = .1, mixture = .5) |> 
  set_engine("glmnet") |> 
  set_mode("regression") |> 
  translate()

# Cubist Rules Model
cubist_spec <- cubist_rules(
  committees = 1,
  neighbors = 0,
  max_rules = 50
) %>%
  set_engine("Cubist") %>%
  set_mode("regression") %>%
  translate()

# Fit the models
tree_fit <- workflow() |> 
  add_recipe(rec) |> 
  add_model(tree_spec) |> 
  fit(data = train)

rf_fit <- workflow() |>
  add_recipe(rec) |> 
  add_model(rf_spec) |> 
  fit(data = train)

xgb_fit <- workflow() |>
  add_recipe(rec) |> 
  add_model(xgb_spec) |> 
  fit(data = train)

# Too slow
# svm_fit <- workflow() |>
#   add_recipe(rec) |> 
#   add_model(svm_spec) |> 
#   fit(data = train)

nnet_fit <- workflow() |>
  add_recipe(rec) |> 
  add_model(nn_spec) |> 
  fit(data = train)

glmnet_fit <- workflow() |>
  add_recipe(rec) |> 
  add_model(glmnet_spec) |> 
  fit(data = train)

knn_fit <- workflow() |>
  add_recipe(rec) |> 
  add_model(knn_spec) |> 
  fit(data = train)

lm_fit <- workflow() |>
  add_recipe(rec) |> 
  add_model(lm_spec) |> 
  fit(data = train)

bayes_fit <- workflow() |>
  add_recipe(rec) |> 
  add_model(bayes_spec) |> 
  fit(data = train)

poisson_fit <- workflow() |>
  add_recipe(rec) |> 
  add_model(poisson_spec) |> 
  fit(data = train)

cubist_fit <- workflow() |>
  add_recipe(rec) |> 
  add_model(cubist_spec) |> 
  fit(data = train)

# Evaluate the models
tree_results <- augment(tree_fit, new_data = test) |> 
  metrics(truth = score, estimate = .pred) |> 
  mutate(model = "Decision Tree")

rf_results <- augment(rf_fit, new_data = test) |>
  metrics(truth = score, estimate = .pred) |> 
  mutate(model = "Random Forest")

xgb_results <- augment(xgb_fit, new_data = test) |>
  metrics(truth = score, estimate = .pred) |> 
  mutate(model = "XGBoost")

# Too slow
# svm_results <- augment(svm_fit, new_data = test) |>
#   metrics(truth = score, estimate = .pred) |>
#   mutate(model = "SVM")

nnet_results <- augment(nnet_fit, new_data = test) |>
  metrics(truth = score, estimate = .pred) |> 
  mutate(model = "Neural Network")

glmnet_results <- augment(glmnet_fit, new_data = test) |>
  metrics(truth = score, estimate = .pred) |> 
  mutate(model = "GLMNET")

knn_results <- augment(knn_fit, new_data = test) |>
  metrics(truth = score, estimate = .pred) |> 
  mutate(model = "KNN")

lm_results <- augment(lm_fit, new_data = test) |>
  metrics(truth = score, estimate = .pred) |> 
  mutate(model = "LM")

bayes_results <- augment(bayes_fit, new_data = test) |>
  metrics(truth = score, estimate = .pred) |> 
  mutate(model = "Bayes Stan")

poisson_results <- augment(poisson_fit, new_data = test) |>
  metrics(truth = score, estimate = .pred) |> 
  mutate(model = "Poisson")

cubist_results <- augment(cubist_fit, new_data = test) |> 
  metrics(truth = score, estimate = .pred) |> 
  mutate(model = "Cubist")

# Combine the results
results <- bind_rows(tree_results, rf_results, xgb_results,
                      nnet_results, glmnet_results, knn_results,
                      lm_results, bayes_results, poisson_results,
                      cubist_results
                     )

# Plot the results
results |> 
  filter(.metric %in% c("rmse")) |> 
  arrange(.estimate)

# .metric .estimator .estimate model         
# <chr>   <chr>          <dbl> <chr>         
# 1 rmse    standard        10.5 Neural Network
# 2 rmse    standard        10.6 Cubist        
# 3 rmse    standard        10.6 XGBoost       
# 4 rmse    standard        10.6 LM            
# 5 rmse    standard        10.6 Bayes Stan    
# 6 rmse    standard        10.6 GLMNET        
# 7 rmse    standard        10.6 Random Forest 
# 8 rmse    standard        10.7 Poisson       
# 9 rmse    standard        12.0 Decision Tree 
# 10 rmse    standard       12.1 KNN           


results |> 
  filter(.metric %in% c("rmse")) |> 
  ggplot(aes(x = reorder(model, .estimate), y = .estimate)) +
  geom_col(position = "dodge") +
  labs(title = "Model RMSE Performance",
       x = "Model",
       y = "RMSE") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Plots/reg_all_once_rmse.png", width = 8, height = 6)

results |> 
  filter(.metric %in% c("mae")) |> 
  arrange(.estimate)

# .metric .estimator .estimate model         
# <chr>   <chr>          <dbl> <chr>         
# 1 mae     standard        8.30 Neural Network
# 2 mae     standard        8.32 Cubist        
# 3 mae     standard        8.34 LM            
# 4 mae     standard        8.34 Bayes Stan    
# 5 mae     standard        8.34 XGBoost       
# 6 mae     standard        8.35 GLMNET        
# 7 mae     standard        8.38 Random Forest 
# 8 mae     standard        8.49 Poisson       
# 9 mae     standard        9.55 KNN           
# 10 mae     standard       9.60 Decision Tree 

results |> 
  filter(.metric %in% c("mae")) |> 
  ggplot(aes(x = reorder(model, .estimate), y = .estimate)) +
  geom_col(position = "dodge") +
  labs(title = "Model MAE Performance",
       x = "Model",
       y = "MAE") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Plots/reg_all_once_mae.png", width = 8, height = 6)

