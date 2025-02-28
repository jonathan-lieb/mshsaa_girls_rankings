# Jonathan Lieb
# 2/13/2025

# This script is used to create a mixed effect model for the data 

# Load Libraries and Functions
library(tidymodels)
library(tidyverse)
library(arrow)
library(future)
library(multilevelmod)
library(broom.mixed)
library(lme4)

basic_file_path <- gsub("/", "\\\\", file.path(Sys.getenv("USERPROFILE"), "OneDrive",
                                               "Documents", "mshsaa_girls_rankings"))
miceadds::source.all(paste(basic_file_path, "Functions\\", sep = "\\\\"))

# Load quad data
quad_values <- read_rds("Data/quad_values.rds")

# Data
data <- read_parquet("Data/modeling_data.parquet") |> 
  filter(season >= 2015) |> 
  weight_history() |> 
  add_school_season_id() |> 
  add_quad(quad_values) 

# Data Split
set.seed(77)
data_split <- group_initial_split(data, prop = 0.6, group = game_id)
train <- training(data_split)
test <- testing(data_split)

# cv_folds <- group_vfold_cv(train, v = 3, group = game_id)

# Metric Set
# metrics <- metric_set(rmse, rsq, mae, mape)

# Model Formula
me_formula <- score ~ h_a_n_n + h_a_n_h  + rpi + rpi_opp + crpi + crpi_opp + 
  ppg + papg_opp + OWE + DWE_opp + o_rate + d_rate_opp + avg_pace + avg_pace_opp + 
  (1 | school_season_id)

# lm_formula <- score ~ h_a_n_n + h_a_n_h  + rpi + rpi_opp + crpi + crpi_opp + 
#   ppg + papg_opp + OWE + DWE_opp + o_rate + d_rate_opp + avg_pace + avg_pace_opp + 
#   ppg * OWE + DWE_opp * papg_opp

me_preprocessing_formula <- score ~ h_a_n+ ppg + OWE + rpi + crpi + 
  papg_opp + DWE_opp + rpi_opp + crpi_opp + g + g_opp + school_season_id + 
  school_season_id_opp + o_rate + d_rate_opp + avg_pace + avg_pace_opp + quad + quad_opp + 
  school

# Model Recipe
me_rec <- recipe(me_preprocessing_formula, data = train) |> 
  step_normalize(all_numeric_predictors()) |>
  step_dummy(h_a_n) 

me_spec <- linear_reg() |> 
  set_engine("lmer") |> 
  set_mode("regression")

# Model Workflow
me_wf <- workflow() |> 
  add_recipe(me_rec) |> 
  add_model(me_spec, formula = me_formula)

# Model Fitting
me_fit <- me_wf |> 
  fit(data = train)

tidy(me_fit) |> print(n = 100)
glance(me_fit)
random_effects = ranef(extract_fit_parsnip(me_fit)$fit)

random_effects

# Model Predictions
me_pred <- augment(me_fit, new_data = test)
rmse(me_pred, truth = score, estimate = .pred)
rsq(me_pred, truth = score, estimate = .pred)
mae(me_pred, truth = score, estimate = .pred)
mape(me_pred, truth = score, estimate = .pred)

me_pred |> 
  group_by(g) |>
  summarize(
    n = n(),
    avg_error = mean(abs(.resid))) |> 
  ggplot(aes(x = g, y = avg_error)) +
  geom_col() +
  geom_hline(yintercept = 7.5, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 10, linetype = "dashed", color = "blue") +
  theme_minimal() +
  labs(title = "Average Error by Game",
       x = "Games Played",
       y = "Average Error")

ggsave("Plots/me_avg_error_by_game.png", width = 10, height = 6)
