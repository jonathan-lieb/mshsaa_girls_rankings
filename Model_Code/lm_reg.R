# Jonathan Lieb
# 2/17/25

# This script fits a basic linear regression model to the data
# and evaluates its performance using RMSE and MAE

library(tidymodels)
library(tidyverse)
library(arrow)

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

# cv_folds <- group_vfold_cv(train, v = 3, group = game_id)

# Metric Set
# metrics <- metric_set(rmse, rsq, mae, mape)

# Model Formula
lm_formula <- score ~ h_a_n_h + h_a_n_n + ppg + OWE + rpi + crpi + 
  papg_opp + DWE_opp + rpi_opp + crpi_opp + g + g_opp + 
  o_rate + d_rate_opp + avg_pace + avg_pace_opp

lm_preprocessing_formula <- score ~ h_a_n+ ppg + OWE + rpi + crpi + 
  papg_opp + DWE_opp + rpi_opp + crpi_opp + g + g_opp +
  o_rate + d_rate_opp + avg_pace + avg_pace_opp

# Model Recipe
lm_rec <- recipe(lm_preprocessing_formula, data = train) |> 
  step_dummy(h_a_n) 

lm_spec <- linear_reg() |> 
  set_engine("lm") |> 
  set_mode("regression")

# Model Workflow
lm_wf <- workflow() |> 
  add_recipe(lm_rec) |> 
  add_model(lm_spec, formula = lm_formula)

# Model Fitting
lm_fit <- lm_wf |> 
  fit(data = train)

tidy(lm_fit) |> print(n = 100)
glance(lm_fit)

# Model Predictions
lm_pred <- augment(lm_fit, new_data = test)
rmse(lm_pred, truth = score, estimate = .pred)
rsq(lm_pred, truth = score, estimate = .pred)
mae(lm_pred, truth = score, estimate = .pred)
mape(lm_pred, truth = score, estimate = .pred)

lm_pred |> 
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

ggsave("Plots/lm_avg_error_by_game.png", width = 10, height = 6)
