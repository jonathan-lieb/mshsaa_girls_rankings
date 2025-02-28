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
  weight_history() |> 
  add_ternary()

# Data Split
set.seed(77)
data_split <- group_initial_split(data, prop = 0.6, group = game_id)
train <- training(data_split)
test <- testing(data_split)

# Model Recipe
lm_rec <- recipe(score~h_a_n + rpi + rpi_opp + crpi + crpi_opp + 
                   ppg + papg_opp + o_rate + d_rate_opp + DWE_opp+
                   avg_pace + avg_pace_opp + OWE_opp+
                   papg + OWE + ppg_opp + DWE + d_rate + o_rate_opp # Extras
                   , data = train)
lm_opp_rec <- recipe(opp_score~h_a_n + rpi + rpi_opp + crpi + crpi_opp + 
                   ppg + papg_opp + o_rate + d_rate_opp + DWE_opp+
                   avg_pace + avg_pace_opp + OWE_opp+
                   papg + OWE + ppg_opp + DWE + d_rate + o_rate_opp # Extras
                   , data = train)

lm_spec <- linear_reg() |> 
  set_engine("lm") |> 
  set_mode("regression")

lm_opp_spec <- linear_reg() |> 
  set_engine("lm") |> 
  set_mode("regression")

# Model Workflow
lm_wf <- workflow() |> 
  add_recipe(lm_rec) |> 
  add_model(lm_spec)

lm_opp_wf <- workflow() |>
  add_recipe(lm_opp_rec) |> 
  add_model(lm_opp_spec)

# Model Fitting
lm_fit <- lm_wf |> 
  fit(data = train)

lm_opp_fit <- lm_opp_wf |>
  fit(data = train)

tidy(lm_fit) |> print(n = 100) |> write_rds("Data/lm_reg_coeffs.rds")
glance(lm_fit)

tidy(lm_opp_fit) |> print(n = 100) |> write_rds("Data/lm_opp_reg_coeffs.rds")
glance(lm_opp_fit)
# Model Predictions
lm_pred <- augment(lm_fit, new_data = test)
lm_opp_pred <- augment(lm_opp_fit, new_data = test)

rmse(lm_pred, truth = score, estimate = .pred)
rmse(lm_opp_pred, truth = opp_score, estimate = .pred)
# 10.5
rsq(lm_pred, truth = score, estimate = .pred)
rsq(lm_opp_pred, truth = opp_score, estimate = .pred)
# .529
mae(lm_pred, truth = score, estimate = .pred)
mae(lm_opp_pred, truth = opp_score, estimate = .pred)
# 8.24
mape(lm_pred, truth = score, estimate = .pred)
mape(lm_opp_pred, truth = opp_score, estimate = .pred)
# 23.2

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

# This is my selected model
write_rds(lm_fit, "Models/lm_reg.rds")
write_rds(lm_opp_fit, "Models/lm_opp_reg.rds")

reg_mod <- read_rds("Models/lm_reg.rds")
reg_opp_mod <- read_rds("Models/lm_opp_reg.rds")
log_class_mod <- read_rds("Models/class_glm_model.rds")

calvary <- data |> 
  filter(school == "Calvary Lutheran"|opp == "Calvary Lutheran") |> 
  arrange(desc(date)) 

calvary_preds <- calvary |> 
  mutate(pred_score = predict(reg_mod, new_data = calvary)$.pred) |>
  mutate(pred_opp_score = predict(reg_opp_mod, new_data = calvary)$.pred) |> 
  mutate(predict(log_class_mod, new_data = calvary, type = "prob")) |> 
  select(date, school, opp, score, opp_score, pred_score, pred_opp_score, .pred_w, .pred_l)
 