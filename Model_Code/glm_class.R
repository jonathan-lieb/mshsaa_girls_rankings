# Jonathan Lieb
# 2/21/25

# Logistic regression to predict win or loss
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
  weight_history() |> 
  add_ternary() |> 
  mutate(w = factor(ifelse(score > opp_score, "w", "l"), 
                    levels = c("l", "w")))

# Data Split
set.seed(77)
data_split <- group_initial_split(data, prop = 0.6, group = game_id)
train <- training(data_split)
test <- testing(data_split)

# Count nas in columns
cv_folds <- group_vfold_cv(train, v = 3, group = game_id)

# Metric Set
metrics <- metric_set(brier_class, mn_log_loss, accuracy)

# Recipe
reg_rec <- recipe(w ~ h_a_n + ppg + papg +
                    OWE+ DWE + rpi + crpi + 
                    o_rate + d_rate +
                    ppg_opp + papg_opp + OWE_opp + DWE_opp + rpi_opp +
                    crpi_opp + o_rate_opp + d_rate_opp + avg_pace +
                    avg_pace_opp,
                  data = train)

prepped <- bake(prep(reg_rec, training = train), new_data = train)
count(prepped, h_a_n)

# step_interact(terms = ~g_opp:papg_opp + g_opp:DWE_opp + g_opp:rpi_opp + g_opp:crpi_opp + 
#                 g_opp:avg_pace_opp + g_opp:pace_control_opp) |>
# step_interact(terms  = ~g:ppg + g:OWE + g:rpi + g:crpi + g:avg_pace + g:pace_control) |> 
# step_normalize(all_predictors())

# Model Specification
reg_spec <- logistic_reg() |>
  set_engine("glm") |> 
  set_mode("classification")

reg_wf <- workflow() |> 
  add_recipe(reg_rec) |> 
  add_model(reg_spec)


# Initial results
ctrl <- control_resamples(save_pred = TRUE)

reg_fit <- reg_wf |> 
  fit_resamples(cv_folds, metrics = metrics, control = ctrl)

reg_fit |> collect_metrics()

# Last Fit
final_reg_res <- reg_wf |> 
  last_fit(split = data_split, metrics = metrics)

# Show the results
final_reg_res |> collect_metrics()


final_reg_res |> collect_metrics() |> mutate(mod_type = "class_glm") |> 
  write_rds("Data/class_glm_metrics.rds")

# Augment the results
final_reg_res_aug <- augment(final_reg_res)


# Plot the results by games played
final_reg_res_aug |> 
  group_by(g) |>
  summarise(accuracy = mean(.pred_class == w)) |> 
  ggplot(aes(x = g, y = accuracy)) +
  geom_col()+
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") +
  labs(title = "Classification Accuracy by Games",
       x = "Games",
       y = "Accuracy")

conf_mat(final_reg_res_aug, truth = w, estimate = .pred_class)

ggsave("Plots/classification_accuracy_by_game.png", width = 10, height = 8)

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

final_reg_res |> 
  extract_fit_parsnip() |> 
  tidy() |> 
  mutate(odds_ratio = exp(estimate)) |> 
  print(n = 30) |> 
  write_rds("Data/class_glm_odds_ratio.rds")


ggsave("Plots/class_reg_coefficients.png", width = 10, height = 8)

# Save the model
library(butcher)
butcher(extract_fit_parsnip(final_reg_res)) |> 
write_rds("Models/class_glm_model.rds")

mod <- read_rds("Models/class_glm_model.rds")
# Check my school's games this year
calvary <- data |> 
  filter(school == "Calvary Lutheran") |> 
  arrange(desc(date))

calvary_preds = augment(mod, new_data = calvary)
calvary |> 
  augment(mod, new_data = _)
