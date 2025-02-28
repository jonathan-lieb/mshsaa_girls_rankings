# Jonathan Lieb
# 2/18/2025

# This is to find the best rates for logistic regression

library(tidyverse)
library(tidymodels)
library(furrr)

# Grid of starting weights to test
weights <- tibble(
  league_avg = rep(c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1), each = 9),
  past_success = rep(c(1, .9, .8, .7, .6, .5, .4, .3, .2, .1, 0), each = 9),
  current_success = 0,
  c = rep(c(.1, .2, .3, .4, .5, .6, .7, .8, .9), 11)
)

adj_weight_history <- function(df, la_weight, ps_weight, cs_weight, 
                               constant){
  la_crpi <- .5
  la_rpi <- .5
  la_o_rate <- 0
  la_avg_pace <- 48
  la_ppg <- 45
  
  df |> 
    mutate(across(all_of(c("score", "g", "g_opp", "ppg", "papg_opp",
                           "crpi", "crpi_opp", "rpi", "rpi_opp", 
                           "o_rate", "d_rate", "o_rate_opp", "d_rate_opp",
                           "OWE", "DWE_opp", "avg_pace", "papg", 
                           "ppg_opp",
                           "DWE", "OWE_opp",
                           "o_rate_ly", "d_rate_ly", "o_rate_ly_opp", "d_rate_ly_opp",
                           "avg_pace_opp")), ~replace(., is.na(.), 0))) |> 
    mutate(across(all_of(c("ppg_ly", "papg_ly", "papg_ly_opp", "ppg_ly_opp",
                           "OWE_ly", "DWE_ly", "OWE_ly_opp", "DWE_ly_opp",
                           "ppg_3y", "papg_3y", "papg_3y_opp", "ppg_3y_opp",
                           "OWE_3y", "DWE_3y", "OWE_3y_opp", "DWE_3y_opp"
    )), 
    ~replace(., is.na(.), 45))) |>
    mutate(across(all_of(c("rpi_ly", "rpi_ly_opp", "crpi_ly", "crpi_ly_opp", 
                           "rpi_3y", "rpi_3y_opp", "crpi_3y", "crpi_3y_opp")), 
                  ~replace(., is.na(.), .5))) |>
    mutate(across(all_of(c("avg_pace_ly", "avg_pace_ly_opp",
                           "avg_pace_3y", "avg_pace_3y_opp")), 
                  ~replace(., is.na(.), 48))) |>
    mutate(
      lawg = la_weight * constant^g,
      lawg_opp = la_weight * constant^g_opp,
      pswg = ps_weight * constant^g,
      pswg_opp = ps_weight * constant^g_opp,
      cswg = 1 - lawg - pswg,
      cswg_opp = 1 - lawg_opp - pswg_opp,
      ppg = ppg*cswg + ppg_ly * pswg +  la_ppg * lawg,
      papg = papg_opp*cswg + papg_ly_opp * pswg + la_ppg * lawg,
      crpi = crpi*cswg + crpi_ly * pswg + la_crpi * lawg,
      rpi = rpi*cswg + rpi_ly * pswg + la_rpi * lawg,
      rpi_opp = rpi_opp*cswg_opp + rpi_ly_opp * pswg_opp + la_rpi * lawg_opp,
      papg_opp = papg_opp*cswg_opp + papg_ly_opp * pswg_opp + la_ppg * lawg_opp,
      crpi_opp = crpi_opp*cswg_opp + crpi_ly_opp * pswg_opp + la_crpi * lawg_opp,
      ppg_opp = ppg_opp*cswg_opp + ppg_ly_opp * pswg_opp + la_ppg * lawg_opp,
      OWE = OWE*cswg + OWE_ly * pswg + la_ppg * lawg,
      DWE = DWE*cswg + DWE_ly * pswg + la_ppg * lawg,
      DWE_opp = DWE_opp*cswg_opp + DWE_ly_opp * pswg_opp + la_ppg * lawg_opp,
      OWE_opp = OWE_opp*cswg_opp + OWE_ly_opp * pswg_opp + la_ppg * lawg_opp,
      o_rate = o_rate*cswg + o_rate_ly * pswg + la_o_rate * lawg,
      d_rate = d_rate*cswg + d_rate_ly * pswg + la_o_rate * lawg,
      o_rate_opp = o_rate_opp*cswg_opp + o_rate_ly_opp * pswg_opp + la_o_rate * lawg_opp,
      d_rate_opp = d_rate_opp*cswg_opp + d_rate_ly_opp * pswg_opp + la_o_rate * lawg_opp,
      avg_pace = avg_pace*cswg + avg_pace_ly * pswg + la_avg_pace * lawg,
      avg_pace_opp = avg_pace_opp*cswg_opp + avg_pace_ly_opp * pswg_opp + la_avg_pace * lawg_opp,
    )
}

# Load the data
library(arrow)
library(future)

basic_file_path <- gsub("/", "\\\\", file.path(Sys.getenv("USERPROFILE"), "OneDrive",
                                               "Documents", "mshsaa_girls_rankings"))
miceadds::source.all(paste(basic_file_path, "Functions\\", sep = "\\\\"))

data <- read_parquet("Data/modeling_data.parquet") |> 
  filter(season >= 2015) |> 
  add_ternary()

# Data Split
set.seed(77)
data_split <- group_initial_split(data, prop = 0.6, group = game_id)
train <- training(data_split)
test <- testing(data_split)

# Create a loop that will loop through each row of the weights tibble
# and run the adj_weight_history function on each row and then 
# fit a model to the testing data using 3 fold cv and that tracks the 
# RMSE and the MAE for each of the them and then save the results in a 
# tibble called results.

models <- future_map_dfr(1:nrow(weights), function(i){
  print(i/100)
  league_avg <- weights$league_avg[i]
  past_success <- weights$past_success[i]
  current_success <- weights$current_success[i]
  c <- weights$c[i]
  
  # Create a new dataframe with the adjusted weights
  adj_data <- adj_weight_history(train, league_avg, past_success, current_success, c) |> 
    mutate(w = factor(ifelse(score > opp_score, "w", "l"), levels = c("l", "w")))
  
  glm_recipe <- recipe(w ~ h_a_n + rpi + rpi_opp + crpi + crpi_opp + 
                        ppg + papg + ppg_opp + papg_opp +
                        OWE + OWE_opp + DWE + DWE_opp +
                        o_rate + o_rate_opp + d_rate + d_rate_opp + 
                         avg_pace + avg_pace_opp,
                      data = adj_data)
  
  cv_folds <- group_vfold_cv(adj_data, v = 3, group = game_id)
  
  glm_spec <- logistic_reg() |>
    set_engine("glm") 
  
  glm_wf <- workflow() |>
    add_recipe(glm_recipe) |>
    add_model(glm_spec)
  
  metric_set <- metric_set(brier_class, accuracy, mn_log_loss, roc_auc)
  
  # Fit the model using 3 fold cv
  glm_fit <- fit_resamples(glm_wf,
                          resamples = cv_folds,
                          metrics = metric_set,
                          control = control_resamples(save_pred = TRUE))
  
  # Get the results
  collect_metrics(glm_fit) |> 
    mutate(league_avg = league_avg,
           past_success = past_success,
           current_success = current_success,
           c = c)
}, .progress = TRUE, .options = furrr_options(seed = TRUE))

top_brier <- models |> 
  filter(.metric == "brier_class") |> 
  arrange(mean)

top_accuracy <- models |>
  filter(.metric == "accuracy") |> 
  arrange(desc(mean))

top_mn_log_loss <- models |>
  filter(.metric == "mn_log_loss") |> 
  arrange(mean)

top_roc_auc <- models |>
  filter(.metric == "roc_auc") |> 
  arrange(desc(mean))

ggplot(top_brier, aes(x = c, y = mean, color = factor(league_avg))) +
  geom_point() +
  geom_line() +
  labs(title = "Brier Class",
       x = "c",
       y = "Mean") +
  theme_minimal()

ggplot(top_accuracy, aes(x = c, y = mean, color = factor(league_avg))) +
  geom_point() +
  geom_line() +
  labs(title = "Accuracy",
       x = "c",
       y = "Mean") +
  theme_minimal()

ggplot(top_mn_log_loss, aes(x = c, y = mean, color = factor(league_avg))) +
  geom_point() +
  geom_line() +
  labs(title = "Mean Log Loss",
       x = "c",
       y = "Mean") +
  theme_minimal()

a = models |> 
  group_by(.metric) |>
  arrange(mean) |> 
  mutate(rank = ifelse(.metric == "accuracy", n() - row_number() + 1, row_number())) |> 
  group_by(past_success, league_avg, c) |> 
  summarise(rank_brier = rank[.metric == "brier_class"],
            rank_accuracy = rank[.metric == "accuracy"],
            rank_mn_log_loss = rank[.metric == "mn_log_loss"]) |> 
  ungroup() |> 
  mutate(rank_mean = (rank_brier+rank_accuracy+rank_mn_log_loss)/3,
        rank_max = pmax(rank_brier, rank_accuracy, rank_mn_log_loss)) |> 
  arrange(rank_mean)


# Save the results
write_rds(models, "Data/weights_results_class.rds")

# Best results 
# Top values, .6, .4, .8
# but .8, .2, .8 is best in regular regression and 3rd here