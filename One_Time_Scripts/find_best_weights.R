# Jonathan Lieb
# 2/17/2025

# This script is designed to find the best weights for the league 
# average, past success, and current success for predicting the 
# score for a game.

# Each will start with a starting weight wa, wp, and wc.
# Then each will be multiplied by some constant between 
# 0 and 1 called c, which will be the same for each of the
# three weights.
# Each constant c will be raised to some power p, which will 
# increase with each game that is played. 
# The weights will always add to 1, so the weights will be:
# wa = league_avg * c^p
# wp = past_success * c^p
# wc = current_success * c^p

# The script will use the following libraries:
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
                             "OWE", "DWE_opp", "avg_pace",
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
  adj_data <- adj_weight_history(train, league_avg, past_success, current_success, c)
  
  lm_recipe <- recipe(score ~ h_a_n + rpi + rpi_opp + crpi + crpi_opp + 
                        ppg + papg + OWE + DWE + o_rate + d_rate + avg_pace +
                        ppg_opp + papg_opp + DWE_opp + OWE_opp + o_rate_opp +
                        d_rate_opp + avg_pace_opp,
                data = adj_data)
  
  cv_folds <- group_vfold_cv(adj_data, v = 3, group = game_id)
  
  lm_spec <- linear_reg() |>
    set_engine("lm") 
  
  lm_wf <- workflow() |>
    add_recipe(lm_recipe) |>
    add_model(lm_spec)
  
  metric_set <- metric_set(rmse, mae, rsq, mape)
  
  # Fit the model using 3 fold cv
  lm_fit <- fit_resamples(lm_wf,
                          resamples = cv_folds,
                          metrics = metric_set,
                          control = control_resamples(save_pred = TRUE))
  
  # Get the results
  collect_metrics(lm_fit) |> 
    mutate(league_avg = league_avg,
           past_success = past_success,
           current_success = current_success,
           c = c)
}, .progress = TRUE, .options = furrr_options(seed = TRUE))

top_rmse <- models |> 
  filter(.metric == "rmse") |> 
  arrange(mean)

top_mae <- models |>
  filter(.metric == "mae") |> 
  arrange(mean)

top_rsq <- models |>
  filter(.metric == "rsq") |> 
  arrange(desc(mean))

top_mape <- models |>
  filter(.metric == "mape") |> 
  arrange(mean)

top_models <- models |> 
  group_by(.metric) |>
  arrange(mean) |> 
  mutate(rank = ifelse(.metric == "rsq", n() - row_number() + 1, row_number())) |> 
  group_by(past_success, league_avg, c) |> 
  summarise(rank_rmse = rank[.metric == "rmse"],
            rank_rsq = rank[.metric == "rsq"],
            rank_mae = rank[.metric == "mae"],
            rank_mape = rank[.metric == "mape"]) |> 
  ungroup() |> 
  mutate(rank_mean = (rank_rmse+rank_rsq+rank_mae + rank_mape)/4,
         rank_max = pmax(rank_rmse, rank_rsq, rank_mae + rank_mape)) |> 
  arrange(rank_mean)
  
  

# Save the results
write_rds(models, "Data/weights_results.rds")

# Best results 
# la_weight = 0.2, ps_weight = 0.8, c = 0.8