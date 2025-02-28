# # This function creates modeling data that creates a results
# # full dataframe for each matchup and makes the process capable
# # of being done in parallel.

# This function requires the following packages:
# library(arrow)
# library(tidyverse)
# library(furrr)
# library(parallel)
# 
# # Parallel setup
# plan(multisession, workers = availableCores())


# Optimized create_modeling_data function
create_modeling_data <- function(df, new_data = T){
  seasons <- unique(df$season)
  df <- df |> 
    mutate(poss_est = pmax(score, opp_score) * .292960 + (score + opp_score) * .002931 + 31.601755,
           ppp = score / poss_est,
           ppp_opp = opp_score / poss_est)
  
  modeling_data <- future_map_dfr(seasons, function(s){
    past <- team_history(df, s)
    
    season_df <- df |> 
      filter(season == s)
    
    days <- unique(season_df$date)
    
    day_results <- future_map_dfr(days, function(d){
      day_df <- season_df |> 
        filter(date < d)
      
      day_team_results <- team_stats(day_df) |> 
        distinct(school, .keep_all = TRUE)
      
      day_matchups <- season_df |> 
        select(score, opp_score, school, class, opp, opp_class, h_a_n, date, season, game_id) |> 
        filter(date == d)
      
      day_full_matchups <- day_matchups |> 
        left_join(day_team_results, by = c("school" = "school")) |> 
        left_join(day_team_results, by = c("opp" = "school"), suffix = c("", "_opp")) |>
        left_join(past, by = c("school" = "school")) |>
        left_join(past, by = c("opp" = "school"), suffix = c("", "_opp")) 

      day_full_matchups
    })
    day_results
  }, .progress = TRUE)
  modeling_data
}

# Example usage
# library(tidyverse)
# library(arrow)
# data <- read_parquet("Data/cleaned.parquet")
# data_2025 <- data |> 
#   filter(season == 2025)
# team_stats_2025 <- team_stats(data_2025)
# Example 
# base <- read_parquet("C:/Users/Jonat/OneDrive/Documents/Sports Stats Analytics/MSHSAA Boys Basketball/mshsaa_app/Data/base.parquet")
# st <- proc.time()
# create_modeling_data(base) |> 
#   write_parquet("modeling_data.parquet")
# proc.time() - st
