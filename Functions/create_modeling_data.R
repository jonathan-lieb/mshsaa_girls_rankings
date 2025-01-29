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

team_stats <- function(data){
  league_avg <- mean(data$score, na.rm = TRUE)
  
  data <- data %>%
    group_by(opp) %>%
    mutate(
      opp_avg_allowed = mean(score),
      opp_avg_scored = mean(opp_score),
      opp_win_per = mean(opp_score > score),
      opp_class_val = mean(ifelse(!is.na(class), class, opp_class)) / 6
    ) %>%
    ungroup() %>%
    mutate(
      opp_strength_def = league_avg / opp_avg_allowed,
      opp_strength_off = opp_avg_scored / league_avg,
      weighted_points_off = score * opp_strength_def,
      weighted_points_def = opp_score / opp_strength_off,
      pace = pmax(score, opp_score)
    )
  
  team_summary <- data %>%
    group_by(school, class) %>%
    summarize(
      g = n(),
      OWE = mean(weighted_points_off, na.rm = TRUE),
      DWE = mean(weighted_points_def, na.rm = TRUE),
      ppg = mean(score),
      papg = mean(opp_score),
      eff_margin = OWE - DWE,
      pts_margin = ppg - papg,
      wp = mean(score > opp_score),
      owp = mean(opp_win_per, na.rm = TRUE),
      oowp = mean(data$opp_win_per[match(opp, data$school)], na.rm = TRUE),
      rpi = 0.25 * wp + 0.50 * owp + 0.25 * oowp,
      avg_pace = mean(pace),
      ocv = mean(opp_class_val),
      cv = ifelse(!is.na(first(class)), first(class) / 6, ocv),
      oocv = mean(data$opp_class_val[match(opp, data$school)], na.rm = TRUE),
      crpi = .25 * cv + .5 * oocv + .25 * ocv,
      .groups = "drop"
    ) |> 
    select(-class)
  
  team_summary
}

team_history <- function(df, sea){
  past <- df |> 
    filter(season >= (sea-3) & season < sea) |>
    group_by(season, school) |> 
    summarize(
      school = last(school),
      ppg = mean(score),
      papg = mean(opp_score),
    ) |> 
    arrange(desc(season)) |>
    group_by(school) |> 
    summarize(
      school = first(school),
      ppg_ly = first(ppg),
      papg_ly = first(papg),
      ppg_3y = mean(ppg),
      papg_3y = mean(papg),
    )
  past
}

calculate_pace_control <- function(scores, alpha, default_dispersion = 0.5) {
  # If only one game played, return default dispersion
  if (length(scores) <= 1) {
    return(default_dispersion)
  }
  
  med <- median(scores)
  mad <- mean(abs(scores - med))
  range <- max(scores) - min(scores)
  dispersion <- alpha * (mad / med) + (1 - alpha) * (range / med)
  1 - min(dispersion, 1)
}

# Function to optimize alpha
optimize_alpha <- function(df, target_median = 0.5, tolerance = 1e-3, default_dispersion = 0.5) {
  lower <- 0
  upper <- 1
  alpha <- (lower + upper) / 2
  
  if (nrow(df) == 0) {
    return(alpha)
  }
  
  while ((upper - lower) > tolerance) {
    pace_controls <- df %>%
      group_by(school) %>%
      summarise(pace_control = calculate_pace_control(score, alpha, default_dispersion)) %>%
      pull(pace_control)
    
    median_pace <- median(pace_controls)
    
    if (median_pace > target_median) {
      upper <- alpha
    } else {
      lower <- alpha
    }
    
    alpha <- (lower + upper) / 2
  }
  
  alpha
}

# Optimized create_modeling_data function
create_modeling_data <- function(df){
  seasons <- unique(base$season)
  modeling_data <- future_map_dfr(seasons, function(s){
    past <- team_history(df, s)
    
    season_df <- df |> 
      filter(season == s)
    
    days <- unique(season_df$date)
    
    day_results <- future_map_dfr(days, function(d){
      day_df <- season_df |> 
        filter(date < d)
      
      day_team_results <- team_stats(day_df)
      
      optimal_alpha <- optimize_alpha(day_df)
      
      pace_control <- day_df %>%
        group_by(school) %>%
        summarise(pace_control = calculate_pace_control(score, optimal_alpha)) %>%
        ungroup()
      
      day_matchups <- season_df |> 
        select(score, opp_score, school, class, opp, opp_class, h_a_n, date, season) |> 
        filter(date == d)
      
      day_full_matchups <- day_matchups |> 
        left_join(day_team_results, by = c("school" = "school")) |> 
        left_join(day_team_results, by = c("opp" = "school"), suffix = c("", "_opp")) |>
        left_join(past, by = c("school" = "school")) |>
        left_join(past, by = c("opp" = "school"), suffix = c("", "_opp")) |> 
        left_join(pace_control, by = c("school" = "school")) |>
        left_join(pace_control, by = c("opp" = "school"), suffix = c("", "_opp"))
      
      day_full_matchups
    })
    day_results
  }, .progress = TRUE)
  modeling_data
}


# Example 
# base <- read_parquet("C:/Users/Jonat/OneDrive/Documents/Sports Stats Analytics/MSHSAA Boys Basketball/mshsaa_app/Data/base.parquet")
# st <- proc.time()
# create_modeling_data(base) |> 
#   write_parquet("modeling_data.parquet")
# proc.time() - st
