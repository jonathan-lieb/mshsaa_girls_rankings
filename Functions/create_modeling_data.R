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
      opp_class_val = mean(ifelse(!is.na(opp_class), opp_class, class)) / 6,
      # opp_avg_poss = mean(poss_est),
      opp_avg_eff = mean(ppp_opp),
      opp_avg_eff_allowed = mean(ppp),
    ) %>%
    ungroup() %>%
    mutate(
      opp_strength_def = league_avg / opp_avg_allowed,
      opp_strength_off = opp_avg_scored / league_avg,
      weighted_points_off = score * opp_strength_def,
      weighted_points_def = opp_score / opp_strength_off,
      # pace = pmax(score, opp_score)
    )
  
  team_summary <- data %>%
    group_by(school, class) %>%
    summarize(
      g = n(),
      OWE = mean(weighted_points_off, na.rm = TRUE),
      DWE = mean(weighted_points_def, na.rm = TRUE),
      o_rate = mean(ppp - opp_avg_eff_allowed) * 100,
      d_rate = mean(opp_avg_eff - ppp_opp) * 100,
      ppg = mean(score),
      papg = mean(opp_score),
      eff_margin = OWE - DWE,
      pts_margin = ppg - papg,
      wp = mean(score > opp_score),
      owp = mean(opp_win_per, na.rm = TRUE),
      oowp = mean(data$opp_win_per[match(opp, data$school)], na.rm = TRUE),
      rpi = 0.25 * wp + 0.50 * owp + 0.25 * oowp,
      avg_pace = mean(poss_est),
      # avg_pace = mean(pace),
      ocv = mean(opp_class_val),
      cv = ifelse(!is.na(first(class)), first(class) / 6, ocv),
      oocv = mean(data$opp_class_val[match(opp, data$school)], na.rm = TRUE),
      crpi = case_when(
        !is.na(cv) & !is.na(ocv) & !is.na(oocv) ~ .25 * cv + .5 * ocv + .25 * oocv,
        !is.na(cv) & !is.na(ocv) ~ .33 * cv + .67 * ocv,
        !is.na(cv) & !is.na(oocv) ~ .5 * cv + .5 * oocv,
        !is.na(ocv) & !is.na(oocv) ~ .67 * ocv + .33 * oocv,
        !is.na(cv) ~ cv,
        !is.na(ocv) ~ ocv,
        !is.na(oocv) ~ oocv,
        TRUE ~ NA),
      .groups = "drop"
    ) |> 
    select(-class)
  
  team_summary
}

team_history <- function(df, sea){
  
  league_avg <- mean(df$score, na.rm = TRUE)
  
  data <- df |> 
    filter(season >= (sea-3) & season < sea) |>
    group_by(season, opp) |> 
    mutate(opp_class_val = mean(ifelse(!is.na(opp_class), opp_class, class)) / 6,
           opp_wper = mean(opp_score > score, na.rm = T),
           opp_avg_allowed = mean(score),
           opp_avg_scored = mean(opp_score),
           opp_avg_eff = mean(ppp_opp),
           opp_avg_eff_allowed = mean(ppp)) |> 
    ungroup() |> 
    mutate(
    opp_strength_def = league_avg / opp_avg_allowed,
    opp_strength_off = opp_avg_scored / league_avg,
    weighted_points_off = score * opp_strength_def,
    weighted_points_def = opp_score / opp_strength_off,
    # pace = pmax(score, opp_score),
    )
  
  past <- data |>
    group_by(season, school) |> 
    summarize(
      school = last(school),
      ppg = mean(score),
      papg = mean(opp_score),
      OWE = mean(weighted_points_off, na.rm = TRUE),
      DWE = mean(weighted_points_def, na.rm = TRUE),
      o_rate = mean(ppp - opp_avg_eff_allowed) * 100,
      d_rate = mean(opp_avg_eff - ppp_opp) * 100,
      avg_pace = mean(poss_est),
      ocv = mean(opp_class_val),
      cv = ifelse(!is.na(first(class)), first(class) / 6, ocv),
      oocv = mean(data$opp_class_val[match(opp, data$school)], na.rm = TRUE),
      crpi = case_when(
        !is.na(cv) & !is.na(ocv) & !is.na(oocv) ~ .25 * cv + .5 * ocv + .25 * oocv,
        !is.na(cv) & !is.na(ocv) ~ .33 * cv + .67 * ocv,
        !is.na(cv) & !is.na(oocv) ~ .5 * cv + .5 * oocv,
        !is.na(ocv) & !is.na(oocv) ~ .67 * ocv + .33 * oocv,
        !is.na(cv) ~ cv,
        !is.na(ocv) ~ ocv,
        !is.na(oocv) ~ oocv,
        TRUE ~ NA),
      owp = mean(opp_wper, na.rm = TRUE),
      wp = mean(score > opp_score, na.rm = TRUE),
      oowp = mean(data$opp_wper[match(opp, data$school)], na.rm = TRUE),
      rpi = case_when(
        !is.na(wp) & !is.na(owp) & !is.na(oowp) ~ .25 * wp + .5 * owp + .25 * oowp,
        !is.na(wp) & !is.na(owp) ~ .33 * wp + .67 * owp,
        !is.na(wp) & !is.na(oowp) ~ .5 * wp + .5 * oowp,
        !is.na(owp) & !is.na(oowp) ~ .67 * owp + .33 * oowp,
        !is.na(wp) ~ wp,
        !is.na(owp) ~ owp,
        !is.na(oowp) ~ oowp,
        TRUE ~ NA)
    ) |> 
    arrange(desc(season)) |>
    group_by(school) |> 
    summarize(
      school = first(school),
      ppg_ly = first(ppg),
      papg_ly = first(papg),
      ppg_3y = mean(ppg),
      papg_3y = mean(papg),
      crpi_ly = first(crpi),
      crpi_3y = mean(crpi, na.rm = TRUE),
      rpi_ly = first(rpi),
      rpi_3y = mean(rpi, na.rm = TRUE),
      OWE_ly = first(OWE),
      OWE_3y = mean(OWE, na.rm = TRUE),
      DWE_ly = first(DWE),
      DWE_3y = mean(DWE, na.rm = TRUE),
      o_rate_ly = first(o_rate),
      o_rate_3y = mean(o_rate, na.rm = TRUE),
      d_rate_ly = first(d_rate),
      d_rate_3y = mean(d_rate, na.rm = TRUE),
      avg_pace_ly = first(avg_pace),
      avg_pace_3y = mean(avg_pace, na.rm = TRUE),
    )
  past
}

# calculate_pace_control <- function(scores, alpha, default_dispersion = 0.5) {
#   # If only one game played, return default dispersion
#   if (length(scores) <= 1) {
#     return(default_dispersion)
#   }
#   
#   med <- median(scores)
#   mad <- mean(abs(scores - med))
#   range <- max(scores) - min(scores)
#   dispersion <- alpha * (mad / med) + (1 - alpha) * (range / med)
#   1 - min(dispersion, 1)
# }

# Function to optimize alpha
# optimize_alpha <- function(df, target_median = 0.5, tolerance = 1e-3, default_dispersion = 0.5) {
#   lower <- 0
#   upper <- 1
#   alpha <- (lower + upper) / 2
#   
#   if (nrow(df) == 0) {
#     return(alpha)
#   }
#   
#   while ((upper - lower) > tolerance) {
#     pace_controls <- df %>%
#       group_by(school) %>%
#       summarise(pace_control = calculate_pace_control(score, alpha, default_dispersion)) %>%
#       pull(pace_control)
#     
#     median_pace <- median(pace_controls)
#     
#     if (median_pace > target_median) {
#       upper <- alpha
#     } else {
#       lower <- alpha
#     }
#     
#     alpha <- (lower + upper) / 2
#   }
#   
#   alpha
# }

# Optimized create_modeling_data function
create_modeling_data <- function(df){
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
      
      # optimal_alpha <- optimize_alpha(day_df)
      
      # pace_control <- day_df %>%
      #   group_by(school) %>%
      #   summarise(pace_control = calculate_pace_control(score, optimal_alpha)) %>%
      #   ungroup()
      
      day_matchups <- season_df |> 
        select(score, opp_score, school, class, opp, opp_class, h_a_n, date, season, game_id) |> 
        filter(date == d)
      
      day_full_matchups <- day_matchups |> 
        left_join(day_team_results, by = c("school" = "school")) |> 
        left_join(day_team_results, by = c("opp" = "school"), suffix = c("", "_opp")) |>
        left_join(past, by = c("school" = "school")) |>
        left_join(past, by = c("opp" = "school"), suffix = c("", "_opp")) #|> 
        # left_join(pace_control, by = c("school" = "school")) |>
        # left_join(pace_control, by = c("opp" = "school"), suffix = c("", "_opp"))
      
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
