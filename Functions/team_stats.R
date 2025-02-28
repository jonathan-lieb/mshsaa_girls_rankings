# Jonathan Lieb
# 2/25/25

# This function makes a summary of team statistics for a given dataset.
#It calculates the average points scored and allowed by each team's opponents, 
#the win percentage of each opponent, the average efficiency of each opponent,
#the strength of each opponent's defense and offense, 
#and the weighted points scored and allowed by each team. 
#It then calculates the overall efficiency, points per game, 
#points allowed per game, efficiency margin, points margin, wins, 
#losses, win percentage, overall win percentage, opponent's overall win percentage,
#RPI, average pace, class value, opponent's class value, and combined RPI for
#each team. The function returns a summary of these statistics for each team
#in the dataset.

team_stats <- function(data){
  league_avg <- mean(data$score, na.rm = TRUE)
  
  data <- data %>%
    group_by(opp) %>%
    mutate(
      opp_avg_allowed = mean(score),
      opp_avg_scored = mean(opp_score),
      opp_win_per = mean(opp_score > score),
      opp_class_val = mean(ifelse(!is.na(opp_class), opp_class, class)) / 6,
      opp_avg_eff = mean(ppp_opp),
      opp_avg_eff_allowed = mean(ppp),
    ) %>%
    ungroup() %>%
    mutate(
      opp_strength_def = league_avg / opp_avg_allowed,
      opp_strength_off = opp_avg_scored / league_avg,
      weighted_points_off = score * opp_strength_def,
      weighted_points_def = opp_score / opp_strength_off,
    )
  
  team_summary <- data %>%
    group_by(school, class, district, season) %>%
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
      wins = sum(score > opp_score),
      losses = sum(score < opp_score),
      wp = mean(score > opp_score),
      owp = mean(opp_win_per, na.rm = TRUE),
      oowp = mean(data$opp_win_per[match(opp, data$school)], na.rm = TRUE),
      rpi = 0.25 * wp + 0.50 * owp + 0.25 * oowp,
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
      .groups = "drop"
    ) |> 
    select(-class)
  
  team_summary
}
