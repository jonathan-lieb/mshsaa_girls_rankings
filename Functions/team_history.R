# Jonathan Lieb
# 2/25/25

# This function creates historical team data.
team_history <- function(df, sea){
  
  league_avg <- mean(df$score, na.rm = TRUE)
  
  data <- df |> 
    filter(season == (sea-1)) |>
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
    )
  
  past <- data |>
    group_by(season, school) |> 
    summarize(
      school = last(school),
      season = last(season),
      ppg_ly = mean(score),
      papg_ly = mean(opp_score),
      OWE_ly = mean(weighted_points_off, na.rm = TRUE),
      DWE_ly = mean(weighted_points_def, na.rm = TRUE),
      o_rate_ly = mean(ppp - opp_avg_eff_allowed) * 100,
      d_rate_ly = mean(opp_avg_eff - ppp_opp) * 100,
      avg_pace_ly = mean(poss_est),
      ocv = mean(opp_class_val),
      cv = ifelse(!is.na(first(class)), first(class) / 6, ocv),
      oocv = mean(data$opp_class_val[match(opp, data$school)], na.rm = TRUE),
      crpi_ly = case_when(
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
      rpi_ly = case_when(
        !is.na(wp) & !is.na(owp) & !is.na(oowp) ~ .25 * wp + .5 * owp + .25 * oowp,
        !is.na(wp) & !is.na(owp) ~ .33 * wp + .67 * owp,
        !is.na(wp) & !is.na(oowp) ~ .5 * wp + .5 * oowp,
        !is.na(owp) & !is.na(oowp) ~ .67 * owp + .33 * oowp,
        !is.na(wp) ~ wp,
        !is.na(owp) ~ owp,
        !is.na(oowp) ~ oowp,
        TRUE ~ NA)
    ) |> 
    select(school, season, ends_with("ly"))
  past
}
