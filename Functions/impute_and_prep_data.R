# Jonathan Lieb
# 2/6/2025
# This function preps a data set before it is used in my models.

impute_and_prep_data <- function(data, groups = F){
  selected_variables <- c("school", "opp", "date", "score", "g", "g_opp", "ppg", "papg_opp",
                            "crpi", "crpi_opp", "rpi", "rpi_opp", "pace_control",
                            "pace_control_opp", "OWE", "DWE_opp", "avg_pace",
                            "avg_pace_opp", "h_a_n", "game_id")
  if (groups == T){
    selected_variables <- c(selected_variables, "grouping")
  }
  data |> 
    mutate(
      g = ifelse(is.na(g), 0, g),
      g_opp = ifelse(is.na(g_opp), 0, g_opp),
      ppg_ly = ifelse(is.na(ppg_ly), 45.23, ppg_ly),
      papg_ly_opp = ifelse(is.na(papg_ly_opp), 45.23, papg_ly_opp),
      ppg = ifelse(is.na(ppg), ppg_ly, ppg),
      papg_opp = ifelse(is.na(papg_opp), papg_ly_opp, papg_opp),
      crpi = case_when(!is.na(crpi) ~ crpi,
                       !is.na(crpi_ly) ~ crpi_ly,
                       !is.na(crpi_opp) ~ crpi_opp,
                       T ~ .49),
      crpi_opp = case_when(!is.na(crpi_opp) ~ crpi_opp,
                           !is.na(crpi_ly_opp) ~ crpi_ly_opp,
                           !is.na(crpi) ~ crpi,
                           T ~ .49),
      rpi = ifelse(is.na(rpi), .5, rpi),
      rpi_opp = ifelse(is.na(rpi_opp), .5, rpi_opp),
      pace_control = ifelse(is.na(pace_control), .5, pace_control),
      pace_control_opp = ifelse(is.na(pace_control_opp), .5, pace_control_opp),
      OWE = ifelse(is.na(OWE), 45.23, OWE),
      DWE_opp = ifelse(is.na(DWE_opp), 45.23, DWE_opp),
      # eff_margin = ifelse(is.na(eff_margin), 0, eff_margin),
      # eff_margin_opp = ifelse(is.na(eff_margin_opp), 0, eff_margin_opp),
      # pts_margin = ifelse(is.na(pts_margin), 0, pts_margin),
      # pts_margin_opp = ifelse(is.na(pts_margin_opp), 0, pts_margin_opp),
      avg_pace = ifelse(is.na(avg_pace), 55.15, avg_pace),
      avg_pace_opp = ifelse(is.na(avg_pace_opp), 55.15, avg_pace_opp),
      # wp = ifelse(is.na(wp), .5, wp),
      # wp_opp = ifelse(is.na(wp_opp), .5, wp_opp)
    ) |> 
    select(selected_variables)
}

