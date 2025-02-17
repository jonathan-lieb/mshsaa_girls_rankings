# Jonathan Lieb
# 2/13/2025

# This function is used to recalculate values of my modeling data 
# using a combination of historical data and the current data.

weight_history <- function(df){
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
      # ppg_ly = .28 * 12.474 + .54 * ppg_ly + .18 * ppg_3y,
      #      ppg_ly_opp = .28 * 12.474 + .54 * ppg_ly_opp + .18 * ppg_3y_opp,
      #      rpi_ly = .13 + .70 * rpi_ly + .17 * rpi_3y,
      #      rpi_ly_opp = .13 + .70 * rpi_ly_opp + .17 * rpi_3y_opp,
      #      crpi_ly = .54 + .29 * crpi_ly + .17 * crpi_3y,
      #      crpi_ly_opp = .54 + .29 * crpi_ly_opp + .17 * crpi_3y_opp,
           ppg = ppg*(1-(1/2)^g) + ppg_ly * (.5)^g,
           papg = papg*(1-(1/2)^g) + papg_ly * (.5)^g,
           crpi = crpi*(1-(1/2)^g) + crpi_ly * (.5)^g,
           rpi = rpi*(1-(1/2)^g) + rpi_ly * (.5)^g,
           rpi_opp = rpi_opp*(1-(1/2)^g_opp) + rpi_ly_opp * (.5)^g_opp,
           papg_opp = papg_opp*(1-(1/2)^g_opp) + papg_ly_opp * (.5)^g_opp,
           crpi_opp = crpi_opp*(1-(1/2)^g_opp) + crpi_ly_opp * (.5)^g_opp,
           ppg_opp = ppg_opp*(1-(1/2)^g_opp) + ppg_ly_opp * (.5)^g_opp,
           OWE = OWE*(1-(1/2)^g) + OWE_ly * (.5)^g,
           DWE = DWE*(1-(1/2)^g) + DWE_ly * (.5)^g,
           DWE_opp = DWE_opp*(1-(1/2)^g_opp) + DWE_ly_opp * (.5)^g_opp,
           OWE_opp = OWE_opp*(1-(1/2)^g_opp) + OWE_ly_opp * (.5)^g_opp,
           o_rate = o_rate*(1-(1/2)^g) + o_rate_ly * (.5)^g,
           d_rate = d_rate*(1-(1/2)^g) + d_rate_ly * (.5)^g,
           o_rate_opp = o_rate_opp*(1-(1/2)^g_opp) + o_rate_ly_opp * (.5)^g_opp,
           d_rate_opp = d_rate_opp*(1-(1/2)^g_opp) + d_rate_ly_opp * (.5)^g_opp,
           avg_pace = avg_pace*(1-(1/2)^g) + avg_pace_ly * (.5)^g,
           avg_pace_opp = avg_pace_opp*(1-(1/2)^g_opp) + avg_pace_ly_opp * (.5)^g_opp,
    )
}
