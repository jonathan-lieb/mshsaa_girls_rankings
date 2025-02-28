# Jonathan Lieb
# 2/13/2025

# This function is used to recalculate values of my modeling data 
# using a combination of historical data and the current data.

weight_history <- function(df){
  la_crpi <- .5
  la_rpi <- .5
  la_o_rate <- 0
  la_avg_pace <- 48
  la_ppg <- 45
  
  law <- .2
  psw <- .8
  c <- .8
  
  df |> 
    mutate(across(all_of(c("g", "g_opp", "ppg", "papg_opp",
                           "papg", "ppg_opp", 
                           "crpi", "crpi_opp", "rpi", "rpi_opp", 
                           "o_rate", "d_rate", "o_rate_opp", "d_rate_opp",
                           "OWE", "DWE_opp", "avg_pace",
                           "DWE", "OWE_opp", 
                           "o_rate_ly", "d_rate_ly", "o_rate_ly_opp", "d_rate_ly_opp",
                           "avg_pace_opp")), ~replace(., is.na(.), 0))) |> 
    mutate(across(all_of(c("ppg_ly", "papg_ly", "papg_ly_opp", "ppg_ly_opp",
                           "OWE_ly", "DWE_ly", "OWE_ly_opp", "DWE_ly_opp"
    )), 
    ~replace(., is.na(.), 45))) |>
    mutate(across(all_of(c("rpi_ly", "rpi_ly_opp", "crpi_ly", "crpi_ly_opp")), 
                  ~replace(., is.na(.), .5))) |>
    mutate(across(all_of(c("avg_pace_ly", "avg_pace_ly_opp")), 
                  ~replace(., is.na(.), 48))) |>
    mutate(
      lawg = law * (c)^g,
      lawg_opp = law * (c)^g_opp,
      pswg = psw * (c)^g,
      pswg_opp = psw * (c)^g_opp,
      cswg = 1 - lawg - pswg,
      cswg_opp = 1 - lawg_opp - pswg_opp,
      ppg = ppg*cswg + ppg_ly * pswg +  la_ppg * lawg,
      papg = papg*cswg + papg_ly * pswg + la_ppg * lawg,
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
    ) |> 
    select(-lawg, -lawg_opp, -pswg, -pswg_opp, -cswg, -cswg_opp)
}
