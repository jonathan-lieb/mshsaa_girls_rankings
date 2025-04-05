# Jonathan Lieb
# 3/10/25

# This function replaces augment()

# score_mod <- read_rds("Models/lm_reg.rds")
# opp_score_mod <- read_rds("Models/lm_opp_reg.rds")
# class_mod <- read_rds("Models/class_glm_model.rds")
# 
# score_mod$fit$fit$fit
# # Coefficients:
# # (Intercept)         h_a_n           rpi       rpi_opp          crpi      crpi_opp           ppg  
# # -22.93227       0.99004      10.20321      -6.96414      13.74338     -13.54202       0.90532  
# # papg_opp        o_rate    d_rate_opp       DWE_opp      avg_pace  avg_pace_opp       OWE_opp  
# # -0.22754      -0.20509      -0.57693      -0.06009      -0.90800       1.38641      -0.12986  
# # papg           OWE       ppg_opp           DWE        d_rate    o_rate_opp  
# # 0.51905       0.41851      -0.34074      -0.11922       0.24526       0.18772
# opp_score_mod$fit$fit$fit
# Coefficients:
# (Intercept)         h_a_n           rpi       rpi_opp          crpi      crpi_opp           ppg  
# -22.93227      -0.99004      -6.96414      10.20321     -13.54202      13.74338      -0.34074  
# papg_opp        o_rate    d_rate_opp       DWE_opp      avg_pace  avg_pace_opp       OWE_opp  
# 0.51905       0.18772       0.24526      -0.11922       1.38641      -0.90800       0.41851  
# papg           OWE       ppg_opp           DWE        d_rate    o_rate_opp  
# -0.22754      -0.12986       0.90532      -0.06009      -0.57693      -0.20509  

predict_score <- function(df, both = T){
  df |> 
    add_ternary() |> 
    weight_history() |> 
    mutate(pred_score = -22.93227 + 
             0.99004 * h_a_n + 
             10.20321 * rpi - 
             6.96414 * rpi_opp +
             13.74338 * crpi -
             13.54202 * crpi_opp +
             0.90532 * ppg -
             0.22754 * papg_opp -
             0.20509 * o_rate -
             0.57693 * d_rate_opp -
             0.06009 * DWE_opp -
             0.90800 * avg_pace +
             1.38641 * avg_pace_opp -
             0.12986 * OWE_opp +
             0.51905 * papg +
             0.41851 * OWE -
             0.34074 * ppg_opp -
             0.11922 * DWE +
             0.24526 * d_rate +
             0.18772 * o_rate_opp,
           pred_score_opp = -22.93227 -
             .99004 * h_a_n -
             6.96414 * rpi +
             10.20321 * rpi_opp -
             13.54202 * crpi +
             13.74338 * crpi_opp -
             0.34074 * ppg +
             0.51905 * papg_opp +
             0.18772 * o_rate +
             0.24526 * d_rate_opp -
             0.11922 * DWE_opp +
             1.38641 * avg_pace -
             0.90800 * avg_pace_opp +
             0.41851 * OWE_opp -
             0.22754 * papg -
             0.12986 * OWE +
             0.90532 * ppg_opp -
             0.06009 * DWE - 
             0.57693 * d_rate -
             0.20509 * o_rate_opp) 
}
