# Jonathan Lieb
# 3/10/25

# This function is a replacement for augment()


# Coefficients:
# (Intercept)         h_a_n           ppg          papg           OWE           DWE           rpi  
# -0.0005051     0.2446058     0.1303700     0.0794696     0.0463138     0.0059175     2.7417908  
# crpi        o_rate        d_rate       ppg_opp      papg_opp       OWE_opp       DWE_opp  
# 3.3514973    -0.0271013     0.1007637    -0.1303736    -0.0794365    -0.0463049    -0.0059270  
# rpi_opp      crpi_opp    o_rate_opp    d_rate_opp      avg_pace  avg_pace_opp  
# -2.7413040    -3.3519798     0.0270887    -0.1007383    -0.2555660     0.2555479  

# Define a prediction function for logistic regression
predict_class <- function(data) {
  coef_vec <- setNames(
    c(-0.0005051, 0.2446058, 0.1303700, 0.0794696, 0.0463138,
      0.0059175, 2.7417908, 3.3514973, -0.0271013, 0.1007637,
      -0.1303736, -0.0794365, -0.0463049, -0.0059270, -2.7413040,
      -3.3519798, 0.0270887, -0.1007383, -0.2555660, 0.2555479),
    c("(Intercept)", "h_a_n", "ppg", "papg", "OWE", "DWE", "rpi",
      "crpi", "o_rate", "d_rate", "ppg_opp", "papg_opp", "OWE_opp",
      "DWE_opp", "rpi_opp", "crpi_opp", "o_rate_opp", "d_rate_opp",
      "avg_pace", "avg_pace_opp"
    ))

  feature_matrix <- data %>%
    mutate(
      `(Intercept)` = 1
    ) |> 
    select(all_of(names(coef_vec))) %>%
    mutate(across(everything(), as.numeric)) %>%
    as.matrix()
  
  # Compute logits
  logit_values <- as.numeric(feature_matrix %*% coef_vec)
  
  # Add predictions while keeping all columns
  b <- data %>%
    mutate(
      logit = logit_values,
      pred_win = 1 / (1 + exp(-logit)),  # Probability of win
      pred_lose = 1 - pred_win,         # Probability of lose
      pred_class = ifelse(pred_win > 0.5, "win", "lose")  # Predicted class
    ) |> 
    select(-logit)
  
}

# Example usage
# data <- old_modeling[1:2,] |> 
#                    add_ternary() |> 
#                    weight_history()


