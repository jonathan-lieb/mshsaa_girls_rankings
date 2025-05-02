# Jonathan Lieb
# 3/11/25

# Prediction accuracy for secon half of 2025 season
modeling <- read_parquet("Data/modeling.parquet")
modeling_2025 <- modeling %>%
  filter(date > mdy("1-27-2025"))
glimpse(modeling_2025)
modeling_2025 |> 
  predict_score() |> 
  predict_class() |> 
  mutate(outcome = ifelse(score > opp_score, "win", "lose")) |> 
  summarise(accuracy = mean(pred_class == outcome), 
            mae = mean(abs(pred_score - score)))
