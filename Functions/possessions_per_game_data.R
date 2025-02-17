# Jonathan Lieb
# 2/23/2024
# Possessions per game data

# possession_data <- tribble(
#   ~ score, ~opp_score, ~possessions,
#   33, 40, 49.7,
#   43, 55, 56.82,
#   56, 36, 69.99,
#   49, 29, 58.11,
#   58, 10, 46.17, 
#   57, 23, 62.7, 
#   44, 47, 66.52,
#   27, 45, 41.88, 
#   65, 71, 77.41,
#   74, 54, 65.99,
#   56, 31, 66.58, 
#   48, 58, 55.7,
#   49, 43, 54.7, 
#   48, 57, 59.7,
#   40, 65, 63.82, 
#   52, 72, 62.7,
#   44, 69, 57.11,
#   47, 70, 75.82, 
#   48, 44, 66.75, 
#   46, 42, 55.7,
#   42, 60, 60.23,
#   50, 70, 68.23,
#   62, 55, 62.11
# ) |> 
#   mutate(ppp = score / possessions,
#          opp_ppp = opp_score / possessions,
#          max_score = pmax(score, opp_score),
#          total_score = score + opp_score,
#          ppp_winner = ifelse(score > opp_score, ppp, opp_ppp), 
#          pred_poss1 = max_score / 1.1,
#          pred_poss2 = max_score * .002931 + total_score * .294960 + 31.601755)
# 
# 
# lm_mod <- lm(possessions ~ max_score + total_score, data = possession_data)
# summary(lm_mod)
# 
# rmse(possession_data, truth = possessions, estimate = pred_poss1)
# rmse(possession_data, truth = possessions, estimate = pred_poss2)
# 
# 
# poss_f <- function(fga, fta, to, or){
#   fga + fta * .47 + to - or
# }

# poss_f(36, 9, 23, 3)
