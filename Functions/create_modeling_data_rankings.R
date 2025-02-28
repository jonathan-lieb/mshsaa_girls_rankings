create_modeling_data_rankings <- function(base_df, teams_vector, s, loc = "n"){
  grid <- as_tibble(t(combn(teams_vector, 2))) |> 
    rename(school = V1, opp = V2)
  
  base_df <- base_df |> 
    filter(season >= s-1) |> 
    mutate(poss_est = pmax(score, opp_score) * .292960 + (score + opp_score) * .002931 + 31.601755,
           ppp = score / poss_est,
           ppp_opp = opp_score / poss_est)
  
  past <- team_history(base_df, s)
    
  season_df <- base_df |> 
    filter(season == s)
      
  day_team_results <- team_stats(season_df) |> 
    distinct(school, .keep_all = TRUE) |> 
    filter(school %in% teams_vector)
      
  day_full_matchups <- grid |> 
      left_join(day_team_results, by = c("school" = "school")) |> 
      left_join(day_team_results, by = c("opp" = "school"), suffix = c("", "_opp")) |>
      left_join(past, by = c("school" = "school")) |>
      left_join(past, by = c("opp" = "school"), suffix = c("", "_opp"))

  modeling_data <- day_full_matchups |> 
    mutate(h_a_n = factor(loc, levels = c("a", "n", "h")))
  
  return(list(modeling_data = modeling_data, stats = day_team_results))
}

# Example
# library(arrow)
# library(tidyverse)
# library(tidymodels)
# miceadds::source.all("Functions/")
# base <- read_parquet("Data/cleaned.parquet")
# teams <- base |>
#   filter(class == 1 & season == 2025) |>
#   pull(school) |>
#   unique()
# ranking_frames <- base |>
#   create_modeling_data_rankings(teams, 2025, "n")
# glimpse(ranking_frames)
# score_mod <- read_rds("Models/lm_reg.rds")
# opp_score_mod <- read_rds("Models/lm_opp_reg.rds")
# class_mod <- read_rds("Models/class_glm_model.rds")
# ranking_preds <- ranking_frames |>
#   weight_history() |> 
#   add_ternary() |> 
#   augment(class_mod, new_data = _) |> 
#   augment(score_mod, new_data = _) |>
#   augment(opp_score_mod, new_data = _) |> 
#   rename(pred_score = `.pred...2`,
#          pred_score_opp = `.pred...1`)
# 
# rankings <- ranking_preds |> 
#   create_rankings()
# 
# glimpse(rankings)
