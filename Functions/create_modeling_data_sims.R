# Jonathan Lieb
# 2/28/25

create_modeling_data_sims <- function(base_df, t, o, s, loc = "n"){
  grid <- tibble(school = t, opp = o, h_a_n = loc)
  
  base_df <- base_df |> 
    filter(season >= s-1) |> 
    mutate(poss_est = pmax(score, opp_score) * .292960 + (score + opp_score) * .002931 + 31.601755,
           ppp = score / poss_est,
           ppp_opp = opp_score / poss_est)
  
  past <- team_history(base_df, s) |> 
    filter(school %in% c(t, o))
  
  season_df <- base_df |> 
    filter(season == s)
  
  day_team_results <- team_stats(season_df) |> 
    filter(school %in% c(t, o)) |>
    distinct(school, .keep_all = TRUE)
  
  day_full_matchups <- grid |> 
    left_join(day_team_results, by = c("school" = "school")) |> 
    left_join(day_team_results, by = c("opp" = "school"), suffix = c("", "_opp")) |>
    left_join(past, by = c("school" = "school")) |>
    left_join(past, by = c("opp" = "school"), suffix = c("", "_opp"))
  
  modeling_data <- day_full_matchups |> 
    weight_history() |> 
    add_ternary()
  
  return(list(modeling_data = modeling_data, stats = day_team_results))
}
