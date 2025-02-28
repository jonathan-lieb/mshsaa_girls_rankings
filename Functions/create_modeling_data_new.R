create_modeling_data_new <- function(base_data, new_data){
  seasons <- unique(new_data$season)
  new_data_days <- new_data |> 
    pull(date) |> 
    unique()
  base_data <- base_data |> 
    filter(!date %in% new_data_days)
  combined_df <- bind_rows(base_data, new_data)
  combined_df <- combined_df |> 
    mutate(poss_est = pmax(score, opp_score) * .292960 + (score + opp_score) * .002931 + 31.601755,
           ppp = score / poss_est,
           ppp_opp = opp_score / poss_est)
  
  modeling_data <- future_map_dfr(seasons, function(s){
    past <- team_history(combined_df, s)
    
    season_df <- combined_df |> 
      filter(season == s)
    
    days <- new_data |> 
      filter(season == s) |> 
      pull(date) |> 
      unique()
    
    day_results <- future_map_dfr(days, function(d){
      day_df <- season_df |> 
        filter(date < d)
      
      day_team_results <- team_stats(day_df) |> 
        distinct(school, .keep_all = TRUE)
      
      day_matchups <- season_df |> 
        select(score, opp_score, school, class, opp, opp_class, h_a_n, date, season, game_id) |> 
        filter(date == d)
      
      day_full_matchups <- day_matchups |> 
        left_join(day_team_results, by = c("school" = "school")) |> 
        left_join(day_team_results, by = c("opp" = "school"), suffix = c("", "_opp")) |>
        left_join(past, by = c("school" = "school")) |>
        left_join(past, by = c("opp" = "school"), suffix = c("", "_opp")) #|> 

      day_full_matchups
    })
    day_results
  }, .progress = TRUE)
  modeling_data
}


# Example
# base <- read_parquet("Data/cleaned.parquet")
# new <- base |> 
#   filter(date == max(date))
# new_modeling <- create_modeling_data_new(base, new)

