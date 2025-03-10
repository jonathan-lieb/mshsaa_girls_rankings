create_modeling_data_new <- function(old_modeling_data, new_base_df, 
                                     new_days){
  old_modeling_data <- old_modeling_data |> 
    filter(date < min(new_days))
  min_year <- if(month(min(new_days)) > 9)(year(min(new_days))+1)else(year(min(new_days)))
  max_year <- if(month(max(new_days)) > 9)(year(max(new_days))+1)else(year(max(new_days)))
  seasons <- min_year:max_year
  combined_df <- new_base_df |> 
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
        left_join(day_team_results, by = c("school" = "school", "season")) |> 
        left_join(day_team_results, by = c("opp" = "school", "season"), suffix = c("", "_opp")) |>
        left_join(past, by = c("school" = "school", "season")) |>
        left_join(past, by = c("opp" = "school", "season"), suffix = c("", "_opp"))  #|> 

      day_full_matchups
    })
    day_results
  }, .progress = TRUE)
  bind_rows(modeling_data, old_modeling_data) |> 
    arrange(desc(date)) |> 
    group_by(school, opp, season) |>
    mutate(day_diff = abs(as.numeric(date - lag(date)))) |>
    filter(day_diff > 3 | (abs(score -lag(score, default = -2)) + abs(opp_score - lag(opp_score, default = -2)) > 5)) |> 
    ungroup() |> 
    select(-day_diff) |>
    distinct(school, opp, score, opp_score, wk = week(date), season, .keep_all = TRUE) |>
    select(-wk)
}


# Example
# base <- read_parquet("Data/cleaned.parquet")
# new <- base |> 
#   filter(date == max(date))
# new_modeling <- create_modeling_data_new(base, new)

