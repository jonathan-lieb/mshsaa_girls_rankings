# Jonathan Lieb
# 2/28/25

sim_single_game <- function(base, #score_mod, opp_score_mod, class_mod, 
                            t, o, s, loc = "n"){
  game_data <- create_modeling_data_sims(base, t, o, s, loc)
  preds <- suppressMessages(game_data[[1]] |> 
    predict_score() |> 
    predict_class() |> 
    select(school, opp, pred_class, pred_score, pred_score_opp, pred_win, pred_lose))
  
  full_data <- suppressMessages(preds |> 
    left_join(game_data[[2]] |> 
                select(school, district, wins, losses, ppg, papg, rpi, crpi, season),
              by = "school") |> 
    left_join(game_data[[2]] |> 
                select(school, district, wins, losses, ppg, papg, rpi, crpi, season),
              by = c("opp" = "school"), suffix = c("", "_opp")) |> 
    mutate(across(everything(), ~ replace_na(.x, 0))))
  
  full_data
    
}

# Example usage:
# g = sim_single_game(base, score_mod, opp_score_mod, class_mod, "Calvary Lutheran", "Jamestown", 2025, "n")
# 
# t = c("Calvary Lutheran", "Jamestown", "Smithton", "South Callaway")
# o = c("St. Elizabeth", "Macks Creek", "Tuscumbia", "North Callaway")
# loc = c("n", "a", "n", "h")
# g_all = sim_single_game(base, score_mod, opp_score_mod, class_mod, t, o, 2025, loc)
# g_all
