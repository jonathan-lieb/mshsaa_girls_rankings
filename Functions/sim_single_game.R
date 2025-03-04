# Jonathan Lieb
# 2/28/25

sim_single_game <- function(base, score_mod, opp_score_mod, class_mod, t, o, s, loc = "n"){
  game_data <- create_modeling_data_sims(base, t, o, s, loc)
  preds <- suppressMessages(game_data[[1]] |> 
    augment(class_mod, new_data = _) |>
    augment(class_mod, new_data = _, type = "prob") |>
    augment(score_mod, new_data = _) |>
    augment(opp_score_mod, new_data = _) |>
    rename(pred_score = `.pred...2`,
           pred_score_opp = `.pred...1`,
           .pred_class = `.pred_class...3`,
           .pred_l = `.pred_l...4`,
           .pred_w = `.pred_w...5`) |> 
    select(school, opp, .pred_class, pred_score, pred_score_opp, .pred_w, .pred_l))
  
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
