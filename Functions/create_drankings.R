# Jonathan Lieb
# 2/24/25

# This script makes ranking frames for the most recent data in the database
make_drankings <- function(df, past_drankings = tibble()){
  max_date <- max(df$date) + 1
  
  score_mod <- read_rds("Models/lm_reg.rds")
  opp_score_mod <- read_rds("Models/lm_opp_reg.rds")
  class_mod <- read_rds("Models/class_glm_model.rds")
  
  s <- max(df$season)
  
  classes <- df |> 
    filter(season == s, (!is.na(class)|class == -1)) |>
    pull(class) |>
    unique() |> 
    sort()
  
  class_drankings <- vector("list", length(classes))
  for (i in 1:length(classes)){
    teams <- df |>
      filter(class == classes[i] & season == s) |>
      pull(school) |>
      unique()
    
    ranking_frames <- df |>
      create_modeling_data_rankings(teams, s, "n")
    
    ranking_preds <- ranking_frames[[1]] |>
      weight_history() |>
      add_ternary() |>
      augment(class_mod, new_data = _) |>
      augment(score_mod, new_data = _) |>
      augment(opp_score_mod, new_data = _) |>
      rename(pred_score = `.pred...2`,
             pred_score_opp = `.pred...1`)
    
    rankings <- ranking_preds |>
      create_rankings() |> 
      mutate(rank = row_number()) |> 
      left_join(ranking_frames[[2]] |> 
                  select(school, district, wins, losses, ppg, papg, rpi, crpi, season),
                by = "school") |> 
      mutate(class = classes[i]) |> 
      mutate(across(everything(), ~ replace_na(.x, 0)))
    
    class_drankings[[i]] <- rankings
  }
  
  dranks <- bind_rows(class_drankings) |>
    mutate(date = max_date)
  
  if(nrow(past_drankings) > 0){
    dranks <- bind_rows(dranks, past_drankings |> filter(date != max_date)) |> 
      arrange(desc(date), class, rank)
  }
  
  dranks
}

# Example
# library(arrow)
# library(tidyverse)
# library(tidymodels)
# miceadds::source.all("Functions/")
# base <- read_parquet("Data/cleaned.parquet")
# 
# drankings <- make_drankings(base)
# glimpse(drankings)
# write_parquet(drankings, "Data/drankings.parquet")
