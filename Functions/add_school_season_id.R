# Jonathan Lieb
# 2/13/25

# This function adds a new column that is the team's unique combination of 
# school and season.

add_school_season_id <- function(df){
  df |> 
    mutate(school_season_id = paste0(school, "_", season),
           school_season_id_opp = paste0(opp, "_", season)) 
}
