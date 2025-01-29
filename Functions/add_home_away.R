# Jonathan Lieb
# 1/25/25

# Library needed
# library(tidyverse)

# This function changes the h_a_n column to a factor with levels a, n, h. 
# Additionally it checks if a tournament is being played at a home school 
# for one of the teams and changes the h_a_n column to h for that team.
add_home_away <- function(df){
  df %>%
    group_by(game_id) |>
    mutate(h_a_n = case_when(row_number() == 1 & (tourney == "" |
                                                    str_detect(tourney, opp)) ~ "a",
                             row_number() == 2 & (tourney == "" |
                                                    str_detect(tourney, school))  ~ "h",
                             T ~ "n")) |>
    ungroup() |>
    mutate(h_a_n = factor(h_a_n, levels = c("a", "n", "h")))
}