# Jonathan Lieb
# 2/1/25

#This function creates a new row of a dataframe that is reverse of the original row

reverse_row <- function(df_row){
  df_row <- select(df_row, 1:20)
  colnames(df_row) <- c("opp", "opp_score", "opp_class_and_district", 
                        "opp_class", "opp_district", "school", "score",
                        "class_and_district", "class", "district",
                        "result", "tourney", "location", "date",
                        "opp_schoolid", "schoolid", "game_id", "season",
                        "w_l", "h_a_n")
  df_row |> 
    mutate(w_l = factor(ifelse(w_l == "W", "L", "W"),levels = c("L", "W")),
           h_a_n = factor(case_when(
             h_a_n == "h" ~ "a",
             h_a_n == "a" ~ "h",
             TRUE ~ "n"
           ), levels = c("a", "n", "h")))
}

add_reverse_row <- function(df_row){
  df_row |>
    bind_rows(reverse_row(df_row))
}
