# Jonathan Lieb
# 2/23/25

# This function changes h_a_n from a factor or character variable
# to a ternary variable with -1, 0, 1.

add_ternary <- function(df){
  df |> 
    mutate(h_a_n = ifelse(is.numeric(h_a_n), 
                                     h_a_n,
                                     case_when(
      h_a_n == "h" ~ 1,
      h_a_n == "a" ~ -1,
      TRUE ~ 0
    )))
}