# Jonathan Lieb
# 2/11/24

# This script requires the following packages:
# library(tidyverse)

add_groupings <- function(df){
  df |> 
    mutate(grouping = case_when(
      g == 0 & g_opp == 0 ~ "0-0",
      g == 0 & g_opp == 1 ~ "0-1",
      g == 0 & g_opp %in% 2:4 ~ "0-2to4",
      g == 0 & g_opp %in% 5:9 ~ "0-5to9",
      g == 0 & g_opp >= 10 ~ "0-10plus",
      g == 1 & g_opp == 0 ~ "1-0",
      g == 1 & g_opp == 1 ~ "1-1",
      g == 1 & g_opp %in% 2:4 ~ "1-2to4",
      g == 1 & g_opp %in% 5:9 ~ "1-5to9",
      g == 1 & g_opp >= 10 ~ "1-10plus",
      g %in% 2:4 & g_opp == 0 ~ "2to4-0",
      g %in% 2:4 & g_opp == 1 ~ "2to4-1",
      g %in% 2:4 & g_opp %in% 2:4 ~ "2to4-2to4",
      g %in% 2:4 & g_opp %in% 5:9 ~ "2to4-5to9",
      g %in% 2:4 & g_opp >= 10 ~ "2to4-10plus",
      g %in% 5:9 & g_opp == 0 ~ "5to9-0",
      g %in% 5:9 & g_opp == 1 ~ "5to9-1",
      g %in% 5:9 & g_opp %in% 2:4 ~ "5to9-2to4",
      g %in% 5:9 & g_opp %in% 5:9 ~ "5to9-5to9",
      g %in% 5:9 & g_opp >= 10 ~ "5to9-10plus",
      g >= 10 & g_opp == 0 ~ "10plus-0",
      g >= 10 & g_opp == 1 ~ "10plus-1",
      g >= 10 & g_opp %in% 2:4 ~ "10plus-2to4",
      g >= 10 & g_opp %in% 5:9 ~ "10plus-5to9",
      g %in% 10:19 & g_opp %in% 10:19 ~ "10to19-10to19",
      g %in% 10:19 & g_opp >= 20 ~ "10to19-20plus",
      g >= 20 & g_opp %in% 10:19 ~ "20plus-10to19",
      g >= 20 & g_opp >= 20 ~ "20plus-20plus",
      T ~ NA
    ))
}

