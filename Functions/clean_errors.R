# Jonathan Lieb
# 1/27/25

# Libraries needed
# library(tidyverse)
# library(rvest)


# Types of errors to check for

# 1. Same score between teams within one week of each other

# 2. Almost same score between two teams within 3 days of each other (scores within 5)

# 3. Forfeiture games (scores less than 10 for both)

# 4. One teams score basically nothing (score for one less than 5)

# 5. Flipped scores (One score in day or within 3 days has one team winning
# and the the other has the other team winning with the same score)

# 6. Two different scores in the same day

# 7. Score for one of the teams is equal to their schoolid.

# 8. The School is or opponent is "TBD"

# However these need to be done in the order of 8, 7, 6, 5, 4, 3, 2, 1

# This function takes a base dataframe and checks for the above errors.
# It returns a dataframe that has been cleaned of the errors.

clean_errors <- function(base_df, recent = FALSE){
  
  if(recent){
    base_df_old <- base_df |> filter(date <= today() - 7)
    base_df <- base_df |> filter(date > today() - 7)
  }
  
  correct_error7_8 <- function(df, tbd = FALSE) {
    # Parallel setup
    plan(multisession, workers = availableCores())
    df <- df |> distinct(game_id, .keep_all = TRUE)
    
    # Helper function to process each row
    process_row <- function(i) {
      school <- df$school[i]
      school_id <- df$schoolid[i]
      school_id_opp <- df$opp_schoolid[i]
      opp <- df$opp[i]
      d <- df$date[i]
      season <- df$season[i] - 1
      score <- df$score[i]
      opp_score <- df$opp_score[i]
      
      if (score == school_id | opp == "TBD"){
        if(school_id_opp == -1){
          game_1 <- scrape_team_page(school_id, season, opp, d, womens = TRUE, tbd = tbd)
          row1 <- df[i, ] |>
            mutate(score = ifelse(game_1$score[1]< 200, 
                            game_1$score[1],
                            score),
                   opp_score = ifelse(game_1$opp_score[1]<200, 
                                      game_1$opp_score[1],
                                      opp_score))
          return(row1 |> add_reverse_row())
        } else {
          game_2 <- scrape_team_page(school_id_opp, season, school, d, womens = TRUE, tbd = tbd)
          row1 <- df[i,] |> 
            mutate(score = ifelse(game_2$opp_score[1]< 200, 
                            game_2$opp_score[1],
                            score),
                   opp_score = ifelse(game_2$score[1]<200, 
                                      game_2$score[1],
                                      opp_score))
          return(row1 |> add_reverse_row())
        }
      } else {
        if(school_id == -1){
          game_2 <- scrape_team_page(school_id_opp, season, school, d, womens = TRUE, tbd = tbd)
          row1 <- df[i,] |> 
            mutate(score = ifelse(game_2$opp_score[1]< 200, 
                            game_2$opp_score[1],
                            score),
                   opp_score = ifelse(game_2$score[1]<200, 
                                      game_2$score[1],
                                      opp_score))
          return(row1 |> add_reverse_row())
        } else {
          game_1 <- scrape_team_page(school_id, season, opp, d, womens = TRUE, tbd = tbd)
          row1 <- df[i,] |> 
            mutate(score= ifelse(game_1$score[1]< 200, 
                            game_1$score[1],
                            score),
                   opp_score = ifelse(game_1$opp_score[1]<200, 
                                      game_1$opp_score[1],
                                      opp_score))
          return(row1 |> add_reverse_row())
        }
      }
    }
    
    # Use future_map_dfr for parallel processing and combine results
    corrections <- future_map_dfr(seq_len(nrow(df)), process_row, .progress = TRUE)
    
    if(nrow(corrections) == 0){
      return(df)
    }
    
    corrections <- corrections |>
      filter(score %in% 6:100 | !is.na(schoolid), opp_score %in% 6:100 | !is.na(opp_schoolid))
    
    return(corrections)
  }
  
  correct_error5_6 <- function(df) {
    # Parallel setup
    plan(multisession, workers = availableCores())
    
    df <- df |> distinct(game_id, .keep_all = TRUE)
    # Helper function to process each row
    process_row <- function(i) {
      school <- df$school[i]
      school_id <- df$schoolid[i]
      school_id_opp <- df$opp_schoolid[i]
      opp <- df$opp[i]
      d <- df$date[i]
      season <- df$season[i] - 1
      score <- df$score[i]
      opp_score <- df$opp_score[i]
      
      if (school_id == -1 & school_id_opp == -1) {
        return(NULL)
      } else if (school_id == -1){
        game_2 <- scrape_team_page(school_id_opp, season, school, d, womens = TRUE)
        row1 <- df[i,] |> 
          mutate(score = game_2$opp_score[1], opp_score = game_2$score[1])
        return(row1 |> add_reverse_row())
      } else if (school_id_opp == -1){
        game_1 <- scrape_team_page(school_id, season, opp, d, womens = TRUE)
        row1 <- df[i,] |> 
          mutate(score = game_1$score[1], opp_score = game_1$opp_score[1])
        return(row1 |> add_reverse_row())
      } else{
        game_1 <- scrape_team_page(school_id, season, opp, d, womens = TRUE)
        game_2 <- scrape_team_page(school_id_opp, season, school, d, womens = TRUE)
        
        if (nrow(game_1) == 0 | nrow(game_2) == 0) {
          return(NULL)
        } else if (nrow(game_1) == 0){
          row1 <- df[i,] |> 
            mutate(score = game_2$opp_score[1], opp_score = game_2$score[1])
          return(row1 |> add_reverse_row())
        } else if (nrow(game_2) == 0){
          row1 <- df[i,] |> 
            mutate(score = game_1$score[1], opp_score = game_1$opp_score[1])
          return(row1 |> add_reverse_row())
        } else if (game_1$score[1] > 200 | game_1$opp_score[1] > 200 |
                   school_id == game_1$score[1] | school_id_opp == game_1$opp_score[1]){
          row1 <- df[i,] |> 
            mutate(score = game_2$opp_score[1], opp_score = game_2$score[1])
          return(row1 |> add_reverse_row())
        } else if (game_2$score[1] > 200 | game_2$opp_score[1] > 200 |
                   game_2$opp_score[1] == school_id[1] | game_2$score[1] == school_id_opp[1]){
          row1 <- df[i,] |> 
            mutate(score = game_1$score[1], opp_score = game_1$opp_score[1])
          return(row1 |> add_reverse_row())
        }else{
          row1 <- df[i,] |> 
            mutate(score = game_1$score[1], opp_score = game_1$opp_score[1])
        return(row1 |> add_reverse_row())
        }
      }
    }
    # Use future_map_dfr for parallel processing and combine results
    corrections <- future_map_dfr(seq_len(nrow(df)), process_row, .progress = TRUE)
    
    return(corrections)
  }
  
  base_df <- base_df |> 
    arrange(date, game_id) |> 
    distinct(school, opp, score, opp_score, date, .keep_all = TRUE)
  
  print(count(count(base_df, game_id), n <2))
  
  # Error 8 
  data_error8 <- base_df |> 
    filter(school == "TBD" | opp == "TBD")
  
  data8_corrected <- correct_error7_8(data_error8, tbd = TRUE)
  
  data8_fixed <- base_df |> 
    bind_rows(data8_corrected)  |> 
    filter(!(school == "TBD" | opp == "TBD"))
  
  print("8 Done")
  print(count(count(data8_fixed, game_id), n <2))  
  # Error 7
  data_error7 <- data8_fixed |> 
    filter(score == schoolid | opp_score == opp_schoolid)
  
  error7_corrections <- correct_error7_8(data_error7)
  
  data7_fixed <- data8_fixed |> 
    anti_join(data_error7, by = c("school", "opp", "date")) |>
    bind_rows(error7_corrections)
  
  print("7 Done")
  print(count(count(data7_fixed, game_id), n <2))
  
  # Error 6
  data_error6 <- data7_fixed |> 
    group_by(school, opp, date) |>
    filter(row_number() > 1)
  
  error6_corrections <- correct_error5_6(data_error6)
  
  data6_fixed <- data7_fixed |> 
    anti_join(data_error6, by = c("school", "opp", "date")) |>
    bind_rows(error6_corrections)
  
  print("6 Done")
  print(count(count(data6_fixed, game_id), n <2))
  
  # Error 5
  data_error5 <- data6_fixed |> 
    group_by(school, opp, season) |> 
    mutate(day_diff = abs(as.numeric(date - lag(date)))) |> 
    filter(day_diff <= 3 & score == lag(opp_score) & opp_score == lag(score))
  
  error5_corrections <- correct_error5_6(data_error5)
  
  data5_fixed <- data6_fixed |> 
    anti_join(data_error5, by = c("school", "opp", "date")) |>
    bind_rows(error5_corrections)
  
  print("5 Done")
  print(count(count(data5_fixed, game_id), n <2))
  # Error 4
  data4_fixed <- data5_fixed |>
    filter(score >= 4 & opp_score >= 4)
  
  print(count(count(data4_fixed, game_id), n <2))
  # Error 3
  data3_fixed <- data4_fixed |>
    filter(score >= 10 | opp_score >= 10)
  print(count(count(data3_fixed, game_id), n <2))
  
  # Error 2
  data2_fixed <- data3_fixed |>
    group_by(school, opp, season) |>
    mutate(day_diff = abs(as.numeric(date - lag(date)))) |>
    filter(day_diff > 3 | (abs(score -lag(score, default = -2)) + abs(opp_score - lag(opp_score, default = -2)) > 5))
    
  print(count(count(data2_fixed, game_id), n <2))
  # Error 1
  data1_fixed <- data2_fixed |>
    distinct(school, opp, score, opp_score, wk = week(date), season, .keep_all = TRUE) |>
    select(-wk)
  
  print(count(count(data1_fixed, game_id), n <2))
  
  last_data <- data1_fixed |> 
    select(1:20) |> 
    ungroup() |> 
    mutate(w_l = factor(ifelse(score > opp_score, "W", "L"), levels = c("L", "W")))
  
  if(recent)(return(bind_rows(base_df_old, last_data) |> ungroup()))else(return(last_data))
}

# Example usage
# data <- read_parquet("C:/Users/Jonat/OneDrive/Documents/Sports Stats Analytics/MSHSAA Boys Basketball/mshsaa_app/Data/base.parquet")
# data_cleaned <- clean_errors(data)


# library(arrow)
# library(tidyverse)
# library(rvest)
# data <- read_parquet("C:/Users/Jonat/OneDrive/Documents/Sports Stats Analytics/MSHSAA Boys Basketball/mshsaa_app/Data/base.parquet")
# 
# # 1
# data_error1 <- data |> 
#   janitor::get_dupes(school, opp, score, opp_score, season)
# 
# data1_fixed <- data |> 
#   distinct(school, opp, score, opp_score, wk = week(date), season, .keep_all = TRUE) |> 
#   select(-wk)
# 
# # 2
# data_error2 <- data1_fixed |> 
#   group_by(school, opp, season) |> 
#   mutate(day_diff = abs(as.numeric(date - lag(date)))) |> 
#   filter(day_diff <= 3 & (abs(score -lag(score)) + abs(opp_score - lag(opp_score)) <= 5))
# 
# data2_fixed <- data1_fixed |> 
#   group_by(school, opp, season) |>
#   mutate(day_diff = abs(as.numeric(date - lag(date)))) |>
#   filter(day_diff > 3 | (abs(score -lag(score, 0)) + abs(opp_score - lag(opp_score, default = 0)) > 5))
# 
# # 3 
# data_error3 <- data2_fixed |> 
#   filter(score < 10 & opp_score < 10)
# 
# data3_fixed <- data2_fixed |>
#   filter(score >= 10 & opp_score >= 10)
# 
# # 4 
# data_error4 <- data3_fixed |> 
#   filter(score < 5 | opp_score < 5)
# 
# data4_fixed <- data3_fixed |>
#   filter(score >= 5 & opp_score >= 5)
# 
# # 5
# data_error5 <- data4_fixed |> 
#   group_by(school, opp, season) |> 
#   mutate(day_diff = abs(as.numeric(date - lag(date)))) |> 
#   filter(day_diff <= 3 & score == lag(opp_score) & opp_score == lag(score))
# 
# library(furrr)
# library(parallel)
# library(rvest)
# 
# # Function to correct errors
# 
# 
# error5_corrections <- correct_error5_6(data_error5)
# 
# data5_fixed <- data4_fixed |> 
#   anti_join(data_error5, by = c("school", "opp", "date")) |>
#   bind_rows(error5_corrections)
# 
# # 6
# data_error6 <- data5_fixed |> 
#   group_by(school, opp, date) |>
#   filter(row_number() > 1)
# 
# error6_corrections <- correct_error5_6(data_error6)
# 
# data6_fixed <- data5_fixed |> 
#   anti_join(data_error6, by = c("school", "opp", "date")) |>
#   bind_rows(error6_corrections)
# 
# 
# # 7
# data_error7 <- data6_fixed |> 
#   filter(score == schoolid | opp_score == opp_schoolid)
# 
# # Function to correct error 7
# 
# 
# 
# 
# # Example usage
# error7_corrections <- correct_error7_8(data_error7)
# 
# 
# data7_fixed <- data6_fixed |> 
#   anti_join(data_error7, by = c("school", "opp", "date")) |>
#   bind_rows(error7_corrections)
# 
# # 8
# data_error8 <- data7_fixed |> 
#   filter(school == "TBD" | opp == "TBD")
# 
# data8_corrected <- correct_error7_8(data_error8)
# 
# data8_fixed <- data7_fixed |> 
#   bind_rows(data8_corrected)  |> 
#   filter(!(school == "TBD" | opp == "TBD"))
# 
