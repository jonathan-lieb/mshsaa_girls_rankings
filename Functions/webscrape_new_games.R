# Jonathan Lieb
# 1/25/25

# Libraries
# tidyverse
# furrr
# rvest

# This function scrapes the MSHSAA girls basketball scoreboard for 
# a given date range. It returns a tibble of the games. It can 
# be used to scrape both prior and future games. 
scrape_days <- function(date_vector, finished = F){
  plan(multisession, workers = availableCores())
  
  dv <- as.character(date_vector) |> 
    str_remove_all("-")
  
  dv1 <- str_c(str_sub(dv, 5), str_sub(dv, 1, 4))
  
  extract_second_to_last <- function(input_str) {
    elements <- strsplit(input_str, ",")[[1]]
    result <- trimws(elements[length(elements) - 1])
  }
  
  extract_third_to_last <- function(input_str) {
    elements <- strsplit(input_str, ",")[[1]]
    result <- trimws(elements[length(elements) - 2])
  }
  extract_fourth_to_last <- function(input_str) {
    elements <- strsplit(input_str, ",")[[1]]
    result <- trimws(elements[length(elements) - 3])
  }
  check_empty <- function(a, b){
    ifelse(a == "" & b != "", b, a)
  }
  data_combined <- future_map_dfr(seq_along(dv1), function(i) {
    page=read_html(paste0('https://www.mshsaa.org/Activities/Scoreboard.aspx?alg=6&date=',dv1[[i]]))
    game_info=html_nodes(page, ".gamedetails td") %>% html_text() |> 
      str_remove_all(",") |> 
      str_replace_all("\\n", ",") |> 
      str_squish()
    location <- map_chr(game_info, extract_second_to_last)
    tourney <- map_chr(game_info, extract_third_to_last)
    tourney2 <- map_chr(game_info, extract_fourth_to_last)
    tourney <- map2_chr(tourney, tourney2, check_empty)
    outcome=html_nodes(page, ".outcome") %>% html_text()
    game_info2=html_nodes(page, ".tournamentName") %>% html_text()
    scores=as.numeric(html_nodes(page, ".score") %>% html_text())
    schools=html_nodes(page, ".name") %>% html_text()
    schools= ifelse(str_count(schools, "\\(") > str_count(schools, "\\)"), str_c(schools, ")"), schools)
    classification=html_nodes(page, ".classification") %>% html_text()
    classes=as.numeric(gsub(".*?([0-9]+).*", "\\1", classification))
    districts=as.numeric(gsub('.*\\b(\\d+)\\b.*$', '\\1', classification))
    schoolids= as.numeric(html_nodes(page, "tr") |> html_attr("data-school")) |> purrr::discard(is.na)
    data=tibble(school=schools[c(T, F)], score=scores[c(T, F)], class_and_district=classification[c(T, F)],
                class=classes[c(T, F)], district=districts[c(T, F)],
                opp=schools[c(F, T)], opp_score=scores[c(F, T)], 
                opp_class_and_district=classification[c(F, T)],
                opp_class= classes[c(F, T)], opp_district=districts[c(F, T)],
                result=outcome, tourney=tourney, location=location, 
                date = date_vector[[i]],
                schoolid = schoolids[c(T, F)], opp_schoolid = schoolids[c(F, T)]
                )
    new_data <- data |> 
      mutate(game_id = paste(row_number(), "-", dv1[[i]]),
             season = ifelse(month(date) < 8, year(date), year(date) + 1))
    new_col_names <- c("opp", "opp_score", "opp_class_and_district", "opp_class",
                       "opp_district", "school", "score", "class_and_district", 
                       "class", "district", "result", "tourney", "location", "date", 
                       "opp_schoolid", "schoolid",
                       "game_id", "season")
    new_data2 <- new_data
    names(new_data2) <- new_col_names
    combined_df <- bind_rows(new_data, new_data2) |> 
      mutate(score = as.numeric(score),
             opp_score = as.numeric(opp_score),
             school = str_squish(school),
             opp = str_squish(opp),
             w_l = factor(ifelse(score > opp_score, "W", "L"),
                          levels = c("L", "W"))) |> 
      add_home_away()
    
    if(finished == T){
      combined_df <- combined_df |> 
        filter(str_detect(result, "Final"))
    }
    
    combined_df
  }, .progress = TRUE)
  data_combined
}
