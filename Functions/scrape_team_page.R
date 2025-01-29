# Jonathan Lieb
# 1/26/25

# Packages needed
# library(tidyverse)
# library(rvest)

# This function is designed to scrape a single team's game
scrape_team_page <- function(id, season, opp = T, d = T, womens = T, tbd = F){
  if (opp != T){
    opp <- str_squish(str_remove_all(opp, "at|[^\\w\\s]|\\d"))
  }
  if (womens)(value <- 6) else (value <- 5)
  url <- read_html(paste0("https://www.mshsaa.org/MySchool/Schedule.aspx?s=", id, "&alg=", value, "&year=", season))
  tab <- html_table(url)
  if (length(tab) < 2){
    return(tibble(score = NA, opp_score = NA, date = NA))
  }
  
  table3 <- ifelse(nrow(tab[[2]])<5, T, F)
  tab_new <- url |> 
    html_nodes("table")
  if (table3){
    tab_new <- tab_new[[3]] |>
      html_nodes("tr")
  }else{
    tab_new <- tab_new[[2]] |>
      html_nodes("tr")
  }
  
  varsity <- tab_new %>%
    .[grepl('data-level="1"', as.character(.))] |> 
    map(~{
      cells <- .x %>%
        html_nodes("td") %>%
        html_text(trim = TRUE)  # Extract text from each cell
      return(cells)
    })
  
  varsity_frame <- suppressWarnings(do.call(rbind, varsity) |> 
                                      as.data.frame())
  
  colnames(varsity_frame) <- c("Special Designation", "Date", "Opponent", "Outcome", "Score", "Matchup")
  
  table <- varsity_frame |> 
    mutate(Date = str_extract(Date,"\\b\\d{1,2}/\\d{1,2}\\b"),
           Date = mdy(str_c(str_extract(Date, "[^-]+"), "/",
                            ifelse(as.numeric(str_extract(Date, "[^/]+")) >= 11, season, season + 1))),
           Opponent = str_squish(str_remove_all(Opponent, "at|[^\\w\\s]|\\d")),
           score = as.numeric(str_remove_all(Score, "-.*|[^\\d]")),
           opp_score = as.numeric(str_remove_all(Score, "^[^\\-]*-|[^\\d]"))) |> 
    filter(Outcome %in% c("W", "L"))|> 
    group_by(Date, Opponent) |>
    filter(row_number() == 1) |>
    select(-Score) |> 
    ungroup()
  if (tbd){
    return(table |> filter(Date == d))
  }
  
  if (d != T & opp != T){
    target_game <- table |> 
      filter(str_detect(Opponent, opp) & abs(Date - d) <= 2)
  }else if (d != T){
    target_game <- table |> 
      filter(abs(Date - d) <= 2)
  }else if (opp != T){
    target_game <- table |> 
      filter(str_detect(Opponent, opp))
  }else{
    target_game <- table
  }
  
  target_game
}
