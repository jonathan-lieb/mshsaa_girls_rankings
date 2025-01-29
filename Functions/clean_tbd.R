# Jonathan Lieb
# 1/25/25
# Deprecated

# Libraries needed
# library(tidyverse)
# library(rvest)

# This function is designed to clean games played by a team called TBD from the data.
# It takes a dataframe as input and returns a dataframe.
clean_tbd <- function(df){
  scrape_tbd <- function(id, date, season){
    print(paste0("https://www.mshsaa.org/MySchool/Schedule.aspx?s=", id, "&alg=6&year=", season))
    url <- read_html(paste0("https://www.mshsaa.org/MySchool/Schedule.aspx?s=", id, "&alg=6&year=", season))
    tab <- html_table(url)
    table3 <- ifelse(nrow(tab[[2]])<5, T, F)
    if (table3){
      table <- tab[[3]] |>
        mutate(Date = str_extract(Date,"\\b\\d{1,2}/\\d{1,2}\\b")) |>
        filter(Outcome %in% c("W", "L"))
    }else{
      table <- tab[[2]]
    }
    mock <- table |>
      mutate(Date = gsub(".* ","", Date),
             Date = mdy(str_c(str_extract(Date, "[^-]+"), "/",
                              ifelse(as.numeric(str_extract(Date, "[^/]+")) >= 11, season, season + 1)))) |>
      filter(abs(as.numeric(Date - date)) == min(abs(as.numeric(Date - date))),
             abs(as.numeric(Date - date)) <= 5) |>
      filter(!is.na(Score), Outcome %in% c("W", "L"))
    print(mock)
    if(nrow(mock)<1){
      actual <- NA
    }else{
      actual <- mock |> pull(Opponent) |> first()
    }
    actual
  }
  for(i in seq_len(nrow(df))){
    school <-  df[i,]$school
    id <- df[i, ]$schoolid.x
    season <- df[i,]$season - 1
    date <- df[i,]$date
    o <- scrape_tbd(id, date, season)
    df[i, ]$opp <- o
    print(i)
  }
  df
}