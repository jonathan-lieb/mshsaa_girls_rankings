# Jonathan Lieb
# 3/1/2025

# initial future scrape
library(tidyverse)
library(rvest)
library(arrow)
library(furrr)
library(parallel)

basic_file_path <- gsub("/", "\\\\", file.path(Sys.getenv("USERPROFILE"), "OneDrive",
                                               "Documents", "mshsaa_girls_rankings"))
miceadds::source.all(paste(basic_file_path, "Functions\\", sep = "\\\\"))

# Then I call scrape_days in parallel to scrape all games for each day
# # Parallel setup
plan(multisession, workers = availableCores())
initial_future_data <- scrape_days(seq(today(), today() + 7, by = "day")) |> 
  filter(school != "TBD" & opp != "TBD") |> 
  distinct(school, opp, .keep_all = TRUE) 
write_parquet(initial_future_data, paste(basic_file_path, "Data\\", sep = "\\\\", "upcoming.parquet"))
    
