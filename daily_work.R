# Jonathan Lieb
# 3/10/25

# This script is designed to be run each day during the night to 
# update the base df, the future df, the modeling df, and the 
# drankings.

# These first few lines were run once to get stuff up to date
library(tidyverse)
library(rvest)
library(arrow)
library(furrr)
library(tidymodels)
basic_file_path <- gsub("/", "\\\\", file.path(Sys.getenv("USERPROFILE"), "OneDrive",
                                               "Documents", "mshsaa_girls_rankings"))
miceadds::source.all(paste(basic_file_path, "Functions\\", sep = "\\\\"))
# initial_data <- read_parquet(paste(basic_file_path, "Data\\cleaned.parquet", sep = "\\\\"))
# write_parquet(initial_data, paste(basic_file_path, "Data\\base.parquet", sep = "\\\\"))
# modeling_data <- read_parquet(paste(basic_file_path, "Data\\modeling_data.parquet", sep = "\\\\"))
# write_parquet(modeling_data, paste(basic_file_path, "Data\\modeling.parquet", sep = "\\\\"))
# 
# base <- read_parquet(paste(basic_file_path, "Data\\base.parquet", sep = "\\\\"))
# modeling <- read_parquet(paste(basic_file_path, "Data\\modeling.parquet", sep = "\\\\"))
# drankings <- read_parquet(paste(basic_file_path, "Data\\drankings.parquet", sep = "\\\\"))
# date_range_past <- seq(as.Date("2025-01-25"), as.Date("2025-03-09"), by = "day")
# date_range_future <- seq(today(), today() + 7, by = "day")
# plan(multisession)
# new_data <- scrape_days(date_range_past, finished = T)
# new_cleaned <- clean_errors(base, new_data, recent = T)
# new_days <- new_data$date |> unique()
# new_modeling <- create_modeling_data_new(modeling, new_cleaned, new_days)
# new_drankings <- make_drankings(new_cleaned, past_drankings = drankings)
# new_data_future <- scrape_days(date_range_future, finished = F) |>
#   filter(school != "TBD" & opp != "TBD") |>
#   distinct(school, opp, .keep_all = TRUE)
# showConnections()
# write_parquet(new_cleaned, paste(basic_file_path, "Data\\base.parquet", sep = "\\\\"))
# write_parquet(new_modeling, paste(basic_file_path, "Data\\modeling.parquet", sep = "\\\\"))
# write_parquet(new_drankings, paste(basic_file_path, "Data\\drankings.parquet", sep = "\\\\"))

base <- read_parquet(paste(basic_file_path, "Data\\base.parquet", sep = "\\\\"))
modeling <- read_parquet(paste(basic_file_path, "Data\\modeling.parquet", sep = "\\\\"))
drankings <- read_parquet(paste(basic_file_path, "Data\\drankings.parquet", sep = "\\\\"))
date_range_past <- seq(today()-7, today()-1, by = "day")
date_range_future <- seq(today(), today() + 7, by = "day")
plan(multisession)
new_data <- scrape_days(date_range_past, finished = T)
new_cleaned <- clean_errors(base, new_data, recent = T)
new_days <- new_data$date |> unique()
new_modeling <- create_modeling_data_new(modeling, new_cleaned, new_days)
new_drankings <- make_drankings(new_cleaned, past_drankings = drankings)
new_data_future <- scrape_days(date_range_future, finished = F) |>
  filter(school != "TBD" & opp != "TBD") |>
  distinct(school, opp, .keep_all = TRUE)
showConnections()
write_parquet(new_cleaned, paste(basic_file_path, "Data\\base.parquet", sep = "\\\\"))
write_parquet(new_modeling, paste(basic_file_path, "Data\\modeling.parquet", sep = "\\\\"))
write_parquet(new_drankings, paste(basic_file_path, "Data\\drankings.parquet", sep = "\\\\"))
write_parquet(new_data_future, paste(basic_file_path, "Data\\upcoming.parquet", sep = "\\\\"))
