# Jonathan Lieb
# 1/28/25

# This script creates the original girls data set

# Load packages and set file path
library(tidyverse)
library(rvest)
library(arrow)
library(furrr)
library(parallel)

basic_file_path <- gsub("/", "\\\\", file.path(Sys.getenv("USERPROFILE"), "OneDrive",
                                               "Documents", "mshsaa_girls_rankings"))

# First I make a date vector with every day from November 1 to April 1
# for the years 2010-11 to 2024-25. No days in the summer are included.
game_dates <- tibble(year = 2010:2024) |> 
  mutate(dates = map(year, ~ seq.Date(
    from = as.Date(paste0(.x, "-11-01")),
    to = as.Date(paste0(.x + 1, "-04-01")),
    by = "day"
  )))  |> 
  unnest(dates) |>
  filter(dates < today()) |> 
  pull(dates)

# Next I load all functions from the Functions folder
miceadds::source.all(paste(basic_file_path, "Functions\\", sep = "\\\\"))

# Then I call scrape_days in parallel to scrape all games for each day
# # Parallel setup
# plan(multisession, workers = availableCores())
initial_data <- scrape_days(game_dates, finished = T)

write_parquet(initial_data, paste(basic_file_path, "Data\\", "initial_data.parquet", sep = "\\"))
initial_data <- read_parquet(paste(basic_file_path, "Data\\", "initial_data.parquet", sep = "\\"))

# Then I clean the data of errors

cleaned <- clean_errors(initial_data)
cleaned_errors <- cleaned |> 
  group_by(game_id) |>
  filter(n() != 2)
write_parquet(cleaned, paste(basic_file_path, "Data\\", "cleaned.parquet", sep = "\\"))

cleaned <- read_parquet(paste(basic_file_path, "Data\\", "cleaned.parquet", sep = "\\"))
# Create modeling data
modeling_data <- create_modeling_data(cleaned)
write_parquet(modeling_data, paste(basic_file_path, "Data\\", "modeling_data.parquet", sep = "\\"))
