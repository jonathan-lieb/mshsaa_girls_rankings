# Jonathan Lieb
# 3/24/25

# This script is designed to make the end of season drankings for the 
# 2015 to 2024 seasons

seasons = 2015:2024
old_drankings <- read_parquet("Data/drankings.parquet")
base <- read_parquet("Data/base.parquet")
new_dranks <- map_dfr(seasons, ~base |> 
          filter(season <= .x) |> 
          make_drankings(old_drankings))

distinct_dranks <- new_dranks %>% 
  distinct(date, school, .keep_all = TRUE)

write_parquet(distinct_dranks, "Data/drankings.parquet")
