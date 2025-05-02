drankings_jan <- read_parquet("Data/drankings.parquet") |> 
  filter(date == mdy("01-28-2025"))

ranks_jan_28 <- drankings_jan |> 
  filter(rank <= 10) |> 
  select(school, class, rank) |> 
  pivot_wider(names_from = class, values_from = school)

write_csv(ranks_jan_28, "Data/ranks_jan_28.csv")
