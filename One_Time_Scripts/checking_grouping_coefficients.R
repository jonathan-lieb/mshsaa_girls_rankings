library(tidyverse)
library(tidymodels)
group_models <- read_rds("Models/pen_reg_group_models.rds")
a <- group_models |> imap_dfr(~.x |> extract_fit_parsnip() |> tidy() |>
                                arrange(desc(abs(estimate))) |> 
                                mutate(grouping = .y))
intercepts <- a |> 
  filter(term == "(Intercept)") |> 
  arrange(desc(estimate))

most_important <- a |> 
  filter(term != "(Intercept)") |> 
  group_by(grouping) |> 
  filter(row_number() == 1)

total_rmse <- group_models |> 
  map_dbl(~.x |> collect_metrics() |> filter(.metric == "rmse") |> pull(.estimate) *
            .x |> collect_predictions() |> summarize(count = n()) |> pull(count))

total_rsq <- group_models |> 
  map_dbl(~.x |> collect_metrics() |> filter(.metric == "rsq") |> pull(.estimate)*
            .x |> collect_predictions() |> summarize(count = n()) |> pull(count))

total_mae <- group_models |> 
  map_dbl(~.x |> collect_metrics() |> filter(.metric == "mae") |> pull(.estimate)*
            .x |> collect_predictions() |> summarize(count = n()) |> pull(count))

total_mape <- group_models |> 
  map_dbl(~.x |> collect_metrics() |> filter(.metric == "mape") |> pull(.estimate)*
            .x |> collect_predictions() |> summarize(count = n()) |> pull(count))

total_preds <- group_models |> 
  map_dbl(~.x |> collect_predictions() |> summarize(count = n()) |> pull(count))

sum(total_rmse) / sum(total_preds)
sum(total_rsq) / sum(total_preds)
sum(total_mae) / sum(total_preds)
sum(total_mape) / sum(total_preds)

read_rds("Data/reg_metrics.rds")

modeling_data <- read_parquet("Data/modeling_data.parquet")
preds <- predict_grouping_model(new_data = modeling_data |> filter(season ==2025), 
                               group_models)
