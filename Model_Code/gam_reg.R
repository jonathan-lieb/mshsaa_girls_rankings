# Jonathan Lieb
# 2/11/25

# Libraries
library(tidyverse)
library(tidymodels)
library(mgcv)
library(arrow)

basic_file_path <- gsub("/", "\\\\", file.path(Sys.getenv("USERPROFILE"), "OneDrive",
                                               "Documents", "mshsaa_girls_rankings"))
miceadds::source.all(paste(basic_file_path, "Functions\\", sep = "\\\\"))
# Data
data <- read_parquet("Data/modeling_data.parquet") |> 
  filter(season >= 2015) |> 
  impute_and_prep_data()

# Data Split
set.seed(77)
data_split <- group_initial_split(data, prop = 0.6, group = game_id)
train <- training(data_split)
test <- testing(data_split)

glimpse(train)

gam_model <- gam(score ~ s(g) + s(g_opp) + s(ppg) + s(papg_opp) + 
                   s(crpi) + s(crpi_opp) + s(rpi) + s(rpi_opp) +
                   s(pace_control) + s(pace_control_opp) + s(OWE) + 
                   s(DWE_opp) + s(avg_pace) + s(avg_pace_opp) + h_a_n, 
                 data = train)
summary(gam_model)

preds <- predict(gam_model, newdata = test)
gam_preds <- tibble(test, preds = predict(gam_model, newdata = test))
sd(gam_preds$score)
gam_preds |> 
  rmse(truth = score, estimate = preds)
