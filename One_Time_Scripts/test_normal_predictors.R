# Jonathan Lieb
# 2/12/25

# This script is designed to check if there is some better way to 
# quantify offensive and defensive rates.

# libraries
library(tidyverse)
library(arrow)

# Data
cleaned_data <- read_parquet("Data/cleaned.parquet")
current_modeling_data <- read_parquet("Data/modeling_data.parquet")

# Functions
basic_file_path <- gsub("/", "\\\\", file.path(Sys.getenv("USERPROFILE"), "OneDrive",
                                               "Documents", "mshsaa_girls_rankings"))
miceadds::source.all(paste(basic_file_path, "Functions\\", sep = "\\\\"))



normalized_data <- current_modeling_data |> 
  mutate(across(all_of(c("score", "g", "g_opp", "ppg", "papg_opp",
                  "crpi", "crpi_opp", "rpi", "rpi_opp", "pace_control",
                  "pace_control_opp", "OWE", "DWE_opp", "avg_pace",
                  "avg_pace_opp")), ~replace(., is.na(.), 0))) |> 
  mutate(new_ppg = ppg*(1-(1/2)^g) + ppg_ly * (.5)^g,
         new_papg = papg*(1-(1/2)^g) + papg_ly * (.5)^g,
         new_crpi = crpi*(1-(1/2)^g) + crpi_ly * (.5)^g,
         new_papg_opp = papg_opp*(1-(1/2)^g_opp) + papg_ly_opp * (.5)^g_opp,
         new_crpi_opp = crpi_opp*(1-(1/2)^g_opp) + crpi_ly_opp * (.5)^g_opp,
         new_ppg_opp = ppg_opp*(1-(1/2)^g_opp) + ppg_ly_opp * (.5)^g_opp
         ) |> 
  group_by(g) |>
  filter(season >= 2015) |> 
  mutate(norm_ppg = qnorm(rank(new_ppg, ties.method = "min")/(n()+.5)),
         norm_papg = qnorm(rank(new_papg, ties.method = "min")/(n()+.5)),
         norm_crpi = qnorm(rank(new_crpi, ties.method = "min")/(n()+.5)),
         norm_papg_opp = qnorm(rank(new_papg_opp, ties.method = "min")/(n()+.5)),
         norm_crpi_opp = qnorm(rank(new_crpi_opp, ties.method = "min")/(n()+.5)),
         norm_ppg_opp = qnorm(rank(new_ppg_opp, ties.method = "min")/(n()+.5)))

lm_mod <- lm(score ~ norm_papg_opp + norm_crpi_opp,
     data = normalized_data)

lm_mod_opp <- lm(opp_score ~ norm_ppg + norm_crpi,
     data = normalized_data)

normalized_pred_scores <- tibble(normalized_data, pred_scored = predict(lm_mod, normalized_data),
                                 pred_score_opp = lm_mod_opp, normalized_data) |> 
  mutate(PAP = score - pred_scored,
         PAP_opp = pred_score_opp - opp_score) |> 
  group_by(g) |> 
  mutate(norm_PAP = qnorm(rank(PAP, ties.method = "min")/(n()+.5))) |> 
  group_by(school, season) |>
  mutate(norm_PAP_mean = mean(norm_PAP, na.rm = TRUE)) |>
  ungroup()

brm_mod <- brm(score ~ norm_ppg(opp))