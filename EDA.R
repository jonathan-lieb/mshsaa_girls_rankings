# Jonathan Lieb
# 1/31/25

# EDA
# The main goal of this is to try to see the distributions of each of the likely
# variables, find correlations between them and the team's score, and try to 
# determine non-linear relationships that may exist. Additionally, I want to 
# find reasonable values to impute for missing data.


# Libraries 
library(tidyverse)
library(arrow)
library(ggformula)


# Data
data <- read_parquet("Data/modeling_data.parquet")


# Missing values in the data
sapply(data, function(x) sum(is.na(x)))
# Many of these predictors have somewhere between 10-20 thousand missing values.
# I know that most of these missing values occur in historical variables 
# (e.g. ppg_ly, papg_3y, etc.) that are not available for the first 1-3 years of
# the data, or when a team is playing their first game of the season. I will only be 
# using data from the 2014-15 season to the 2024-2025 season for modeling, so I will
# filter the data to only include those seasons and then check for missing values again.
data_filtered <- data |>
  filter(season >= 2015 & season <= 2025)

sapply(data_filtered, function(x) sum(is.na(x)))
# That reduced most of the missing value counts to 10,000 or so.
# I will start imputing the most obvious missing values, and then do 
# some more in depth analysis to determine what may be best for other missing values.

# Imputing missing values
# g and g_opp impute to 0
# ppg_ly, papg_ly impute to mean(score)
# ppg, papg, crpi impute to ppg_ly, papg_ly, (crpi_ly or crpi_opp)
# rpi, pace_control impute to .5
# OWE, DWE impute to mean(score),
# eff_margin, pts_margin impute to 0
# average pace impute to mean(pmax(score, opp_score))
# wp, wp_opp impute to .5
avg_score <- mean(data_filtered$score, na.rm = T) #45.23
avg_pace <- mean(pmax(data_filtered$score, data_filtered$opp_score), na.rm = T) # 55.15

data_imputed <- data_filtered |>
  mutate(g = ifelse(is.na(g), 0, g),
         g_opp = ifelse(is.na(g_opp), 0, g_opp),
         ppg_ly = ifelse(is.na(ppg_ly), 45.23, ppg_ly),
         ppg_ly_opp = ifelse(is.na(ppg_ly_opp), 45.23, ppg_ly_opp),
         papg_ly = ifelse(is.na(papg_ly), 45.23, papg_ly),
         papg_ly_opp = ifelse(is.na(papg_ly_opp), 45.23, papg_ly_opp),
         ppg = ifelse(is.na(ppg), ppg_ly, ppg),
         ppg_opp = ifelse(is.na(ppg_opp), ppg_ly_opp, ppg_opp),
         papg = ifelse(is.na(papg), papg_ly, papg),
         papg_opp = ifelse(is.na(papg_opp), papg_ly_opp, papg_opp),
         crpi = case_when(!is.na(crpi) ~ crpi,
                          !is.na(crpi_ly) ~ crpi_ly,
                          !is.na(crpi_opp) ~ crpi_opp,
                          T ~ .49),
         crpi_opp = case_when(!is.na(crpi_opp) ~ crpi_opp,
                            !is.na(crpi_ly_opp) ~ crpi_ly_opp,
                            !is.na(crpi) ~ crpi,
                            T ~ .49),
         rpi = ifelse(is.na(rpi), .5, rpi),
         rpi_opp = ifelse(is.na(rpi_opp), .5, rpi_opp),
         pace_control = ifelse(is.na(pace_control), .5, pace_control),
         pace_control_opp = ifelse(is.na(pace_control_opp), .5, pace_control_opp),
         OWE = ifelse(is.na(OWE), 45.23, OWE),
         OWE_opp = ifelse(is.na(OWE_opp), 45.23, OWE_opp),
         DWE = ifelse(is.na(DWE), 45.23, DWE),
         DWE_opp = ifelse(is.na(DWE_opp), 45.23, DWE_opp),
         eff_margin = ifelse(is.na(eff_margin), 0, eff_margin),
         eff_margin_opp = ifelse(is.na(eff_margin_opp), 0, eff_margin_opp),
         pts_margin = ifelse(is.na(pts_margin), 0, pts_margin),
         pts_margin_opp = ifelse(is.na(pts_margin_opp), 0, pts_margin_opp),
         avg_pace = ifelse(is.na(avg_pace), 55.15, avg_pace),
         avg_pace_opp = ifelse(is.na(avg_pace_opp), 55.15, avg_pace_opp),
         wp = ifelse(is.na(wp), .5, wp),
         wp_opp = ifelse(is.na(wp_opp), .5, wp_opp)
  )

sapply(data_imputed, function(x) sum(is.na(x)))

# Visualizations of distributions of most of the important variables
# score, g, OWE, DWE, ppg, papg, eff_margin, pts_margin, wp, rpi, avg_pace, 
# crpi, g_opp, OWE_opp, DWE_opp, ppg_opp, papg_opp, eff_margin_opp,
# pts_margin_opp, wp_opp, rpi_opp, avg_pace_opp, crpi_opp, ppg_ly,
# ppg_3y, papg_ly, papg_3y, crpi_ly, crpi_3y, pace_control, 
# ppg_ly_opp, ppg_3y_opp, papg_ly_opp, papg_3y_opp, crpi_ly_opp, crpi_3y_opp
# pace_control_opp
data <- data_imputed

# Score
df_stats(~score, data, mean, median, sd, IQR)
gf_density(~score, data = data, color = "red") |> 
gf_dist("norm", mean = mean(data$score), sd = sd(data$score))

# g
df_stats(~g, data, mean, median, sd, IQR)
gf_density(~g, data = data, color = "red")

# OWE
df_stats(~OWE, data, mean, median, sd, IQR)
gf_density(~OWE, data = data, color = "red") |> 
  gf_dist("norm", mean = mean(data$OWE, na.rm =T), sd = sd(data$OWE, na.rm =T))

# DWE
df_stats(~DWE, data, mean, median, sd, IQR)
gf_density(~DWE, data = data, color = "red") |>
  gf_dist("norm", mean = mean(data$DWE, na.rm =T), sd = sd(data$DWE, na.rm =T))

# ppg
df_stats(~ppg, data, mean, median, sd, IQR)
gf_density(~ppg, data = data, color = "red") |>
  gf_dist("norm", mean = mean(data$ppg, na.rm = T), sd = sd(data$ppg, na.rm = T))

# papg
df_stats(~papg, data, mean, median, sd, IQR)
gf_density(~papg, data = data, color = "red") |>
  gf_dist("norm", mean = mean(data$papg, na.rm = T), sd = sd(data$papg, na.rm = T))

# eff_margin
df_stats(~eff_margin, data, mean, median, sd, IQR)
gf_density(~eff_margin, data = data, color = "red") |>
  gf_dist("norm", mean = mean(data$eff_margin, na.rm = T), sd = sd(data$eff_margin, na.rm = T))

# pts_margin
df_stats(~pts_margin, data, mean, median, sd, IQR)
gf_density(~pts_margin, data = data, color = "red") |>
  gf_dist("norm", mean = mean(data$pts_margin, na.rm = T), sd = sd(data$pts_margin, na.rm = T))

# wp
df_stats(~wp, data, mean, median, sd, IQR)
gf_density(~wp, data = data, color = "red") |>
gf_dist("beta",shape1 = 2,shape2 =  2)

# rpi
df_stats(~rpi, data, mean, median, sd, IQR)
gf_density(~rpi, data = data, color = "red") |>
  gf_dist("beta",shape1 = 20,shape2 =  20)

# avg_pace
df_stats(~avg_pace, data, mean, median, sd, IQR)
gf_density(~avg_pace, data = data, color = "red") |>
  gf_dist("norm", mean = mean(data$avg_pace, na.rm = T), sd = sd(data$avg_pace, na.rm = T))

# crpi
df_stats(~crpi, data, mean, median, sd, IQR)
gf_density(~crpi, data = data, color = "red") |>
  gf_dist("beta",shape1 = 2,shape2 =  3)

# ppg_ly
df_stats(~ppg_ly, data, mean, median, sd, IQR)
gf_density(~ppg_ly, data = data, color = "red") |>
  gf_dist("norm", mean = mean(data$ppg_ly, na.rm = T), sd = sd(data$ppg_ly, na.rm = T))

#ppg_3y
df_stats(~ppg_3y, data, mean, median, sd, IQR)
gf_density(~ppg_3y, data = data, color = "red") |>
  gf_dist("norm", mean = mean(data$ppg_3y, na.rm = T), sd = sd(data$ppg_3y, na.rm = T))

# papg_ly
df_stats(~papg_ly, data, mean, median, sd, IQR)
gf_density(~papg_ly, data = data, color = "red") |>
  gf_dist("norm", mean = mean(data$papg_ly, na.rm = T), sd = sd(data$papg_ly, na.rm = T))

#crpi_ly
df_stats(~crpi_ly, data, mean, median, sd, IQR)
gf_density(~crpi_ly, data = data, color = "red") |>
  gf_dist("beta",shape1 = 1,shape2 =  1)

# crpi_3y
df_stats(~crpi_3y, data, mean, median, sd, IQR)
gf_density(~crpi_3y, data = data, color = "red") |>
  gf_dist("beta",shape1 = 1,shape2 =  1)

# pace_control
df_stats(~pace_control, data, mean, median, sd, IQR)
gf_density(~pace_control, data = data, color = "red") |>
gf_dist("beta", 8, 6)

corr_matrix <- cor(data[c("score", "g", "OWE", "DWE", "ppg", "papg", "eff_margin",
                          "pts_margin", "wp", "rpi", "avg_pace", "crpi",
                          "g_opp", "OWE_opp", "DWE_opp", "ppg_opp", 
                          "papg_opp", "eff_margin_opp", "pts_margin_opp",
                          "wp_opp", "rpi_opp", "avg_pace_opp", "crpi_opp",
                          "ppg_ly", "ppg_3y", "papg_ly", "papg_3y",
                          "crpi_ly", "crpi_3y", "pace_control",
                          "ppg_ly_opp", "ppg_3y_opp", "papg_ly_opp", "papg_3y_opp",
                          "crpi_ly_opp", "crpi_3y_opp", "pace_control_opp"
                          )], use = "pairwise.complete.obs")

score_corrs <- corr_matrix[,"score"] |> sort(decreasing = T)
score_corrs

high_corr <- corr_matrix |>
  as.data.frame() |> 
  rownames_to_column(var = "var1") |>
  pivot_longer(!var1, names_to = "var2", values_to = "correlation") |>
  filter((correlation < -.8 | correlation > .8) & correlation != 1 & correlation != -1) |>
  arrange(desc(correlation)) 

full_lm <- lm(score ~ g + OWE + DWE + ppg + papg + eff_margin + pts_margin + wp + rpi + avg_pace + crpi +
              g_opp + OWE_opp + DWE_opp + ppg_opp + papg_opp + eff_margin_opp +
              pts_margin_opp + wp_opp + rpi_opp + avg_pace_opp + crpi_opp+ 
              ppg_ly + ppg_3y + papg_ly + papg_3y + crpi_ly + crpi_3y + pace_control +
              ppg_ly_opp + ppg_3y_opp + papg_ly_opp + papg_3y_opp + crpi_ly_opp + crpi_3y_opp + pace_control_opp
                , data = data)
summary(full_lm)

# Look at each variable's relationship with score individually in a linear model
# Including R squared and p-value
lm_results <- map(c("g", "OWE", "DWE", "ppg", "papg", "eff_margin",
                     "pts_margin", "wp", "rpi", "avg_pace", "crpi",
                     "g_opp", "OWE_opp", "DWE_opp", "ppg_opp", 
                     "papg_opp", "eff_margin_opp", "pts_margin_opp",
                     "wp_opp", "rpi_opp", "avg_pace_opp", "crpi_opp",
                     "ppg_ly", "ppg_3y", "papg_ly", "papg_3y",
                     "crpi_ly", "crpi_3y", "pace_control",
                     "ppg_ly_opp", "ppg_3y_opp", "papg_ly_opp", "papg_3y_opp",
                     "crpi_ly_opp", "crpi_3y_opp", "pace_control_opp"
                    ), 
                   ~ lm(as.formula(paste("score ~ ", .x)), data = data))
lm_results <- setNames(lm_results, c("g", "OWE", "DWE", "ppg", "papg", "eff_margin",
                            "pts_margin", "wp", "rpi", "avg_pace", "crpi",
                            "g_opp", "OWE_opp", "DWE_opp", "ppg_opp", 
                            "papg_opp", "eff_margin_opp", "pts_margin_opp",
                            "wp_opp", "rpi_opp", "avg_pace_opp", "crpi_opp",
                            "ppg_ly", "ppg_3y", "papg_ly", "papg_3y",
                            "crpi_ly", "crpi_3y", "pace_control",
                            "ppg_ly_opp", "ppg_3y_opp", "papg_ly_opp", "papg_3y_opp",
                            "crpi_ly_opp", "crpi_3y_opp", "pace_control_opp"
                            ))
lm_summaries <- map(lm_results, summary)
lm_summaries <- setNames(lm_summaries, c("g", "OWE", "DWE", "ppg", "papg", "eff_margin",
                                           "pts_margin", "wp", "rpi", "avg_pace", "crpi",
                                           "g_opp", "OWE_opp", "DWE_opp", "ppg_opp", 
                                           "papg_opp", "eff_margin_opp", "pts_margin_opp",
                                           "wp_opp", "rpi_opp", "avg_pace_opp", "crpi_opp",
                                           "ppg_ly", "ppg_3y", "papg_ly", "papg_3y",
                                           "crpi_ly", "crpi_3y", "pace_control",
                                           "ppg_ly_opp", "ppg_3y_opp", "papg_ly_opp", "papg_3y_opp",
                                           "crpi_ly_opp", "crpi_3y_opp", "pace_control_opp"
                                         ))

lm_summaries

# Some likely combinations
lm(score ~ ppg + papg_opp + OWE + DWE_opp, data = data) |> summary()

lm(score ~ ppg *g + papg_opp * g_opp + OWE * g + DWE_opp * g_opp, data = data) |> summary()

lm(score ~ ppg + papg_opp + OWE + DWE_opp + crpi + crpi_opp +rpi + rpi_opp, data = data) |> summary()

lm(score ~ ppg + papg_opp + OWE + DWE_opp + avg_pace*pace_control + avg_pace_opp*pace_control_opp, data = data) |> summary()

mod <- lm(score ~ ppg_ly*g + papg_ly_opp*g_opp + crpi_ly + crpi_ly_opp +
     ppg*g + papg_opp*g_opp + OWE *g + DWE_opp *g_opp + rpi*g + rpi_opp*g_opp +
     crpi + crpi_opp, data = data)

summary(mod)

preds <- mod$fitted.values
errs <- mod$residuals

gf_point(preds ~ errs, data = tibble(preds, errs)) |>
  gf_labs(x = "Error", y = "Predicted") |>
  gf_refine(ggplot2::geom_hline(yintercept = 0, lty = 2))



