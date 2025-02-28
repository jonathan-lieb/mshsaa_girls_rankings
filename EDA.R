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
basic_file_path <- gsub("/", "\\\\", file.path(Sys.getenv("USERPROFILE"), "OneDrive",
                                               "Documents", "mshsaa_girls_rankings"))
miceadds::source.all(paste(basic_file_path, "Functions\\", sep = "\\\\"))

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

# Visualizations of distributions of most of the important variables
# score, g, OWE, DWE, ppg, papg, eff_margin, pts_margin, wp, rpi, avg_pace, 
# crpi, g_opp, OWE_opp, DWE_opp, ppg_opp, papg_opp, eff_margin_opp,
# pts_margin_opp, wp_opp, rpi_opp, avg_pace_opp, crpi_opp, ppg_ly,
# ppg_3y, papg_ly, papg_3y, crpi_ly, crpi_3y, pace_control, 
# ppg_ly_opp, ppg_3y_opp, papg_ly_opp, papg_3y_opp, crpi_ly_opp, crpi_3y_opp
# pace_control_opp

# before imputations and weighting
# score
df_stats(~score, data_filtered, mean, median, sd, IQR)
gf_density(~score, data = data_filtered, color = "red") |>
  gf_dist("norm", mean = mean(data_filtered$score), sd = sd(data_filtered$score))

# ppg
df_stats(~ppg, data_filtered, mean, median, sd, IQR)
gf_density(~ppg, data = data_filtered, color = "red") |>
  gf_dist("norm", mean = mean(data_filtered$ppg, na.rm = T), sd = sd(data_filtered$ppg, na.rm = T))
# Nearly normal distribution
gf_point(score ~ ppg, data = data_filtered, alpha = .5) |> 
  gf_smooth(score ~ ppg, data = data_filtered)
# Linear relationship
cor(data_filtered$score, data_filtered$ppg, use = "pairwise.complete.obs")
# .5756

# papg_opp
df_stats(~papg_opp, data_filtered, mean, median, sd, IQR)
gf_density(~papg_opp, data = data_filtered, color = "red") |>
  gf_dist("norm", mean = mean(data_filtered$papg_opp, na.rm = T), sd = sd(data_filtered$papg_opp, na.rm = T))
# Nearly normal distribution
gf_point(score ~ papg_opp, data = data_filtered, alpha = .5) |>
  gf_smooth(score ~ papg_opp, data = data_filtered)
# Linear relationship 
cor(data_filtered$score, data_filtered$papg_opp, use = "pairwise.complete.obs")
# .281

# OWE
df_stats(~OWE, data_filtered, mean, median, sd, IQR)
gf_density(~OWE, data = data_filtered, color = "red") |>
  gf_dist("norm", mean = mean(data_filtered$OWE, na.rm = T), sd = sd(data_filtered$OWE, na.rm = T))
# Skewed left
gf_point(score ~ OWE, data = data_filtered, alpha = .5) |>
  gf_smooth(score ~ OWE, data = data_filtered)
# Linear relationship
cor(data_filtered$score, data_filtered$OWE, use = "pairwise.complete.obs")
# .5535

# DWE_opp
df_stats(~DWE_opp, data_filtered, mean, median, sd, IQR)
gf_density(~DWE_opp, data = data_filtered, color = "red") |>
  gf_dist("norm", mean = mean(data_filtered$DWE_opp, na.rm = T), sd = sd(data_filtered$DWE_opp, na.rm = T))
# Skewed right slightly
gf_point(score ~ DWE_opp, data = data_filtered, alpha = .5) |>
  gf_smooth(score ~ DWE_opp, data = data_filtered)
# Linear relationship although not as strong
cor(data_filtered$score, data_filtered$DWE_opp, use = "pairwise.complete.obs")
# .279

# rpi
df_stats(~rpi, data_filtered, mean, median, sd, IQR)
gf_density(~rpi, data = data_filtered, color = "red") |>
  gf_dist("beta", shape1 = 20, shape2 = 20)
# Beta distribution but with spike at .5 because that is what every team 
# has after their first game
gf_point(score ~ rpi, data = data_filtered, alpha = .5) |>
  gf_smooth(score ~ rpi, data = data_filtered)
# Linear relationship
cor(data_filtered$score, data_filtered$rpi, use = "pairwise.complete.obs")
# .435

# rpi_opp
# Beta distribution but with spike at .5 because that is what every team
# has after their first game
gf_point(score ~ rpi_opp, data = data_filtered, alpha = .5) |>
  gf_smooth(score ~ rpi_opp, data = data_filtered)
# Negative Linear relationship as expected
cor(data_filtered$score, data_filtered$rpi_opp, use = "pairwise.complete.obs")
# -.1896

# crpi
df_stats(~crpi, data_filtered, mean, median, sd, IQR)
gf_density(~crpi, data = data_filtered, color = "red") |>
  gf_dist("beta", shape1 = 2, shape2 = 3)
# Not close to any form of typical distribution
gf_point(score ~ crpi, data = data_filtered, alpha = .1) |>
  gf_smooth(score ~ crpi, data = data_filtered)
# Not much of a relationship. It does appear that there 
# is more variablity in the score when crpi is lower
cor(data_filtered$score, data_filtered$crpi, use = "pairwise.complete.obs")
# .0976

# crpi_opp
gf_point(score ~ crpi_opp, data = data_filtered, alpha = .1) |>
  gf_smooth(score ~ crpi_opp, data = data_filtered)
# Not much of a relationship at all.
cor(data_filtered$score, data_filtered$crpi_opp, use = "pairwise.complete.obs")
# -.00888

# avg_pace
df_stats(~avg_pace, data_filtered, mean, median, sd, IQR)
gf_density(~avg_pace, data = data_filtered, color = "red") |>
  gf_dist("norm", mean = mean(data_filtered$avg_pace, na.rm = T), sd = sd(data_filtered$avg_pace, na.rm = T))
# Slightly skewed right
gf_point(score ~ avg_pace, data = data_filtered, alpha = .5) |>
  gf_smooth(score ~ avg_pace, data = data_filtered)
# Linear relationship, more variablity in score when pace is higher
cor(data_filtered$score, data_filtered$avg_pace, use = "pairwise.complete.obs")
# .336

# avg_pace_opp
gf_point(score ~ avg_pace_opp, data = data_filtered, alpha = .5) |>
  gf_smooth(score ~ avg_pace_opp, data = data_filtered)
# Slight positive linear relationship
cor(data_filtered$score, data_filtered$avg_pace_opp, use = "pairwise.complete.obs")
# .09889

# o_rate
df_stats(~o_rate, data_filtered, mean, median, sd, IQR)
gf_density(~o_rate, data = data_filtered, color = "red") |>
  gf_dist("norm", mean = mean(data_filtered$o_rate, na.rm = T), sd = sd(data_filtered$o_rate, na.rm = T))
# Skewed left
gf_point(score ~ o_rate, data = data_filtered, alpha = .5) |>
  gf_smooth(score ~ o_rate, data = data_filtered)
# Strong linear relationship
cor(data_filtered$score, data_filtered$o_rate, use = "pairwise.complete.obs")
# .552

# d_rate_opp
df_stats(~d_rate_opp, data_filtered, mean, median, sd, IQR)
gf_density(~d_rate_opp, data = data_filtered, color = "red") |>
  gf_dist("norm", mean = mean(data_filtered$d_rate_opp, na.rm = T), sd = sd(data_filtered$d_rate_opp, na.rm = T))
# right slightly with peak at 0
gf_point(score ~ d_rate_opp, data = data_filtered, alpha = .5) |>
  gf_smooth(score ~ d_rate_opp, data = data_filtered)
# Negative linear relationship
cor(data_filtered$score, data_filtered$d_rate_opp, use = "pairwise.complete.obs")
# -.273

# h_a_n
gf_boxplot(score ~ h_a_n, data = data) |>
  gf_labs(title = "Score by Home/Away/Neutral", x = "Location", y = "Score")
# Slightly higher values for home games


data <- data_filtered |> 
  weight_history()

# ppg
df_stats(~ppg, data, mean, median, sd, IQR)
gf_density(~ppg, data = data, color = "red") |>
  gf_dist("norm", mean = mean(data$ppg, na.rm = T), sd = sd(data$ppg, na.rm = T))
gf_point(score ~ ppg, data = data, alpha = .1) |>
  gf_smooth(score ~ ppg, data = data)
cor(data$score, data$ppg, use = "pairwise.complete.obs")
# .581

# papg_opp
df_stats(~papg_opp, data, mean, median, sd, IQR)
gf_density(~papg_opp, data = data, color = "red") |>
  gf_dist("norm", mean = mean(data$papg_opp, na.rm = T), sd = sd(data$papg_opp, na.rm = T))
gf_point(score ~ papg_opp, data = data, alpha = .1) |>
  gf_smooth(score ~ papg_opp, data = data)
cor(data$score, data$papg_opp, use = "pairwise.complete.obs")
# .315

#OWE
df_stats(~OWE, data, mean, median, sd, IQR)
gf_density(~OWE, data = data, color = "red") |>
  gf_dist("norm", mean = mean(data$OWE, na.rm = T), sd = sd(data$OWE, na.rm = T))
gf_point(score ~ OWE, data = data, alpha = .1) |>
  gf_smooth(score ~ OWE, data = data)
cor(data$score, data$OWE, use = "pairwise.complete.obs")
# .561

# DWE_opp
df_stats(~DWE_opp, data, mean, median, sd, IQR)
gf_density(~DWE_opp, data = data, color = "red") |>
  gf_dist("norm", mean = mean(data$DWE_opp, na.rm = T), sd = sd(data$DWE_opp, na.rm = T))
gf_point(score ~ DWE_opp, data = data, alpha = .1) |>
  gf_smooth(score ~ DWE_opp, data = data)
cor(data$score, data$DWE_opp, use = "pairwise.complete.obs")
# .288

# rpi
df_stats(~rpi, data, mean, median, sd, IQR)
gf_density(~rpi, data = data, color = "red") |>
  gf_dist("beta", shape1 = 20, shape2 = 20)
gf_point(score ~ rpi, data = data, alpha = .1) |>
  gf_smooth(score ~ rpi, data = data)
cor(data$score, data$rpi, use = "pairwise.complete.obs")
# .466

# rpi_opp
gf_point(score ~ rpi_opp, data = data, alpha = .1) |>
  gf_smooth(score ~ rpi_opp, data = data)
cor(data$score, data$rpi_opp, use = "pairwise.complete.obs")
# -.201

# crpi
df_stats(~crpi, data, mean, median, sd, IQR)
gf_density(~crpi, data = data, color = "red") |>
  gf_dist("beta", shape1 = 2, shape2 = 3)
gf_point(score ~ crpi, data = data, alpha = .1) |>
  gf_smooth(score ~ crpi, data = data)
cor(data$score, data$crpi, use = "pairwise.complete.obs")
# .0969

# crpi_opp
gf_point(score ~ crpi_opp, data = data, alpha = .1) |>
  gf_smooth(score ~ crpi_opp, data = data)
cor(data$score, data$crpi_opp, use = "pairwise.complete.obs")
# .0107

# avg_pace
df_stats(~avg_pace, data, mean, median, sd, IQR)
gf_density(~avg_pace, data = data, color = "red") |>
  gf_dist("norm", mean = mean(data$avg_pace, na.rm = T), sd = sd(data$avg_pace, na.rm = T))
gf_point(score ~ avg_pace, data = data, alpha = .1) |>
  gf_smooth(score ~ avg_pace, data = data)
cor(data$score, data$avg_pace, use = "pairwise.complete.obs")
# .354

# avg_pace_opp
gf_point(score ~ avg_pace_opp, data = data, alpha = .1) |>
  gf_smooth(score ~ avg_pace_opp, data = data)
cor(data$score, data$avg_pace_opp, use = "pairwise.complete.obs")
# .110

# o_rate
df_stats(~o_rate, data, mean, median, sd, IQR)
gf_density(~o_rate, data = data, color = "red") |>
  gf_dist("norm", mean = mean(data$o_rate, na.rm = T), sd = sd(data$o_rate, na.rm = T))
gf_point(score ~ o_rate, data = data, alpha = .1) |>
  gf_smooth(score ~ o_rate, data = data)
cor(data$score, data$o_rate, use = "pairwise.complete.obs")
# .558

# d_rate_opp
df_stats(~d_rate_opp, data, mean, median, sd, IQR)
gf_density(~d_rate_opp, data = data, color = "red") |>
  gf_dist("norm", mean = mean(data$d_rate_opp, na.rm = T), sd = sd(data$d_rate_opp, na.rm = T))
gf_point(score ~ d_rate_opp, data = data, alpha = .1) |>
  gf_smooth(score ~ d_rate_opp, data = data)
cor(data$score, data$d_rate_opp, use = "pairwise.complete.obs")
# -.281

cor(data |> select(score, ppg, papg_opp, OWE, DWE_opp, rpi, crpi, avg_pace, o_rate, d_rate_opp,
                   avg_pace_opp, rpi_opp, crpi_opp), 
    use = "pairwise.complete.obs")

# Strong correlations
# ppg - OWE = .948
# ppg - o_rate = .939
# ppg - rpi = .747
# ppg - avg_pace = .613
# papg_opp - DWE = .782
# papg_opp - d_rate_opp = -.761
# OWE - rpi = .814
# OWE - o_rate = .990
# DWE_opp - d_rate_opp = -.976
# DWE_opp - rpi_opp = -.772
# rpi - o_rate = .842
# crpi - crpi_opp = .852
# d_rate_opp - rpi_opp = .834

# Check for interactions
data |> 
  gf_point(score ~ rpi * crpi, alpha = .1) |>
  gf_smooth(score ~ rpi * crpi)
cor(data$score, data$rpi * data$crpi, use = "pairwise.complete.obs")


lm(score ~ rpi + crpi + ppg + papg_opp + avg_pace + o_rate +
     d_rate_opp + avg_pace_opp + rpi_opp + crpi_opp,
   data = data) |> 
  summary()

library(olsrr)
model <- lm(score ~ rpi + crpi + ppg + papg_opp + avg_pace + o_rate + DWE_opp +
              rpi_opp + h_a_n + 
              d_rate_opp + avg_pace_opp + rpi_opp + crpi_opp, data = data)
summary(model)
stepwise_model <- ols_step_both_aic(model, verbose = T)
print(stepwise_model)
plot(stepwise_model)
