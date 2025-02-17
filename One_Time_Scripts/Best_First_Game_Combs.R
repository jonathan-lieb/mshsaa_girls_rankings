# Jonathan Lieb
# 2/15/25

# This script is designed to find which combination of variables 
# (league average, past year, past three years) best predicts the
# score for a game where the team hasn't played yet. 

#Libraries
library(tidyverse)
library(arrow)
library(tidymodels)

# Modleing data
data <- read_parquet("Data/modeling_data.parquet")

mod_data <- data |> 
  filter(season >= 2015)

set.seed(77)
data_split <- group_initial_split(mod_data, prop = 0.6, 
                                  group = "game_id")
train <- training(data_split)
test <- testing(data_split)

# Add average values and filter to be only teams with no prior games played
train_filtered <- train |> 
  filter(is.na(g)) |> 
  mutate(avg_points = 45)

# lm_mod ppg
lm_mod <- lm(score ~ ppg_ly + ppg_3y, data = train_filtered)
summary(lm_mod)
total <- abs(12.474) + abs(0.540 * mean(train_filtered$ppg_ly, na.rm = T)) + abs(0.182 * mean(train_filtered$ppg_3y, na.rm = T))
w1 <- 12.474 / total
w2 <- 0.540 * mean(train_filtered$ppg_ly, na.rm = T) / total
w3 <- 0.182 * mean(train_filtered$ppg_3y, na.rm = T) / total
lm_new <- train_filtered |> 
  mutate(ppg_ly = .28 * 12.474 + .54 * ppg_ly + .18 * ppg_3y)
lm_new_mod <- lm(score ~ ppg_ly, data = lm_new)
summary(lm_new_mod)

# lm_mod rpi
rpi_mod <- lm(score ~ rpi_ly + rpi_3y, data = train_filtered |> mutate(rpi_ly = ifelse(is.na(rpi_ly), 0.5, rpi_ly)))
summary(rpi_mod)
rpi_new <- train_filtered |> 
  mutate(rpi_ly = .13 + .70 * rpi_ly + .17 * rpi_3y)
rpi_new_mod <- lm(score ~ rpi_ly, data = rpi_new)
summary(rpi_new_mod)

print(train_filtered$rpi_ly)

# lm_mod crpi
crpi_mod <- lm(score ~ crpi_ly + crpi_3y, data = train_filtered |> mutate(crpi_ly = ifelse(is.na(crpi_ly), 0.5, crpi_ly)))
summary(crpi_mod)
crpi_new <- train_filtered |> 
  mutate(crpi_ly = .54 + .29 * crpi_ly + .17 * crpi_3y)
crpi_new_mod <- lm(score ~ crpi_ly, data = crpi_new)
summary(crpi_new_mod)

# lm_mod avg_pace
avg_pace_mod <- lm(score ~ avg_pace_ly + avg_pace_3y,
                   data = train_filtered |> mutate(avg_pace_ly = ifelse(is.na(avg_pace_ly),
                                                                        48, avg_pace_ly)))

summary(avg_pace_mod)
total <- abs(-49.0784) + abs(1.3529 * mean(train_filtered$avg_pace_ly, na.rm = T)) + abs(0.6033 * mean(train_filtered$avg_pace_3y, na.rm = T))
w1 <- 49.0784 / total
w2 <- 1.3529 * mean(train_filtered$avg_pace_ly, na.rm = T) / total
w3 <- 0.6033 * mean(train_filtered$avg_pace_3y, na.rm = T) / total

avg_pace_new <- train_filtered |> 
  mutate(avg_pace_ly = .23 * -49.0784 + .46 * avg_pace_ly + 0.21 * avg_pace_3y)
avg_pace_new_mod <- lm(score ~ avg_pace_ly, data = avg_pace_new)
summary(avg_pace_new_mod)

# lm_mod o_rate
o_rate_mod <- lm(score ~ o_rate_ly + o_rate_3y,
                  data = train_filtered |> mutate(o_rate_ly = ifelse(is.na(o_rate_ly),
                                                                       0, o_rate_ly)))
summary(o_rate_mod)
total <- abs(45.04473) + abs(0.30172 * mean(abs(train_filtered$o_rate_ly), na.rm = T)) + abs(0.0824 * mean(abs(train_filtered$o_rate_3y), na.rm = T))
w1 <- 45.04473 / total
w2 <- 0.30172 * mean(abs(train_filtered$o_rate_ly), na.rm = T) / total
w3 <- 0.0824 * mean(abs(train_filtered$o_rate_3y), na.rm = T) / total

sapply(train_filtered, function(x) sum(is.na(x)))
# combine all the models with new weights
combined_mod <- lm(score ~ ppg_ly + rpi_ly + crpi_ly + avg_pace_ly, data = train_filtered |> 
                     mutate(replace_na(ppg_ly, 54),
                            replace_na(ppg_3y, 54),
                            replace_na(rpi_ly, 0.5),
                            replace_na(rpi_3y, 0.5),
                            replace_na(crpi_ly, 0.5),
                            replace_na(crpi_3y, 0.5),
                            replace_na(avg_pace_ly, 48),
                            replace_na(avg_pace_3y, 48),
                            ) |>
                      mutate(ppg_ly = .28 * 12.474 + .54 * ppg_ly + .18 * ppg_3y,
                             rpi_ly = .13 + .70 * rpi_ly + .17 * rpi_3y,
                             crpi_ly = .54 + .29 * crpi_ly + .17 * crpi_3y,
                             avg_pace_ly = .23 * -49.0784 + .46 * avg_pace_ly + 0.21 * avg_pace_3y))
summary(combined_mod)
mae <- mean(abs(combined_mod$residuals))
mae

# Calculate the RMSE for baseline model
baseline_mod <- lm(score ~ ppg_ly + rpi_ly + crpi_ly + avg_pace_ly, data = train_filtered |> 
                      mutate(replace_na(ppg_ly, 54),
                             replace_na(ppg_3y, 54),
                             replace_na(rpi_ly, 0.5),
                             replace_na(rpi_3y, 0.5),
                             replace_na(crpi_ly, 0.5),
                             replace_na(crpi_3y, 0.5),
                             replace_na(avg_pace_ly, 48),
                             replace_na(avg_pace_3y, 48),
                      ))
summary(baseline_mod)
mae <- mean(abs(baseline_mod$residuals))
mae
