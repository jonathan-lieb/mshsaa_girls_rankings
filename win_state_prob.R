# Get Win State Probability

# first round probs
single_round_probs <- function(prob_matrix, initial_play_probs, group_divisor){
  n <- nrow(prob_matrix)
  next_round_probs <- rep(0, 8)
  for(i in 1:n){
    groups <- (1:n - 1) %/% group_divisor
    base_group <- groups[i]
    if (base_group %% 2 == 0){
      competition <- which(groups == base_group +1)
    }else{
      competition <- which(groups == base_group - 1)
    }
    play_next_round_prob <- 0
    for (j in 1:length(competition)){
      play_next_round_prob <- play_next_round_prob + prob_matrix[i, competition[j]] * initial_play_probs[competition[j]] * initial_play_probs[i]
    }
    next_round_probs[i] <- play_next_round_prob
  }
  return(next_round_probs)
}

schools <- c("A", "B", "C", "D", "E", "F", "G", "H",
             "I", "J", "K", "L", "M", "N", "O", "P",
             "Q", "R", "S", "T", "U", "V", "W", "X",
             "Y", "Z", "AA", "BB", "CC", "DD", "EE", "FF",
             "GG", "HH", "II", "JJ", "KK", "LL", "MM", "NN",
             "OO", "PP", "QQ", "RR", "SS", "TT", "UU", "VV",
             "WW", "XX", "YY", "ZZ", "AAA", "BBB", "CCC", "DDD")
prob_tibble <- expand_grid(school = schools, opp= schools) |> 
  mutate(gid = ifelse(school > opp, paste(school, opp), paste(opp, school))) |>
  filter(school != opp) |> 
  group_by(gid) |>
  mutate(.pred_w = ifelse(school == opp, 1, runif(1))) |> 
  mutate(.pred_w = ifelse(row_number() == 1, .pred_w, 1 - lag(.pred_w))) 

team_tibble <- tibble(school = schools[1:56],
                      rank = 1:56,
                      district = c(rep(1:8, 7)))
teams_rearranged <- team_tibble |> 
  group_by(district) |>
  arrange(rank) |>
  mutate(row = row_number()) |> 
  complete(row = 1:8, fill = list(school = NA, rank = NA)) |> 
  mutate(
    school = ifelse(is.na(school), paste0("bye_", district, "_", row), school)
  ) |> 
  mutate(pos = case_when(row_number() == 1 ~ 1, 
                         row_number() == 2 ~ 7,
                         row_number() == 3 ~ 5,
                         row_number() == 4 ~ 3,
                         row_number() == 5 ~ 4,
                         row_number() == 6 ~ 6,
                         row_number() == 7 ~ 8,
                         row_number() == 8 ~ 2)) |> 
  arrange(district, pos) |> 
  pull(school)

n <- length(teams_rearranged)
prob_matrix <- matrix(NA, nrow = n, ncol = n, dimnames = list(teams_rearranged, teams_rearranged))
for (i in seq_len(nrow(prob_tibble))) {
  prob_matrix[prob_tibble$school[i], prob_tibble$opp[i]] <- prob_tibble$.pred_w[i]
}

# Set diagonal to 1
diag(prob_matrix) <- 1


print(prob_matrix)

initial_play_probs <- rep(1, nrow(prob_matrix))
round2 <- single_round_probs(prob_matrix, initial_play_probs, 1)
round3 <- single_round_probs(prob_matrix, round2, 2)
round4 <- single_round_probs(prob_matrix, round3, 4)
round5 <- single_round_probs(prob_matrix, round4, 8)
round6 <- single_round_probs(prob_matrix, round5, 16)
round7 <- single_round_probs(prob_matrix, round6, 32)
tibble(team = teams_rearranged, round2, round3, round4, round5, round6,
       round7) |>
  arrange(desc(round7))

prob1_second <- prob_1beat2
prob2_second <- prob_2beat1
prob3_second <- prob_3beat4
prob4_second <- prob_4beat3
prob5_second <- prob_5beat6
prob6_second <- prob_6beat5
prob7_second <- prob_7beat8
prob8_second <- prob_8beat7

prob1_third <- prob1_second * (prob_1beat3 * prob3_second + prob_1beat4 * prob4_second)
prob2_third <- prob2_second * (prob_2beat3 * prob3_second + prob_2beat4 * prob4_second)
prob3_third <- prob3_second * (prob_3beat1 * prob1_second + prob_3beat2 * prob2_second)
prob4_third <- prob4_second * (prob_4beat1 * prob1_second + prob_4beat2 * prob2_second)
prob5_third <- prob5_second * (prob_5beat7 * prob7_second + prob_5beat8 * prob8_second)
prob6_third <- prob6_second * (prob_6beat7 * prob7_second + prob_6beat8 * prob8_second)
prob7_third <- prob7_second * (prob_7beat5 * prob5_second + prob_7beat6 * prob6_second)
prob8_third <- prob8_second * (prob_8beat5 * prob5_second + prob_8beat6 * prob6_second)

prob1_fourth <- prob1_third * (prob_1beat5 * prob5_third + prob_1beat6 * prob6_third + prob_1beat7 * prob7_third + prob_1beat8 * prob8_third)
prob2_fourth <- prob2_third * (prob_2beat5 * prob5_third + prob_2beat6 * prob6_third + prob_2beat7 * prob7_third + prob_2beat8 * prob8_third)
prob3_fourth <- prob3_third * (prob_3beat5 * prob5_third + prob_3beat6 * prob6_third + prob_3beat7 * prob7_third + prob_3beat8 * prob8_third)
prob4_fourth <- prob4_third * (prob_4beat5 * prob5_third + prob_4beat6 * prob6_third + prob_4beat7 * prob7_third + prob_4beat8 * prob8_third)
prob5_fourth <- prob5_third * (prob_5beat1 * prob1_third + prob_5beat2 * prob2_third + prob_5beat3 * prob3_third + prob_5beat4 * prob4_third)
prob6_fourth <- prob6_third * (prob_6beat1 * prob1_third + prob_6beat2 * prob2_third + prob_6beat3 * prob3_third + prob_6beat4 * prob4_third)
prob7_fourth <- prob7_third * (prob_7beat1 * prob1_third + prob_7beat2 * prob2_third + prob_7beat3 * prob3_third + prob_7beat4 * prob4_third)
prob8_fourth <- prob8_third * (prob_8beat1 * prob1_third + prob_8beat2 * prob2_third + prob_8beat3 * prob3_third + prob_8beat4 * prob4_third)


team <- c(1, 2, 3, 4, 5, 6, 7, 8)
second <- c(prob1_second, prob2_second, prob3_second, prob4_second, prob5_second, prob6_second, prob7_second, prob8_second)
third <- c(prob1_third, prob2_third, prob3_third, prob4_third, prob5_third, prob6_third, prob7_third, prob8_third)
fourth <- c(prob1_fourth, prob2_fourth, prob3_fourth, prob4_fourth, prob5_fourth, prob6_fourth, prob7_fourth, prob8_fourth)
data.frame(team, second, third, fourth) |> 
  arrange(desc(fourth))

# Extend the above work to 16 teams and put the starting values in a tibble
library(dplyr)

# Initialize pairwise win probabilities
prob_matrix <- matrix(NA, nrow = 16, ncol = 16)

# Manually set probabilities for given matchups (fill in as needed)
prob_matrix[1, 2] <- 1
prob_matrix[1, 3] <- 0.8
prob_matrix[1, 4] <- 0.9
prob_matrix[1, 5] <- 0.7
prob_matrix[1, 6] <- 0.95
prob_matrix[1, 7] <- 0.6
prob_matrix[1, 8] <- 0.99
# Extend for teams 9-16 similarly...

# Fill in complementary probabilities
for (i in 1:16) {
  for (j in 1:16) {
    if (!is.na(prob_matrix[i, j]) && is.na(prob_matrix[j, i])) {
      prob_matrix[j, i] <- 1 - prob_matrix[i, j]
    }
  }
}

# Convert to tibble
prob_tibble <- as_tibble(prob_matrix, .name_repair = "minimal")
names(prob_tibble) <- paste0("T", 1:16)
prob_tibble <- mutate(prob_tibble, team = 1:16) |> relocate(team)

# Function to compute win probabilities for each round
generate_probabilities <- function(prob_matrix, round_count) {
  probs <- matrix(0, nrow = 16, ncol = round_count)
  
  # First round is just beating the next opponent (assuming an initial structure)
  for (i in 1:16) {
    probs[i, 1] <- max(prob_matrix[i, ])
  }
  
  # Compute subsequent rounds
  for (r in 2:round_count) {
    for (i in 1:16) {
      probs[i, r] <- sum(prob_matrix[i, ] * probs[, r - 1], na.rm = TRUE)
    }
  }
  
  return(as_tibble(probs) |> mutate(team = 1:16) |> relocate(team))
}

# Compute probabilities for four rounds
fill_prob_matrix <- function(prob_matrix) {
  n <- nrow(prob_matrix)
  
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i == j) {
        prob_matrix[i, j] <- 0.5  # A team vs itself = 50%
      } else if (is.na(prob_matrix[i, j]) && !is.na(prob_matrix[j, i])) {
        prob_matrix[i, j] <- 1 - prob_matrix[j, i]  # Use symmetry
      } else if (is.na(prob_matrix[i, j]) && is.na(prob_matrix[j, i])) {
        prob_matrix[i, j] <- 0.5  # Default for unknown matchups
        prob_matrix[j, i] <- 0.5
      }
    }
  }
  
  return(prob_matrix)
}

# Apply function to your probability matrix
prob_matrix <- fill_prob_matrix(prob_matrix)
colnames(as_tibble(prob_matrix))
prob_matrix
prob_results <- generate_probabilities(prob_matrix, 4)
names(prob_results)[-1] <- c("second", "third", "fourth", "fifth")

# Sort results by the last computed round
prob_results |> arrange(desc(fifth))

