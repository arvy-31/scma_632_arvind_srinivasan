# Install necessary packages if not already installed
if (!requireNamespace("stringr", quietly = TRUE)) {
  install.packages("stringr")
}

install.packages("stringdist")
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(stringdist)
library(lubridate)

# Set working directory
setwd("D:/SCMA632/A1b")

# Load data
ipl_bbb <- read_csv("IPL_ball_by_ball_updated till 2024.csv")
ipl_salary <- read_excel("IPL_SALARIES_2024.xlsx")

# Group data
grouped_data <- ipl_bbb %>%
  group_by(Season, `Innings No`, Striker, Bowler) %>%
  summarise(runs_scored = sum(runs_scored, na.rm = TRUE),
            wicket_confirmation = sum(wicket_confirmation, na.rm = TRUE)) %>%
  ungroup()

player_runs <- grouped_data %>%
  group_by(Season, Striker) %>%
  summarise(runs_scored = sum(runs_scored, na.rm = TRUE)) %>%
  ungroup()

player_wickets <- grouped_data %>%
  group_by(Season, Bowler) %>%
  summarise(wicket_confirmation = sum(wicket_confirmation, na.rm = TRUE)) %>%
  ungroup()

top_run_getters <- player_runs %>%
  group_by(Season) %>%
  top_n(3, runs_scored) %>%
  ungroup()

bottom_wicket_takers <- player_wickets %>%
  group_by(Season) %>%
  top_n(3, wicket_confirmation) %>%
  ungroup()

print("Top Three Run Getters:")
print(top_run_getters)
print("Top Three Wicket Takers:")
print(bottom_wicket_takers)

ipl_bbb <- ipl_bbb %>%
  mutate(year = year(dmy(Date)))

total_run_each_year <- ipl_bbb %>%
  group_by(year, Striker) %>%
  summarise(runs_scored = sum(runs_scored, na.rm = TRUE)) %>%
  arrange(year, desc(runs_scored)) %>%
  ungroup()

list_top_batsman_last_three_year <- list()
for (i in unique(total_run_each_year$year)[1:3]) {
  list_top_batsman_last_three_year[[as.character(i)]] <- total_run_each_year %>%
    filter(year == i) %>%
    top_n(3, runs_scored) %>%
    pull(Striker)
}

print(list_top_batsman_last_three_year)

total_wicket_each_year <- ipl_bbb %>%
  group_by(year, Bowler) %>%
  summarise(wicket_confirmation = sum(wicket_confirmation, na.rm = TRUE)) %>%
  arrange(year, desc(wicket_confirmation)) %>%
  ungroup()

list_top_bowler_last_three_year <- list()
for (i in unique(total_wicket_each_year$year)[1:3]) {
  list_top_bowler_last_three_year[[as.character(i)]] <- total_wicket_each_year %>%
    filter(year == i) %>%
    top_n(3, wicket_confirmation) %>%
    pull(Bowler)
}

print(list_top_bowler_last_three_year)

# Install 'fuzzyjoin' package if not already installed
if(!require(fuzzyjoin)) install.packages('fuzzyjoin')
library(fuzzyjoin)

# Filter for 2024 data
total_runs_2024 <- total_run_each_year %>% filter(year == 2024)
total_wickets_2024 <- total_wicket_each_year %>% filter(year == 2024)

# Merge runs and wickets into a single dataframe
performance_2024 <- total_runs_2024 %>%
  full_join(total_wickets_2024, by = c("year", "Striker" = "Bowler")) %>%
  mutate(total_performance = ifelse(is.na(runs_scored), 0, runs_scored) +
           ifelse(is.na(wicket_confirmation), 0, wicket_confirmation))

# Fuzzy matching function
match_names <- function(name, names_list) {
  matched <- stringdist::stringdistmatrix(name, names_list, method = "jw")
  match <- names_list[which.min(matched)]
  score <- min(matched)
  if (score <= 0.2) {  # Use a threshold score of 0.2
    return(match)
  } else {
    return(NA)
  }
}

# Create a new column in df_salary with matched names from performance_2024
df_salary <- ipl_salary %>%
  rowwise() %>%
  mutate(Matched_Player = match_names(Player, performance_2024$Striker))

# Merge the DataFrames on the matched names
df_merged <- df_salary %>%
  left_join(performance_2024, by = c("Matched_Player" = "Striker"))

# Calculate the correlation
correlation <- cor(df_merged$Rs, df_merged$total_performance, use = "complete.obs")
print(paste("Correlation between Salary and Total Performance:", correlation))

# Specific analysis for Krunal Pandya
player_name_in_bbb <- "KH Pandya"
player_name_in_salary <- "Krunal Pandya"

# Filter for the specific player
player_performance <- performance_2024 %>%
  filter(Striker == player_name_in_bbb) %>%
  pull(total_performance)

player_salary <- df_salary %>%
  filter(Player == player_name_in_salary) %>%
  pull(Rs)

print(paste("Total Performance (Runs + Wickets) of", player_name_in_salary, "in 2024:", player_performance))
print(paste("Salary of", player_name_in_salary, "in 2024:", player_salary))
