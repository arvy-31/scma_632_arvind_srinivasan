#Step 1 Set the working directory and verify it
setwd("D:/SCMA632/Assignment/")
print(getwd())  # Print the working directory to verify

#Step 2 Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

#Step 3 List of required libraries
libraries <- c("dplyr", "readr", "readxl", "tidyr", "ggplot2", "BSDA")
lapply(libraries, install_and_load)

#Step 4 Read the CSV file
data <- read.csv("NSSO68.csv")

#Step 5 Filtering for HP
df <- data %>%
  filter(state_1 == "HP")

#Step 6 Display dataset info
cat("Dataset Information:\n")
print(names(df))
print(head(df))
print(dim(df))

#Step 7 Finding missing values
missing_info <- colSums(is.na(df))
cat("Missing Values Information:\n")
print(missing_info)

#Step 8 Subsetting the data
hpnew <- df %>%
  select(state_1, District, Region, Sector, State_Region, Meals_At_Home, gramdal_q, gramwholep_q, gramGT_q, moong_q, masur_q)

#Step 9 Impute missing values with mean for specific columns
impute_with_mean <- function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}

hpnew$Meals_At_Home <- impute_with_mean(hpnew$Meals_At_Home)
hpnew$gramdal_q <- impute_with_mean(hpnew$gramdal_q)
hpnew$gramwholep_q <- impute_with_mean(hpnew$gramwholep_q)
hpnew$gramGT_q <- impute_with_mean(hpnew$gramGT_q)
hpnew$moong_q <- impute_with_mean(hpnew$moong_q)
hpnew$masur_q <- impute_with_mean(hpnew$masur_q)

#Step 10 Finding outliers and removing them
remove_outliers <- function(df, column_name) {
  Q1 <- quantile(df[[column_name]], 0.25)
  Q3 <- quantile(df[[column_name]], 0.75)
  IQR <- Q3 - Q1
  lower_threshold <- Q1 - (1.5 * IQR)
  upper_threshold <- Q3 + (1.5 * IQR)
  df <- subset(df, df[[column_name]] >= lower_threshold & df[[column_name]] <= upper_threshold)
  return(df)
}

outlier_columns <- c("gramdal_q", "gramwholep_q", "gramGT_q")
for (col in outlier_columns) {
  hpnew <- remove_outliers(hpnew, col)
}

#Step 11 Summarize consumption
hpnew$total_consumption <- rowSums(hpnew[, c("gramdal_q", "gramwholep_q", "gramGT_q")], na.rm = TRUE)

#Step 12 Summarize and display top consuming districts and regions
summarize_consumption <- function(group_col) {
  summary <- hpnew %>%
    group_by(across(all_of(group_col))) %>%
    summarise(total = sum(total_consumption)) %>%
    arrange(desc(total))
  return(summary)
}

district_summary <- summarize_consumption("District")
region_summary <- summarize_consumption("Region")

cat("Top Consuming Districts:\n")
print(head(district_summary, 4))
cat("Region Consumption Summary:\n")
print(region_summary)

#Step 13 Rename districts and sectors
district_mapping <- c("2" = "Kangra", "5" = "Mandi", "11" = "Shimla", "9" = "Sholan")
sector_mapping <- c("2" = "URBAN", "1" = "RURAL")

hpnew$District <- as.character(hpnew$District)
hpnew$Sector <- as.character(hpnew$Sector)
hpnew$District <- ifelse(hpnew$District %in% names(district_mapping), district_mapping[hpnew$District], hpnew$District)
hpnew$Sector <- ifelse(hpnew$Sector %in% names(sector_mapping), sector_mapping[hpnew$Sector], hpnew$Sector)

#Step 14 Test for differences in mean consumption between urban and rural
rural <- hpnew %>%
  filter(Sector == "RURAL") %>%
  select(total_consumption)

urban <- hpnew %>%
  filter(Sector == "URBAN") %>%
  select(total_consumption)

z_test_result <- z.test(rural$total_consumption, urban$total_consumption, alternative = "two.sided", mu = 0, sigma.x = 2.56, sigma.y = 2.34, conf.level = 0.95)

if (z_test_result$p.value < 0.05) {
  cat("P value is <", 0.05, ", Therefore we reject the null hypothesis.\n")
  cat("There is a difference between mean consumptions of urban and rural.\n")
} else {
  cat("P value is >=", 0.05, ", Therefore we fail to reject the null hypothesis.\n")
  cat("There is no significant difference between mean consumptions of urban and rural.\n")
}
