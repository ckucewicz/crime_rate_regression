# installing necessary packages
#install.packages('dplyr')
library(dplyr)
library(janitor)

# reading in data file
data <- read.csv('data/crime_rate_regression_data.csv', na.strings = c("", "NA", "n/a"))


# ---- STEP 1: Data Cleaning ----

# --- 1.1 Drop rows before 2000 or after 2024 ---
data_2000_to_present <- data[(data$year >=2000) & (data$year <2025), ]


# --- 1.2 Clean column names ---
data_2000_to_present <- clean_names(data_2000_to_present)


# --- 1.3 Convert character-formatted numeric columns to actual numeric (remove $ and ,)
#      - Includes population, crime counts, budget allocations, and CPS revenue columns

# make a copy
data_clean <- data_2000_to_present

# identify character cols only
char_cols <- sapply(data_clean, is.character)

# Remove $ and commas from every character column and convert to numeric
data_clean[char_cols] <- lapply(data_clean[char_cols], function(x) {
  x <- gsub("[\\$,]", "", x)
  x <- trimws(x)
  x[x %in% c("", "NA", "n/a", "N/A", "--")] <- NA
  as.numeric(x)
})

# --- 1.4 Interpolate missing population values between census years

# (x,y) need to return rows where population is known
known_data <- data_clean[!is.na(data_clean$population), c('year', 'population')]

# interpolating population counts
interp <- approx(
  x = known_data$year,
  y = known_data$population,
  xout = data_clean$year,
  method = "linear"
)

# apply interpolated values (and preserve known values where available)
data_clean$population <- ifelse(
  is.na(data_clean$population),
  interp$y,
  data_clean$population
)

# --- 1.5 Convert CPS revenue (originally in millions) to full dollars ---
cps_revenue_cols <- c("cps_total_revenue_millions", "cps_state_revenue_millions")

data_clean[cps_revenue_cols] <- lapply(data_clean[cps_revenue_cols], function(x) x*1e6)

# --- 1.7 Adjust dollar amounts for inflation
#      - Convert all dollar-based columns to constant dollars (e.g., 2024 USD)
#      - Use CPI or GDP deflator from BLS or FRED


# Read in crime data
# Adjust dollar amounts for inflation (to 2024 USD)
# Normalize budget variables (as share of total budget)
# Calculate crime rates per 100k population
