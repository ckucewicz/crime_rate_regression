# ---- STEP 0: Setup ----
# --- 0.1 Install/load necessary packages ---
# install.packages('dplyr')
library(dplyr)
library(janitor)
library(stringr)

# --- 0.2 Read in main data file ---
data <- read.csv('data/crime_rate_regression_data.csv', na.strings = c("", "NA", "n/a"))

# ---- STEP 1: Data Cleaning ----

# --- 1.1 Remove `num_of_crimes` column ---
data <- data %>% 
  select(-num_of_crimes)

# --- 1.2 Keep only rows between 2000–2024 ---
data_2000_to_present <- data[(data$year >= 2000) & (data$year < 2025), ]

# --- 1.3 Clean column names ---
data_2000_to_present <- clean_names(data_2000_to_present)

# --- 1.4 Convert character-formatted numeric columns to numeric ---
#      - Includes population, crime counts, budget allocations, CPS revenue
data_clean <- data_2000_to_present
char_cols <- sapply(data_clean, is.character)
data_clean[char_cols] <- lapply(data_clean[char_cols], function(x) {
  x <- gsub("[\\$,]", "", x)
  x <- trimws(x)
  x[x %in% c("", "NA", "n/a", "N/A", "--")] <- NA
  as.numeric(x)
})

# --- 1.5 Interpolate missing population values between census years ---
known_data <- data_clean[!is.na(data_clean$population), c('year', 'population')]
interp <- approx(
  x = known_data$year,
  y = known_data$population,
  xout = data_clean$year,
  method = "linear"
)
data_clean$population <- ifelse(
  is.na(data_clean$population),
  interp$y,
  data_clean$population
)

# --- 1.6 Convert CPS revenue (millions) to full dollars ---
cps_revenue_cols <- c("cps_total_revenue_millions", "cps_state_revenue_millions")
data_clean[cps_revenue_cols] <- lapply(data_clean[cps_revenue_cols], function(x) x * 1e6)

# ---- STEP 2: Crime Data Processing ----

# --- 2.1 Read in crime and IUCR code data ---
crime_data <- read.csv("~/Documents/DataSciencefiles/r_projects/crime_rate_regression/data/Crimes_-_2001_to_Present_20241225.csv")
crime_codes <- read.csv("~/Documents/DataSciencefiles/r_projects/crime_rate_regression/data/IUCR_codes.csv")

# --- 2.2 Inspect and basic cleaning ---
head(crime_data)
str(crime_data)
crime_data %>%
  summarize(across(c(Year, Case.Number, Primary.Type), ~ sum(is.na(.))))
crime_data$Date <- as.POSIXct(crime_data$Date, format = "%m/%d/%Y %I:%M:%S %p")
min(crime_data$Date, na.rm = TRUE)
max(crime_data$Date, na.rm = TRUE)

# --- 2.3 Keep necessary columns in crime_data ---
crime_data_clean <- crime_data %>%
  transmute(
    Year = as.integer(Year),
    Case.Number = as.character(Case.Number),
    Primary.Type,
    IUCR = str_pad(as.character(IUCR), width = 4, pad = "0")
  ) %>%
  filter(!is.na(Year), Year >= 2001, Year <= 2024)

# --- 2.4 Keep necessary columns in crime_codes ---
crime_codes_clean <- crime_codes %>%
  transmute(
    IUCR = str_pad(as.character(IUCR), width = 4, pad = "0"),
    primary_desc = toupper(trimws(PRIMARY.DESCRIPTION)),
    index_code = toupper(trimws(INDEX.CODE))
  )

# --- 2.5 Join IUCR codes onto crime data ---
cd <- crime_data_clean %>%
  left_join(crime_codes_clean, by = "IUCR")

# --- 2.6 Define violent and property crime categories ---
violent_crimes <- c(
  "HOMICIDE", "CRIMINAL SEXUAL ASSAULT", "ROBBERY",
  "AGGRAVATED ASSAULT", "AGGRAVATED BATTERY"
)
property_crimes <- c(
  "BURGLARY", "THEFT", "MOTOR VEHICLE THEFT", "ARSON"
)

# --- 2.7 Flag crime categories ---
crimes <- cd %>%
  mutate(
    index_flag = index_code == "I",
    violent_flag = index_flag & primary_desc %in% violent_crimes,
    property_flag = index_flag & primary_desc %in% property_crimes,
    homicide_index_flag = index_flag & primary_desc == "HOMICIDE",
    homicide_any_flag   = primary_desc == "HOMICIDE"
  )

# --- 2.8 Collapse to incident level ---
incidents <- crimes %>%
  group_by(Year, Case.Number) %>%
  summarise(
    any_index_violent   = any(violent_flag, na.rm = TRUE),
    any_index_property  = any(property_flag, na.rm = TRUE),
    any_index_any       = any(index_flag, na.rm = TRUE),
    any_homicide_index  = any(homicide_index_flag, na.rm = TRUE),
    any_homicide_any    = any(homicide_any_flag, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    category = dplyr::case_when(
      any_index_violent  ~ "index_violent",
      any_index_property ~ "index_property",
      any_index_any      ~ "index_other",
      TRUE               ~ "nonindex"
    )
  )

# --- 2.9 Summarise to yearly counts ---
crime_yearly <- incidents %>%
  group_by(Year) %>%
  summarise(
    crime_all            = n(),
    crime_index_violent  = sum(category == "index_violent"),
    crime_index_property = sum(category == "index_property"),
    crime_index_all      = sum(category %in% c("index_violent", "index_property", "index_other")),
    crime_nonindex       = sum(category == "nonindex"),
    crime_homicide_index = sum(any_homicide_index),
    crime_homicide_all   = sum(any_homicide_any),
    .groups = "drop"
  )

# --- 2.10 QA checks on counts ---
stopifnot(all(crime_yearly$crime_all ==
                crime_yearly$crime_index_all + crime_yearly$crime_nonindex))
stopifnot(all(crime_yearly$crime_homicide_index <= crime_yearly$crime_index_violent))

# --- 2.11 Join crime counts into main dataset ---
crime_yearly <- crime_yearly %>%
  rename(year = Year)
data_clean <- data_clean %>%
  left_join(crime_yearly, by = "year")

# --- 2.12 Remove large intermediates ---
rm(crime_yearly, crime_data, crime_codes, crime_data_clean,
   crime_codes_clean, cd, crimes, incidents)
gc()

# ---- STEP 3: Derived Metrics ----

# --- 3.1 Calculate crime rates per 100k population ---
data_clean <- data_clean %>%
  mutate(
    rate_all_per_100k = (crime_all / population) * 1e5,
    rate_index_violent_per_100k = (crime_index_violent / population) * 1e5,
    rate_index_property_per_100k = (crime_index_property / population) * 1e5,
    rate_nonindex_per_100k = (crime_nonindex / population) * 1e5,
    rate_homicide_all_per_100k = (crime_homicide_all / population) * 1e5,
    rate_homicide_index_per_100k = (crime_homicide_index / population) * 1e5
  )

# ---- STEP 4: Inflation Adjustment ----

# --- 4.1 Read in CPI data ---
inflation_data <- read.csv("~/Documents/DataSciencefiles/r_projects/crime_rate_regression/data/Inflation Data - BLS Data Series.csv")

# --- 4.2 Keep annual CPI and rename for clarity ---
inflation_data_annual <- inflation_data %>%
  select(Year, Annual) %>%
  rename(year = Year, annual_cpi = Annual)

# --- 4.3 Extract CPI for 2024 ---
cpi_2024 <- inflation_data_annual %>%
  filter(year == 2024) %>%
  pull(annual_cpi)
stopifnot(length(cpi_2024) == 1, !is.na(cpi_2024))

# --- 4.4 Join CPI to main dataset ---
data_inflation_adjusted_clean <- data_clean %>%
  left_join(inflation_data_annual, by = "year")

# --- 4.5 Adjust dollar columns to 2024 USD ---
cols_to_adjust <- names(data_inflation_adjusted_clean)[3:27]
data_inflation_adjusted_clean <- data_inflation_adjusted_clean %>%
  mutate(
    across(
      all_of(cols_to_adjust),
      ~ .x * (cpi_2024 / annual_cpi)
    )
  ) %>%
  select(-annual_cpi)

# --- 4.6 Create per-capita budget variables (skip NAs) ---
dept_cols <- setdiff(cols_to_adjust, c("total_budget"))
data_inflation_adjusted_clean <- data_inflation_adjusted_clean %>%
  mutate(
    across(
      all_of(dept_cols),
      ~ ifelse(!is.na(.x) & !is.na(population) & population > 0, .x / population, NA_real_),
      .names = "{.col}_pc"
    )
  )

# --- 4.7 Create share-of-total budget variables (skip NAs) ---
dept_cols_for_share <- setdiff(
  cols_to_adjust,
  c("total_budget", "cps_total_revenue_millions", "cps_state_revenue_millions")
)
data_inflation_adjusted_clean <- data_inflation_adjusted_clean %>%
  mutate(
    across(
      all_of(dept_cols_for_share),
      ~ ifelse(!is.na(.x) & !is.na(total_budget) & total_budget > 0, .x / total_budget, NA_real_),
      .names = "{.col}_share"
    ),
    cps_state_share = ifelse(
      !is.na(cps_state_revenue_millions) & !is.na(cps_total_revenue_millions) & cps_total_revenue_millions > 0,
      cps_state_revenue_millions / cps_total_revenue_millions,
      NA_real_
    )
  )