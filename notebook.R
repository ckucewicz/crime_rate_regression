# installing necessary packages
#install.packages('dplyr')
library(dplyr)
library(janitor)
library(stringr)

# reading in data file
data <- read.csv('data/crime_rate_regression_data.csv', na.strings = c("", "NA", "n/a"))


# ---- STEP 1: Data Cleaning ----

# --- 1.1 remove num of crimes column from data_clean
data <- data %>% 
  select(-num_of_crimes)
# --- 1.2 Drop rows before 2000 or after 2024 ---
data_2000_to_present <- data[(data$year >=2000) & (data$year <2025), ]


# --- 1.3 Clean column names ---
data_2000_to_present <- clean_names(data_2000_to_present)


# --- 1.4 Convert character-formatted numeric columns to actual numeric (remove $ and ,)
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

# --- 1.5 Interpolate missing population values between census years

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

# --- 1.6 Convert CPS revenue (originally in millions) to full dollars ---
cps_revenue_cols <- c("cps_total_revenue_millions", "cps_state_revenue_millions")

data_clean[cps_revenue_cols] <- lapply(data_clean[cps_revenue_cols], function(x) x*1e6)

# --- 1.7 Include crime counts from crime dataset 

# Read in crime data
crime_data <- read.csv("~/Documents/DataSciencefiles/r_projects/crime_rate_regression/data/Crimes_-_2001_to_Present_20241225.csv")

# read in Illinois uniform crime reporting codes
crime_codes <- read.csv("~/Documents/DataSciencefiles/r_projects/crime_rate_regression/data/IUCR_codes.csv")
# crime data

# inspects the first 6 rows of the data set
head(crime_data)

# inspects the structure of the data set
str(crime_data)

# calculates the number of null values in the Year, Case.Number, and Primary.Type columns
crime_data %>%
  summarize(across(c(Year, Case.Number, Primary.Type), ~ sum(is.na(.))))

# formats date column appropriately, then returns the earliest and latest observations of the data set
crime_data$Date <- as.POSIXct(crime_data$Date, format = "%m/%d/%Y %I:%M:%S %p")

min(crime_data$Date, na.rm = TRUE)

max(crime_data$Date, na.rm = TRUE)

## remove unnecessary columns from crime_data
crime_data_clean <- crime_data %>%
  transmute(
    Year = as.integer(Year),
    Case.Number = as.character(Case.Number),
    Primary.Type,
    IUCR = str_pad(as.character(IUCR), width = 4, pad = "0")
  ) %>%
  filter(!is.na(Year), Year >= 2001, Year <= 2024)

# remove unecessary columns from crime_codes
crime_codes_clean <- crime_codes %>%
  transmute(
    IUCR = str_pad(as.character(IUCR), width = 4, pad = "0"),
    primary_desc = toupper(trimws(PRIMARY.DESCRIPTION)),
    index_code = toupper(trimws(INDEX.CODE))
  )

# join codes onto crime_data
cd <- crime_data_clean %>%
  left_join(crime_codes_clean, by = "IUCR")

# define sets for violent/property (subtypes of Index crimes)
violent_crimes <-c("HOMICIDE", 
                   "CRIMINAL SEXUAL ASSAULT", 
                   "ROBBERY",
                   "AGGRAVATED ASSAULT",
                   "AGGRAVATED BATTERY"
                   )

property_crimes <-c("BURGLARY",
                    "THEFT",
                    "MOTOR VEHICLE THEFT",
                    "ARSON"
                    )

crimes <- cd %>%
  mutate(
    index_flag = index_code == "I",
    violent_flag = index_flag & primary_desc %in% violent_crimes,
    property_flag = index_flag & primary_desc %in% property_crimes
  )

# Collapse to incident level (unique Case.Number within Year) with a priority rule: 
# violent > property > non-index

incidents <- crimes %>%
  group_by(Year, Case.Number) %>%
  summarize(
    any_index_violent = any(violent_flag, na.rm = TRUE),
    any_index_property = any(property_flag, na.rm = TRUE),
    any_index_any = any(index_flag, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    category = dplyr::case_when(
      any_index_violent ~ "index_violent",
      any_index_property ~ "index_property",
      any_index_any ~ "index_other",
      TRUE ~ "nonindex"
    )
  )

# Yearly counts for the 5 new columns
crime_yearly <- incidents %>%
  group_by(Year) %>%
  summarize(
    crime_all = n(),
    crime_index_violent = sum(category == "index_violent"),
    crime_index_property = sum(category == "index_property"),
    crime_index_all = sum(category %in% c("index_violent", "index_property", "index_other")),
    crime_nonindex = sum(category == "nonindex"),
    .groups = "drop"
  )

# QA - will error if something is off
stopifnot(all(crime_yearly$crime_all == 
            crime_yearly$crime_index_all + crime_yearly$crime_nonindex))

# merge columns into data_clean
data_clean <- data_clean %>%
  left_join(crime_yearly, by = c("year" = "Year"))

# remove big intermediate dataset in order to free up memory
rm(crime_data, crime_codes, crime_data_clean, crime_codes_clean, cd, crimes, incidents)

# force garbage collection
gc()


# Adjust dollar amounts for inflation (to 2024 USD)
# Normalize budget variables (as share of total budget)
# Calculate crime rates per 100k population
