# =========================
# Chicago Crime–Budget Regression: Data Build
# =========================

# ---- STEP 0: Setup ----
# install.packages(c("dplyr","janitor","stringr","tidycensus","tigris","purrr",
#                    "blsAPI","jsonlite","lubridate","tidyr","readr"))

library(dplyr)
library(janitor)
library(stringr)
library(tidycensus)
library(tigris)
library(purrr)
library(blsAPI)
library(jsonlite)
library(lubridate)
library(tidyr)
library(readr)

options(tigris_use_cache = TRUE)

# (Run once per machine or put in ~/.Renviron as CENSUS_API_KEY)
# census_api_key("YOUR_CENSUS_KEY", install = TRUE)

# -------------------------
# Helper(s)
# -------------------------
safe_row_sum <- function(df, cols){
  cols_present <- intersect(cols, names(df))
  if (length(cols_present) == 0) return(rep(0, nrow(df)))
  rowSums(df[cols_present], na.rm = TRUE)
}

# =========================
# STEP 1: Read main file & base cleaning
# =========================

data <- read.csv('data/crime_rate_regression_data.csv', na.strings = c("", "NA", "n/a"))

# 1.1 Remove num_of_crimes if present
if ("num_of_crimes" %in% names(data)) {
  data <- data %>% select(-num_of_crimes)
}

# 1.2 Keep only rows between 2000–2024
data_2000_to_present <- data[(data$year >= 2000) & (data$year < 2025), ]

# 1.3 Clean column names
data_2000_to_present <- clean_names(data_2000_to_present)

# 1.4 Convert known numeric-like character columns to numeric (name-based, safe)
num_like <- c(
  "population","total_budget",
  "dept_planning_devel_allocation","dept_of_housing_allocation",
  "dept_housing_and_econ_devel_allocation","comm_devel_allocation",
  "cpl_building_and_sites_allocation","cpl_building_sites_fin_general",
  "cpl_maintenance_and_ops_allocation","library_allocation",
  "child_youth_services_allocation","human_services_allocation","fam_supp_services_allocation",
  "fleet_and_facil_mgmt_allocation","dept_asset_info_and_services_allocation",
  "arch_construct_energy_mgmt_allocation",
  "cps_total_revenue_millions","cps_state_revenue_millions"
)
data_clean <- data_2000_to_present
existing_num_like <- intersect(num_like, names(data_clean))

data_clean[existing_num_like] <- lapply(data_clean[existing_num_like], function(x) {
  if (is.character(x)) {
    x <- gsub("[\\$,]", "", x)
    x <- trimws(x)
    x[x %in% c("", "NA", "n/a", "N/A", "--")] <- NA
    as.numeric(x)
  } else {
    x
  }
})

# 1.5 Replace local population with live API pull (tidycensus) and join
chi_geoid <- "1714000"  # City of Chicago (place)

# A) Decennial totals (2000, 2010, 2020)
dec_years <- c(2000, 2010, 2020)
dec <- map_dfr(dec_years, ~
                 get_decennial(
                   geography = "place",
                   variables = "P001001", # total population
                   year = .x,
                   state = "IL"
                 ) %>%
                 filter(GEOID == chi_geoid) %>%
                 transmute(year = .x, population = as.numeric(value))
)

# B) ACS 1-year totals (2005..latest ACS 1-year available; adjust if newer)
acs_years <- 2005:2023
acs <- map_dfr(acs_years, ~
                 get_acs(
                   geography = "place",
                   variables = "B01003_001",
                   year = .x,
                   survey = "acs1",
                   state = "IL"
                 ) %>%
                 filter(GEOID == chi_geoid) %>%
                 transmute(year = .x, population = as.numeric(estimate))
)

# C) Annual estimates (PEP), includes recent years like 2024
pep <- get_estimates(
  geography = "place",
  product   = "population",
  state     = "IL",
  time_series = TRUE
) %>%
  filter(GEOID == chi_geoid) %>%
  transmute(year, population = as.numeric(value))

# D) Combine with preference PEP > ACS > Decennial
pop_yearly <- bind_rows(
  pep %>% mutate(source = "pep"),
  acs %>% mutate(source = "acs"),
  dec %>% mutate(source = "decennial")
) %>%
  arrange(year, match(source, c("pep","acs","decennial"))) %>%
  group_by(year) %>% slice(1) %>% ungroup() %>%
  filter(between(year, 2000, 2024)) %>% arrange(year)

# Fill any gaps by linear interpolation (and allow edge extrapolation if needed)
if (any(is.na(pop_yearly$population)) || length(setdiff(2000:2024, pop_yearly$year)) > 0) {
  pop_yearly <- pop_yearly %>%
    complete(year = 2000:2024) %>%
    arrange(year) %>%
    mutate(population = approx(x = year[!is.na(population)],
                               y = population[!is.na(population)],
                               xout = year, method = "linear", rule = 2)$y)
}

# Join into main table (replace any existing population)
data_clean <- data_clean %>%
  select(-any_of("population")) %>%
  left_join(pop_yearly, by = "year")

# 1.6 Convert CPS revenue (millions) to full dollars BEFORE inflation adjust
cps_revenue_cols <- intersect(c("cps_total_revenue_millions", "cps_state_revenue_millions"),
                              names(data_clean))
if (length(cps_revenue_cols) > 0) {
  data_clean[cps_revenue_cols] <- lapply(data_clean[cps_revenue_cols], function(x) x * 1e6)
}

# =========================
# STEP 2: Crime data processing (incident-level)
# =========================

# 2.1 Read in crime and IUCR code data (adjust paths as needed)
crime_data <- read.csv("~/Documents/DataSciencefiles/r_projects/crime_rate_regression/data/Crimes_-_2001_to_Present_20241225.csv")
crime_codes <- read.csv("~/Documents/DataSciencefiles/r_projects/crime_rate_regression/data/IUCR_codes.csv")

# 2.2 Basic checks
# head(crime_data); str(crime_data)
# colSums(is.na(crime_data[c("Year","Case.Number","Primary.Type")]))

# Ensure Date parsed if needed; not required for counts, but nice for QA
# crime_data$Date <- as.POSIXct(crime_data$Date, format = "%m/%d/%Y %I:%M:%S %p")

# 2.3 Keep necessary columns
crime_data_clean <- crime_data %>%
  transmute(
    Year = as.integer(Year),
    Case.Number = as.character(Case.Number),
    Primary.Type,
    IUCR = str_pad(as.character(IUCR), width = 4, pad = "0")
  ) %>%
  filter(!is.na(Year), Year >= 2001, Year <= 2024)

# 2.4 Clean IUCR codes
crime_codes_clean <- crime_codes %>%
  transmute(
    IUCR = str_pad(as.character(IUCR), width = 4, pad = "0"),
    primary_desc = toupper(trimws(PRIMARY.DESCRIPTION)),
    index_code = toupper(trimws(INDEX.CODE))
  )

# 2.5 Join IUCR onto crime
cd <- crime_data_clean %>%
  left_join(crime_codes_clean, by = "IUCR")

# 2.6 Define categories (use exact IUCR text for sexual assault)
violent_crimes <- c(
  "HOMICIDE","CRIM SEXUAL ASSAULT","ROBBERY",
  "AGGRAVATED ASSAULT","AGGRAVATED BATTERY"
)
property_crimes <- c("BURGLARY","THEFT","MOTOR VEHICLE THEFT","ARSON")

# 2.7 Flags
crimes <- cd %>%
  mutate(
    index_flag = index_code == "I",
    violent_flag = index_flag & primary_desc %in% violent_crimes,
    property_flag = index_flag & primary_desc %in% property_crimes,
    homicide_index_flag = index_flag & primary_desc == "HOMICIDE",
    homicide_any_flag   = primary_desc == "HOMICIDE"
  )

# 2.8 Collapse to incident level (one row per Year x Case.Number)
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

# 2.9 Summarise to yearly counts
crime_yearly <- incidents %>%
  group_by(Year) %>%
  summarise(
    crime_all            = n(),
    crime_index_violent  = sum(category == "index_violent"),
    crime_index_property = sum(category == "index_property"),
    crime_index_all      = sum(category %in% c("index_violent","index_property","index_other")),
    crime_nonindex       = sum(category == "nonindex"),
    crime_homicide_index = sum(any_homicide_index),
    crime_homicide_all   = sum(any_homicide_any),
    .groups = "drop"
  )

# 2.10 QA checks
stopifnot(all(crime_yearly$crime_all ==
                crime_yearly$crime_index_all + crime_yearly$crime_nonindex))
stopifnot(all(crime_yearly$crime_homicide_index <= crime_yearly$crime_index_violent))

# 2.11 Join crime counts into main dataset
crime_yearly <- crime_yearly %>% rename(year = Year)
data_clean <- data_clean %>%
  left_join(crime_yearly, by = "year")

# 2.12 Cleanup big intermediates
rm(crime_yearly, crime_data, crime_codes, crime_data_clean,
   crime_codes_clean, cd, crimes, incidents)
gc()

# =========================
# STEP 3: Derived Metrics (rates)
# =========================
data_clean <- data_clean %>%
  mutate(
    rate_all_per_100k             = ifelse(population > 0, (crime_all            / population) * 1e5, NA_real_),
    rate_index_violent_per_100k   = ifelse(population > 0, (crime_index_violent  / population) * 1e5, NA_real_),
    rate_index_property_per_100k  = ifelse(population > 0, (crime_index_property / population) * 1e5, NA_real_),
    rate_nonindex_per_100k        = ifelse(population > 0, (crime_nonindex       / population) * 1e5, NA_real_),
    rate_homicide_all_per_100k    = ifelse(population > 0, (crime_homicide_all   / population) * 1e5, NA_real_),
    rate_homicide_index_per_100k  = ifelse(population > 0, (crime_homicide_index / population) * 1e5, NA_real_)
  ) %>%
  mutate(across(starts_with("rate_"), ~ ifelse(is.finite(.x), .x, NA_real_)))

# =========================
# STEP 4: Inflation Adjustment via BLS API
# =========================

series_id <- "CUUR0000SA0"  # CPI-U, NSA, All items, U.S. city avg
start_year <- 2000
end_year   <- 2024

payload <- list(
  seriesid   = list(series_id),
  startyear  = as.character(start_year),
  endyear    = as.character(end_year)
)

resp <- blsAPI::blsAPI(payload, asJson = TRUE)
bls <- jsonlite::fromJSON(resp)

cpi_monthly <- bls$Results$series$data[[1]] %>%
  transmute(
    year = as.integer(year),
    period,
    month = as.integer(str_remove(period, "M")),
    value = as.numeric(value)
  ) %>%
  filter(!is.na(month))  # keep M01..M12

inflation_data_annual <- cpi_monthly %>%
  group_by(year) %>%
  summarise(annual_cpi = mean(value, na.rm = TRUE), .groups = "drop")

cpi_2024 <- inflation_data_annual %>% filter(year == 2024) %>% pull(annual_cpi)
stopifnot(length(cpi_2024) == 1, !is.na(cpi_2024))

# Only adjust true $ columns (NOT rates or counts)
dollar_cols <- intersect(c(
  "total_budget",
  "dept_planning_devel_allocation","dept_of_housing_allocation",
  "dept_housing_and_econ_devel_allocation","comm_devel_allocation",
  "cpl_building_and_sites_allocation","cpl_building_sites_fin_general",
  "cpl_maintenance_and_ops_allocation","library_allocation",
  "child_youth_services_allocation","human_services_allocation","fam_supp_services_allocation",
  "fleet_and_facil_mgmt_allocation","dept_asset_info_and_services_allocation",
  "arch_construct_energy_mgmt_allocation",
  "cps_total_revenue_millions","cps_state_revenue_millions"
), names(data_clean))

data_inflation_adjusted_clean <- data_clean %>%
  left_join(inflation_data_annual, by = "year") %>%
  mutate(
    across(all_of(dollar_cols), ~ ifelse(!is.na(.x) & !is.na(annual_cpi), .x * (cpi_2024 / annual_cpi), .x))
  ) %>%
  select(-annual_cpi)

# =========================
# STEP 5: Consolidations & Per-capita / Shares
# =========================

# 5.1 Consolidated (inflation-adjusted already)
data_inflation_adjusted_clean <- data_inflation_adjusted_clean %>%
  mutate(
    housing_development = safe_row_sum(cur_data(), c(
      "dept_planning_devel_allocation","dept_of_housing_allocation",
      "dept_housing_and_econ_devel_allocation","comm_devel_allocation"
    )),
    library_total = safe_row_sum(cur_data(), c(
      "cpl_building_and_sites_allocation","cpl_building_sites_fin_general",
      "cpl_maintenance_and_ops_allocation","library_allocation"
    )),
    family_human_services = safe_row_sum(cur_data(), c(
      "child_youth_services_allocation","human_services_allocation","fam_supp_services_allocation"
    )),
    asset_facilities_energy = safe_row_sum(cur_data(), c(
      "fleet_and_facil_mgmt_allocation","dept_asset_info_and_services_allocation",
      "arch_construct_energy_mgmt_allocation"
    ))
  )

# 5.2 Per-capita (skip invalid denominators)
data_inflation_adjusted_clean <- data_inflation_adjusted_clean %>%
  mutate(
    across(
      all_of(setdiff(dollar_cols, "total_budget")),
      ~ ifelse(!is.na(population) & population > 0, .x / population, NA_real_),
      .names = "{.col}_pc"
    )
  )

# 5.3 Share of total (skip invalid denominators)
cols_for_share <- setdiff(dollar_cols, c("total_budget","cps_total_revenue_millions","cps_state_revenue_millions"))
data_inflation_adjusted_clean <- data_inflation_adjusted_clean %>%
  mutate(
    across(
      all_of(cols_for_share),
      ~ ifelse(!is.na(total_budget) & total_budget > 0, .x / total_budget, NA_real_),
      .names = "{.col}_share"
    ),
    cps_state_share = ifelse(
      "cps_total_revenue_millions" %in% names(cur_data()) &&
        "cps_state_revenue_millions" %in% names(cur_data()) &&
        !is.na(cps_total_revenue_millions) & cps_total_revenue_millions > 0,
      cps_state_revenue_millions / cps_total_revenue_millions,
      NA_real_
    ),
    
    # consolidated families, per-capita & shares
    housing_development_pc       = ifelse(population > 0, housing_development / population, NA_real_),
    library_total_pc             = ifelse(population > 0, library_total / population, NA_real_),
    family_human_services_pc     = ifelse(population > 0, family_human_services / population, NA_real_),
    asset_facilities_energy_pc   = ifelse(population > 0, asset_facilities_energy / population, NA_real_),
    
    housing_development_share       = ifelse(total_budget > 0, housing_development / total_budget, NA_real_),
    library_total_share             = ifelse(total_budget > 0, library_total / total_budget, NA_real_),
    family_human_services_share     = ifelse(total_budget > 0, family_human_services / total_budget, NA_real_),
    asset_facilities_energy_share   = ifelse(total_budget > 0, asset_facilities_energy / total_budget, NA_real_)
  )

# =========================
# OUTPUT (optional)
# =========================
# readr::write_csv(data_inflation_adjusted_clean, "data/derived/crime_budget_2000_2024.csv")
# arrow::write_parquet(data_inflation_adjusted_clean, "data/derived/crime_budget_2000_2024.parquet")

# The final modeling table is: data_inflation_adjusted_clean
# Contains: population (from APIs), annual CPI-adjusted $ fields, per-capita and share features, and crime counts/rates.