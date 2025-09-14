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
library(jsonlite)
library(lubridate)
library(tidyr)
library(readr)

options(tigris_use_cache = TRUE)

# (Run once per machine or put in ~/.Renviron as CENSUS_API_KEY)
#census_api_key("your_key_here", install = TRUE)

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

# 1.1 Remove num_of_crimes if present + drop budget_and_mgmt_allocation as decided
drop_these <- c("num_of_crimes", "budget_and_mgmt_allocation")
data <- data %>% select(-any_of(drop_these))

# 1.2 Keep only rows between 2000–2024
data_2000_to_present <- data[(data$year >= 2000) & (data$year < 2025), ]

# 1.3 Clean column names
data_2000_to_present <- clean_names(data_2000_to_present)

# 1.3b Drop Budget & Mgmt after cleaning (so the cleaned name matches)
data_2000_to_present <- data_2000_to_present %>% select(-any_of("budget_and_mgmt_allocation"))

# 1.4 Convert known numeric-like character columns to numeric (name-based, safe)
num_like <- c(
  "population","total_budget",
  # Group 2 members
  "dept_planning_devel_allocation","dept_of_housing_allocation",
  "dept_housing_and_econ_devel_allocation","comm_devel_allocation",
  # Group 4 members (Library)
  "cpl_building_and_sites_allocation","cpl_building_sites_fin_general",
  "cpl_maintenance_and_ops_allocation","other_ops_expenses_allocation","library_allocation",
  # Group 1 members (Human/Family/Youth)
  "child_youth_services_allocation","human_services_allocation","fam_supp_services_allocation",
  # Group 3 members (Internal Ops/Infrastructure)
  "fleet_and_facil_mgmt_allocation","dept_asset_info_and_services_allocation",
  "arch_construct_energy_mgmt_allocation","envi_and_sustain_allocation",
  # CPS (will be transformed then dropped)
  "cps_total_revenue_millions","cps_state_revenue_millions",
  # Standalone we are keeping separate
  "streets_sanit_allocation","transportation_allocation"
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
readRenviron("~/.Renviron")  # ensure the CENSUS_API_KEY is loaded in this session
chi_geoid <- "1714000"  # City of Chicago (place)

# Helper: pull decennial total population by year
get_dec_pop <- function(y) {
  var <- if (y == 2020) "P1_001N" else "P001001"
  get_decennial(
    geography = "place",
    variables = var,
    year = y,
    state = "IL"
  ) |>
    dplyr::filter(GEOID == chi_geoid) |>
    dplyr::transmute(year = y, population = as.numeric(value))
}

# A) Decennial totals (2000, 2010, 2020) — uses correct var per year
dec_years <- c(2000, 2010, 2020)
dec <- purrr::map_dfr(dec_years, get_dec_pop)

# B) ACS 1-year totals (2005..latest ACS 1-year available; skip 2020, not released)
acs_years <- c(2005:2019, 2021:2023)

acs <- purrr::map_dfr(acs_years, ~
                        get_acs(
                          geography = "place",
                          variables = "B01003_001",
                          year = .x,
                          survey = "acs1",
                          state = "IL"
                        ) |>
                        dplyr::filter(GEOID == chi_geoid) |>
                        dplyr::transmute(year = .x, population = as.numeric(estimate))
)

# C) Annual estimates (PEP), post-2020: pull year-by-year with vintage 2024
pep_years <- 2020:2024

get_pep_year <- function(y) {
  tryCatch({
    get_estimates(
      geography = "place",
      product   = "population",
      state     = "IL",
      variables = "POPESTIMATE",
      vintage   = 2024,
      year      = y
    ) |>
      dplyr::filter(GEOID == chi_geoid) |>
      dplyr::transmute(year = y, population = as.numeric(value))
  }, error = function(e) {
    message(sprintf("Skipping PEP %d (vintage 2024): %s", y, conditionMessage(e)))
    tibble::tibble(year = integer(0), population = numeric(0))
  })
}

pep <- purrr::map_dfr(pep_years, get_pep_year)

# D) Combine with preference PEP > ACS > Decennial
pop_yearly <- dplyr::bind_rows(
  pep |> dplyr::mutate(source = "pep"),
  acs |> dplyr::mutate(source = "acs"),
  dec |> dplyr::mutate(source = "decennial")
) |>
  dplyr::arrange(year, match(source, c("pep","acs","decennial"))) |>
  dplyr::group_by(year) |>
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  dplyr::filter(dplyr::between(year, 2000, 2024)) |>
  dplyr::arrange(year)

# Fill any gaps by linear interpolation
if (any(is.na(pop_yearly$population)) || length(setdiff(2000:2024, pop_yearly$year)) > 0) {
  pop_yearly <- pop_yearly |>
    tidyr::complete(year = 2000:2024) |>
    dplyr::arrange(year) |>
    dplyr::mutate(population = approx(
      x = year[!is.na(population)],
      y = population[!is.na(population)],
      xout = year, method = "linear", rule = 2
    )$y)
}

# Join into main table (replace any existing population)
data_clean <- data_clean |>
  dplyr::select(-dplyr::any_of("population")) |>
  dplyr::left_join(pop_yearly, by = "year")

# 1.6 CPS transformation: create share, then DROP raw CPS revenue columns
if (all(c("cps_total_revenue_millions","cps_state_revenue_millions") %in% names(data_clean))) {
  data_clean <- data_clean %>%
    mutate(
      cps_state_revenue_share = dplyr::if_else(
        !is.na(cps_total_revenue_millions) & cps_total_revenue_millions > 0,
        cps_state_revenue_millions / cps_total_revenue_millions,
        NA_real_
      )
    ) %>%
    select(-c(cps_total_revenue_millions, cps_state_revenue_millions))
}

# =========================
# STEP 2: Crime data processing (incident-level)
# =========================

# 2.1 Read in crime and IUCR code data (adjust paths as needed)
crime_data <- read.csv("~/Documents/DataSciencefiles/r_projects/crime_rate_regression/data/Crimes_-_2001_to_Present_20241225.csv")
crime_codes <- read.csv("~/Documents/DataSciencefiles/r_projects/crime_rate_regression/data/IUCR_codes.csv")

# 2.2 Basic checks (optional)
# head(crime_data); str(crime_data)

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
# STEP 4: Inflation Adjustment via BLS API (httr + jsonlite)
# =========================

library(httr)   # jsonlite is already loaded above

series_id <- "CUUR0000SA0"  # CPI-U, NSA, All items, U.S. city avg
start_year <- 2000
end_year   <- 2024  # request through 2024

# Call BLS API directly
resp <- POST(
  url   = "https://api.bls.gov/publicAPI/v2/timeseries/data/",
  body  = list(
    seriesid  = list(series_id),
    startyear = as.character(start_year),
    endyear   = as.character(end_year)
  ),
  encode = "json"
)
stopifnot(http_type(resp) == "application/json")
bls <- content(resp, as = "parsed", simplifyVector = TRUE)

# Monthly CPI (drop M13 which is the pre-averaged annual point)
cpi_monthly <- as.data.frame(bls$Results$series$data[[1]]) %>%
  dplyr::transmute(
    year   = as.integer(year),
    period = period,
    month  = as.integer(sub("^M", "", period)),
    value  = as.numeric(value)
  ) %>%
  dplyr::filter(!is.na(month))

# Annual average CPI
inflation_data_annual <- cpi_monthly %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(annual_cpi = mean(value, na.rm = TRUE), .groups = "drop")

# Use the most recent CPI year returned as the normalization base
base_year <- max(inflation_data_annual$year, na.rm = TRUE)
cpi_base  <- inflation_data_annual$annual_cpi[inflation_data_annual$year == base_year]
stopifnot(length(cpi_base) == 1, !is.na(cpi_base))
message(sprintf("Inflation-normalizing to %d dollars.", base_year))

# Only adjust true $ columns (NOT rates/counts; CPS raw revenues are dropped elsewhere)
dollar_cols <- intersect(c(
  "total_budget",
  # Group 2
  "dept_planning_devel_allocation","dept_of_housing_allocation",
  "dept_housing_and_econ_devel_allocation","comm_devel_allocation",
  # Group 4 (Library)
  "cpl_building_and_sites_allocation","cpl_building_sites_fin_general",
  "cpl_maintenance_and_ops_allocation","other_ops_expenses_allocation","library_allocation",
  # Group 1 (Human/Family/Youth)
  "child_youth_services_allocation","human_services_allocation","fam_supp_services_allocation",
  # Group 3 (Internal Ops/Infrastructure)
  "fleet_and_facil_mgmt_allocation","dept_asset_info_and_services_allocation",
  "arch_construct_energy_mgmt_allocation","envi_and_sustain_allocation",
  # Standalone
  "streets_sanit_allocation","transportation_allocation",
  # ✅ Police
  "police_allocation"
), names(data_clean))

data_inflation_adjusted_clean <- data_clean %>%
  dplyr::left_join(inflation_data_annual, by = "year") %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(dollar_cols),
      ~ ifelse(!is.na(.x) & !is.na(annual_cpi), .x * (cpi_base / annual_cpi), .x)
    )
  ) %>%
  dplyr::select(-annual_cpi)

# --- Cleanup: enforce our drop/rename decisions ---
# Drop Budget & Mgmt (raw + any derived)
data_inflation_adjusted_clean <- data_inflation_adjusted_clean %>%
  dplyr::select(-tidyr::matches("^budget_and_mgmt_allocation($|_)"))

# Remove any lingering CPS raw revenue columns & their derived variants (if they still exist)
data_inflation_adjusted_clean <- data_inflation_adjusted_clean %>%
  dplyr::select(-tidyr::matches("^cps_(total|state)_revenue_millions($|_)"))

# Standardize CPS share column name (if present)
if ("cps_state_revenue_share" %in% names(data_inflation_adjusted_clean)) {
  data_inflation_adjusted_clean <- data_inflation_adjusted_clean %>%
    dplyr::rename(cps_state_share = cps_state_revenue_share)
}

# =========================
# STEP 5: Consolidations & Per-capita / Shares
# =========================

# 5.1 Consolidated (post-inflation, in real 2024 $)
data_inflation_adjusted_clean <- data_inflation_adjusted_clean %>%
  mutate(
    # Group 2: Planning/Housing/Community Development
    planning_housing_commdev_allocation = safe_row_sum(cur_data(), c(
      "dept_planning_devel_allocation","dept_of_housing_allocation",
      "dept_housing_and_econ_devel_allocation","comm_devel_allocation"
    )),
    # Group 4: Library (includes other_ops_expenses_allocation)
    library_combined_allocation = safe_row_sum(cur_data(), c(
      "cpl_building_and_sites_allocation","cpl_building_sites_fin_general",
      "cpl_maintenance_and_ops_allocation","other_ops_expenses_allocation","library_allocation"
    )),
    # Group 1: Human/Family/Youth Services
    human_family_youth_services_allocation = safe_row_sum(cur_data(), c(
      "child_youth_services_allocation","human_services_allocation","fam_supp_services_allocation"
    )),
    # Group 3: Internal Ops / Infrastructure Services (adds envi_and_sustain)
    internal_ops_infrastructure_allocation = safe_row_sum(cur_data(), c(
      "fleet_and_facil_mgmt_allocation","dept_asset_info_and_services_allocation",
      "arch_construct_energy_mgmt_allocation","envi_and_sustain_allocation"
    ))
  )

# 5.1b Targeted fix: interpolate 2014 only for Planning/Housing/CommDev allocation
# Use your consolidated column name. If you named it differently, adjust target_col.
target_col <- if ("planning_housing_commdev_allocation" %in% names(data_inflation_adjusted_clean)) {
  "planning_housing_commdev_allocation"
} else {
  # fallback to earlier name we used; change if yours differs
  "housing_development"
}

i2014 <- which(data_inflation_adjusted_clean$year == 2014)

if (length(i2014) == 1 && target_col %in% names(data_inflation_adjusted_clean)) {
  val_2014 <- data_inflation_adjusted_clean[[target_col]][i2014]
  if (!is.na(val_2014) && val_2014 == 0) {
    v2013 <- data_inflation_adjusted_clean[[target_col]][data_inflation_adjusted_clean$year == 2013]
    v2015 <- data_inflation_adjusted_clean[[target_col]][data_inflation_adjusted_clean$year == 2015]
    if (!is.na(v2013) && !is.na(v2015)) {
      data_inflation_adjusted_clean[[target_col]][i2014] <- mean(c(v2013, v2015))
    }
  }
}
# 5.2 Per-capita (skip invalid denominators) for RAW dollar columns
data_inflation_adjusted_clean <- data_inflation_adjusted_clean %>%
  mutate(
    across(
      all_of(setdiff(dollar_cols, "total_budget")),
      ~ ifelse(!is.na(population) & population > 0, .x / population, NA_real_),
      .names = "{.col}_pc"
    )
  )

# 5.3 Per-capita & Shares for CONSOLIDATED groups + shares for raw
consolidated_cols <- c(
  "planning_housing_commdev_allocation",
  "library_combined_allocation",
  "human_family_youth_services_allocation",
  "internal_ops_infrastructure_allocation"
)

cols_for_share_raw <- setdiff(dollar_cols, c("total_budget"))

data_inflation_adjusted_clean <- data_inflation_adjusted_clean %>%
  mutate(
    # per-capita for consolidated groups
    across(
      all_of(consolidated_cols),
      ~ ifelse(!is.na(population) & population > 0, .x / population, NA_real_),
      .names = "{.col}_pc"
    ),
    # shares for raw dollar columns
    across(
      all_of(cols_for_share_raw),
      ~ ifelse(!is.na(total_budget) & total_budget > 0, .x / total_budget, NA_real_),
      .names = "{.col}_share"
    ),
    # shares for consolidated groups
    across(
      all_of(consolidated_cols),
      ~ ifelse(!is.na(total_budget) & total_budget > 0, .x / total_budget, NA_real_),
      .names = "{.col}_share"
    )
  )

# Remove any lingering Budget & Mgmt columns (raw/pc/share) just in case
data_inflation_adjusted_clean <- data_inflation_adjusted_clean %>%
  select(-matches("^budget_and_mgmt_allocation($|_)"))

# NOTE: cps_state_revenue_share is already created and retained (raw CPS $ dropped).
# Standalone columns (streets_sanit_allocation, transportation_allocation) remain as-is,
# with _pc and _share created above.

# =========================
# STEP 6: Final Modeling Table
# =========================

# Columns we want to KEEP for modeling
keep_cols <- c(
  "year","population","total_budget",
  
  # grouped allocations (and their per-capita/share versions)
  "planning_housing_commdev_allocation","planning_housing_commdev_allocation_pc","planning_housing_commdev_allocation_share",
  "library_combined_allocation","library_combined_allocation_pc","library_combined_allocation_share",
  "human_family_youth_services_allocation","human_family_youth_services_allocation_pc","human_family_youth_services_allocation_share",
  "internal_ops_infrastructure_allocation","internal_ops_infrastructure_allocation_pc","internal_ops_infrastructure_allocation_share",
  
  # standalone allocations
  "streets_sanit_allocation","streets_sanit_allocation_pc","streets_sanit_allocation_share",
  "transportation_allocation","transportation_allocation_pc","transportation_allocation_share",
  # Police
  "police_allocation","police_allocation_pc","police_allocation_share",
  
  # CPS
  "cps_state_share",
  
  # crime counts and rates
  "crime_all","crime_index_violent","crime_index_property","crime_index_all","crime_nonindex",
  "crime_homicide_index","crime_homicide_all",
  "rate_all_per_100k","rate_index_violent_per_100k","rate_index_property_per_100k",
  "rate_nonindex_per_100k","rate_homicide_all_per_100k","rate_homicide_index_per_100k"
)

model_data <- data_inflation_adjusted_clean %>%
  select(any_of(keep_cols))

# Preview
glimpse(model_data)

# Optional: save
# readr::write_csv(model_data, "data/derived/model_data_2000_2024.csv")
# arrow::write_parquet(model_data, "data/derived/model_data_2000_2024.parquet")

# =========================
# OUTPUT (optional)
# =========================
# readr::write_csv(data_inflation_adjusted_clean, "data/derived/crime_budget_2000_2024.csv")
# arrow::write_parquet(data_inflation_adjusted_clean, "data/derived/crime_budget_2000_2024.parquet")