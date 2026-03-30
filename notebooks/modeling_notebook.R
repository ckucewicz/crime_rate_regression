# =========================================================
# --- Modeling Notebook ---
# =========================================================

library(tidyverse)
library(broom)      # tidy(), glance()
library(car)        # vif
library(lmtest)     # bptest (used in analysis notebook; loaded here for completeness)

# =========================================================
# Setup: predictor sets and outcomes
# Mirror definitions from analysis notebook.
# NOTE: internal_ops_infrastructure_allocation_share is omitted from share_predictors
# as the reference category to avoid perfect multicollinearity (shares sum to 1).
# It IS included in pc_predictors because that constraint doesn't apply there.
# =========================================================

share_predictors <- c(
  "police_allocation_share",
  "streets_sanit_allocation_share",
  "transportation_allocation_share",
  "planning_housing_commdev_allocation_share",
  "library_combined_allocation_share",
  "human_family_youth_services_allocation_share",
  "cps_state_share"
)

pc_predictors <- c(
  "police_allocation_pc",
  "streets_sanit_allocation_pc",
  "transportation_allocation_pc",
  "planning_housing_commdev_allocation_pc",
  "library_combined_allocation_pc",
  "human_family_youth_services_allocation_pc",
  "internal_ops_infrastructure_allocation_pc",
  "cps_state_share"
)

crime_outcomes <- c(
  "rate_all_per_100k",
  "rate_index_violent_per_100k",
  "rate_index_property_per_100k",
  "rate_nonindex_per_100k",
  "rate_homicide_all_per_100k",
  "rate_homicide_index_per_100k"
)

outcome_labels <- c(
  rate_all_per_100k            = "All Crime",
  rate_index_violent_per_100k  = "Index Violent",
  rate_index_property_per_100k = "Index Property",
  rate_nonindex_per_100k       = "Non-Index",
  rate_homicide_all_per_100k   = "Homicide (All)",
  rate_homicide_index_per_100k = "Homicide (Index)"
)

# =========================================================
# Helper: fit OLS and return tidy results
# Note: Newey-West SEs not needed — Durbin-Watson test (DW = 1.79, p = 0.13)
# showed no significant autocorrelation, so standard OLS inference is valid.
# =========================================================

fit_ols <- function(outcome, predictors, data, spec_label) {
  df <- data %>%
    select(all_of(c(outcome, predictors))) %>%
    filter(complete.cases(.))
  
  formula <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
  model   <- lm(formula, data = df)
  
  list(
    model   = model,
    spec    = spec_label,
    outcome = outcome,
    glance  = glance(model),
    tidy    = tidy(model)
  )
}

# =========================================================
# Step 1A: Bivariate models — PRIMARY analysis
# One predictor at a time; avoids multicollinearity entirely.
# Preferred given high VIFs (15.8, 10.0, 9.2) in the full model
# and the small sample size (n ≈ 24).
# =========================================================

bivariate_share <- map_dfr(share_predictors, function(pred) {
  map_dfr(crime_outcomes, function(outcome) {
    df <- model_data %>%
      select(all_of(c(outcome, pred))) %>%
      filter(complete.cases(.))
    
    model <- lm(as.formula(paste(outcome, "~", pred)), data = df)
    
    tidy(model) %>%
      filter(term != "(Intercept)") %>%
      mutate(
        outcome   = outcome,
        predictor = pred,
        r_squared = summary(model)$r.squared,
        adj_r_squared = summary(model)$adj.r.squared,
        spec      = "share"
      )
  })
})

bivariate_pc <- map_dfr(pc_predictors, function(pred) {
  map_dfr(crime_outcomes, function(outcome) {
    df <- model_data %>%
      select(all_of(c(outcome, pred))) %>%
      filter(complete.cases(.))
    
    model <- lm(as.formula(paste(outcome, "~", pred)), data = df)
    
    tidy(model) %>%
      filter(term != "(Intercept)") %>%
      mutate(
        outcome       = outcome,
        predictor     = pred,
        r_squared     = summary(model)$r.squared,
        adj_r_squared = summary(model)$adj.r.squared,
        spec          = "per_capita"
      )
  })
})

# --- View bivariate results ---

# All share results sorted by R-squared
bivariate_share %>%
  select(predictor, outcome, estimate, std.error, statistic, p.value, r_squared) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  arrange(desc(r_squared)) %>%
  print(n = Inf)

# All per-capita results sorted by R-squared
bivariate_pc %>%
  select(predictor, outcome, estimate, std.error, statistic, p.value, r_squared) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  arrange(desc(r_squared)) %>%
  print(n = Inf)

# Significant results only (p < 0.05), shares
bivariate_share %>%
  filter(p.value < 0.05) %>%
  select(predictor, outcome, estimate, p.value, r_squared) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  arrange(desc(r_squared))

# Significant results only (p < 0.05), per capita
bivariate_pc %>%
  filter(p.value < 0.05) %>%
  select(predictor, outcome, estimate, p.value, r_squared) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  arrange(desc(r_squared))

# =========================================================
# Step 1B: Full multivariate models — SECONDARY / robustness check
# Interpret with caution: VIF flags police, library, and
# human services shares as severely collinear in the share spec.
# =========================================================

# --- 1.1 Share of budget models ---
share_models <- map(
  crime_outcomes,
  ~ fit_ols(.x, share_predictors, model_data, "share")
)
names(share_models) <- crime_outcomes

# --- 1.2 Per capita models ---
pc_models <- map(
  crime_outcomes,
  ~ fit_ols(.x, pc_predictors, model_data, "per_capita")
)
names(pc_models) <- crime_outcomes

# =========================================================
# Step 2: Model Evaluation
# =========================================================

# --- 2.1 Goodness-of-fit summary table (multivariate) ---

gof_table <- bind_rows(
  map_dfr(share_models, ~ .x$glance %>% mutate(outcome = .x$outcome, spec = "share")),
  map_dfr(pc_models,    ~ .x$glance %>% mutate(outcome = .x$outcome, spec = "per_capita"))
) %>%
  select(spec, outcome, r.squared, adj.r.squared, sigma, AIC, BIC, df.residual) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  arrange(spec, outcome)

print(gof_table)

# --- 2.2 Coefficient tables (multivariate) ---

walk(crime_outcomes, function(outcome) {
  cat("\n\n====", outcome_labels[outcome], "| Share Model ====\n")
  print(share_models[[outcome]]$tidy %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
})

walk(crime_outcomes, function(outcome) {
  cat("\n\n====", outcome_labels[outcome], "| Per Capita Model ====\n")
  print(pc_models[[outcome]]$tidy %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
})

# --- 2.3 Effect size: standardized coefficients (beta weights) ---
# Standardize all variables to z-scores so coefficients are
# directly comparable across predictors regardless of units.

fit_standardized <- function(outcome, predictors, data) {
  df <- data %>%
    select(all_of(c(outcome, predictors))) %>%
    filter(complete.cases(.)) %>%
    mutate(across(everything(), scale))
  
  formula <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
  lm(formula, data = df)
}

share_std_models <- map(crime_outcomes, ~ fit_standardized(.x, share_predictors, model_data))
names(share_std_models) <- crime_outcomes

pc_std_models <- map(crime_outcomes, ~ fit_standardized(.x, pc_predictors, model_data))
names(pc_std_models) <- crime_outcomes

# --- 2.3b Heatmap of standardized coefficients (shares) ---
# Useful visualization for README Key Findings section

std_coefs_share <- map_dfr(crime_outcomes, function(outcome) {
  tidy(share_std_models[[outcome]]) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      outcome        = outcome_labels[outcome],
      significant    = p.value < 0.05
    )
})

ggplot(std_coefs_share, aes(x = outcome, y = term, fill = estimate)) +
  geom_tile(color = "white") +
  geom_text(aes(
    label = paste0(round(estimate, 2), if_else(significant, "*", ""))
  ), size = 3) +
  scale_fill_gradient2(
    low = "steelblue", mid = "white", high = "firebrick",
    midpoint = 0, name = "Std. Coef."
  ) +
  labs(
    title    = "Standardized Coefficients: Budget Shares → Crime Rates",
    subtitle = "Reference category: Internal Ops & Infrastructure | * p < 0.05",
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Same heatmap for per-capita spec
std_coefs_pc <- map_dfr(crime_outcomes, function(outcome) {
  tidy(pc_std_models[[outcome]]) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      outcome     = outcome_labels[outcome],
      significant = p.value < 0.05
    )
})

ggplot(std_coefs_pc, aes(x = outcome, y = term, fill = estimate)) +
  geom_tile(color = "white") +
  geom_text(aes(
    label = paste0(round(estimate, 2), if_else(significant, "*", ""))
  ), size = 3) +
  scale_fill_gradient2(
    low = "steelblue", mid = "white", high = "firebrick",
    midpoint = 0, name = "Std. Coef."
  ) +
  labs(
    title    = "Standardized Coefficients: Per Capita Spending → Crime Rates",
    subtitle = "* p < 0.05",
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# --- 2.4 Diagnostic plots for multivariate models ---

walk(crime_outcomes, function(outcome) {
  par(mfrow = c(2, 2))
  plot(share_models[[outcome]]$model,
       main = paste("Share |", outcome_labels[outcome]))
  par(mfrow = c(1, 1))
})

# --- 2.5 VIF check on multivariate share models ---
# Reminder: police (15.8), library (10.0), human services (9.2) are flagged.
# This is why bivariate models in Step 1A are the primary results.

walk(crime_outcomes, function(outcome) {
  cat("\nVIF —", outcome_labels[outcome], "(share):\n")
  print(round(vif(share_models[[outcome]]$model), 2))
})

# =========================================================
# Step 3: Sensitivity check — controlling for year trend
# If budget-crime associations survive after controlling for year,
# they are more credible. If they collapse, the time trend
# was the primary driver.
# =========================================================

bivariate_share_year <- map_dfr(share_predictors, function(pred) {
  map_dfr(crime_outcomes, function(outcome) {
    df <- model_data %>%
      select(all_of(c(outcome, pred, "year"))) %>%
      filter(complete.cases(.))
    
    model <- lm(as.formula(paste(outcome, "~", pred, "+ year")), data = df)
    
    tidy(model) %>%
      filter(term == pred) %>%
      mutate(
        outcome       = outcome,
        predictor     = pred,
        r_squared     = summary(model)$r.squared,
        adj_r_squared = summary(model)$adj.r.squared,
        spec          = "share_year_controlled"
      )
  })
})

# Compare R² and significance before/after year control
comparison <- bind_rows(
  bivariate_share %>% mutate(spec = "share_no_year"),
  bivariate_share_year %>% mutate(spec = "share_year_controlled")
) %>%
  select(spec, predictor, outcome, estimate, p.value, r_squared) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  arrange(predictor, outcome, spec)

# Show cases where significance changes after controlling for year
comparison %>%
  pivot_wider(
    id_cols = c(predictor, outcome),
    names_from = spec,
    values_from = c(p.value, r_squared, estimate)
  ) %>%
  mutate(
    significance_change = case_when(
      p.value_share_no_year < 0.05 & p.value_share_year_controlled >= 0.05 ~ "lost significance",
      p.value_share_no_year >= 0.05 & p.value_share_year_controlled < 0.05 ~ "gained significance",
      TRUE ~ "unchanged"
    )
  ) %>%
  arrange(significance_change, predictor) %>%
  print(n = Inf)