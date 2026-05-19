# =========================================================
# --- Modeling Notebook ---
# =========================================================

library(tidyverse)
library(broom)      # tidy(), glance()
library(car)        # vif
library(lmtest)     # bptest

# =========================================================
# Setup: predictor sets and outcomes
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
        outcome       = outcome,
        predictor     = pred,
        r_squared     = summary(model)$r.squared,
        adj_r_squared = summary(model)$adj.r.squared,
        spec          = "share"
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
bivariate_share %>%
  select(predictor, outcome, estimate, std.error, statistic, p.value, r_squared) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  arrange(desc(r_squared)) %>%
  print(n = Inf)

bivariate_pc %>%
  select(predictor, outcome, estimate, std.error, statistic, p.value, r_squared) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  arrange(desc(r_squared)) %>%
  print(n = Inf)

bivariate_share %>%
  filter(p.value < 0.05) %>%
  select(predictor, outcome, estimate, p.value, r_squared) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  arrange(desc(r_squared))

bivariate_pc %>%
  filter(p.value < 0.05) %>%
  select(predictor, outcome, estimate, p.value, r_squared) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  arrange(desc(r_squared))

# =========================================================
# Step 1B: Full multivariate models — SECONDARY / robustness
# =========================================================

share_models <- map(
  crime_outcomes,
  ~ fit_ols(.x, share_predictors, model_data, "share")
)
names(share_models) <- crime_outcomes

pc_models <- map(
  crime_outcomes,
  ~ fit_ols(.x, pc_predictors, model_data, "per_capita")
)
names(pc_models) <- crime_outcomes

# =========================================================
# Step 2: Model Evaluation
# =========================================================

# --- 2.1 Goodness-of-fit summary ---
gof_table <- bind_rows(
  map_dfr(share_models, ~ .x$glance %>% mutate(outcome = .x$outcome, spec = "share")),
  map_dfr(pc_models,    ~ .x$glance %>% mutate(outcome = .x$outcome, spec = "per_capita"))
) %>%
  select(spec, outcome, r.squared, adj.r.squared, sigma, AIC, BIC, df.residual) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  arrange(spec, outcome)

print(gof_table)

# --- 2.2 Coefficient tables ---
walk(crime_outcomes, function(outcome) {
  cat("\n\n====", outcome_labels[outcome], "| Share Model ====\n")
  print(share_models[[outcome]]$tidy %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
})

walk(crime_outcomes, function(outcome) {
  cat("\n\n====", outcome_labels[outcome], "| Per Capita Model ====\n")
  print(pc_models[[outcome]]$tidy %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
})

# --- 2.3 Standardized coefficients ---
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

# =========================================================
# --- 2.3b Heatmaps of standardized coefficients ---
# Clean labels for GitHub Pages output
# =========================================================

share_predictor_labels <- c(
  police_allocation_share                      = "Police",
  streets_sanit_allocation_share               = "Streets & Sanitation",
  transportation_allocation_share              = "Transportation",
  planning_housing_commdev_allocation_share    = "Planning & Housing",
  library_combined_allocation_share            = "Library",
  human_family_youth_services_allocation_share = "Human & Family Services",
  cps_state_share                              = "CPS State Funding"
)

pc_predictor_labels <- c(
  police_allocation_pc                       = "Police",
  streets_sanit_allocation_pc                = "Streets & Sanitation",
  transportation_allocation_pc               = "Transportation",
  planning_housing_commdev_allocation_pc     = "Planning & Housing",
  library_combined_allocation_pc             = "Library",
  human_family_youth_services_allocation_pc  = "Human & Family Services",
  internal_ops_infrastructure_allocation_pc  = "Internal Ops & Infrastructure",
  cps_state_share                            = "CPS State Funding"
)

# Share heatmap
std_coefs_share_clean <- map_dfr(crime_outcomes, function(outcome) {
  tidy(share_std_models[[outcome]]) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      outcome_label   = outcome_labels[outcome],
      predictor_label = dplyr::recode(term, !!!share_predictor_labels),
      significant     = p.value < 0.05
    )
})

coef_heatmap_share <- ggplot(std_coefs_share_clean,
    aes(x = outcome_label, y = predictor_label, fill = estimate)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(
    label = paste0(round(estimate, 2), if_else(significant, "*", ""))
  ), size = 3.2, color = "black") +
  scale_fill_gradient2(
    low = "steelblue", mid = "white", high = "firebrick",
    midpoint = 0, name = "Std. Coef."
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    title    = "Standardized Coefficients: Budget Shares vs. Crime Rates",
    subtitle = "Reference category: Internal Ops & Infrastructure  |  * p < 0.05",
    x        = NULL,
    y        = NULL,
    caption  = "Multivariate OLS regression. Interpret with caution — VIF = 9–16 for several predictors."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 13),
    plot.subtitle   = element_text(color = "gray40", size = 10),
    plot.caption    = element_text(color = "gray50", size = 9),
    axis.text.x     = element_text(angle = 30, hjust = 1, size = 10),
    axis.text.y     = element_text(size = 10),
    panel.grid      = element_blank(),
    legend.position = "right"
  )

dir.create("assets", showWarnings = FALSE)
ggsave("assets/coef_heatmap.png", coef_heatmap_share,
       width = 10, height = 6, dpi = 150, bg = "white")

# Per capita heatmap
std_coefs_pc_clean <- map_dfr(crime_outcomes, function(outcome) {
  tidy(pc_std_models[[outcome]]) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      outcome_label   = outcome_labels[outcome],
      predictor_label = dplyr::recode(term, !!!pc_predictor_labels),
      significant     = p.value < 0.05
    )
})

coef_heatmap_pc <- ggplot(std_coefs_pc_clean,
    aes(x = outcome_label, y = predictor_label, fill = estimate)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(
    label = paste0(round(estimate, 2), if_else(significant, "*", ""))
  ), size = 3.2, color = "black") +
  scale_fill_gradient2(
    low = "steelblue", mid = "white", high = "firebrick",
    midpoint = 0, name = "Std. Coef."
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    title    = "Standardized Coefficients: Per Capita Spending vs. Crime Rates",
    subtitle = "* p < 0.05",
    x        = NULL,
    y        = NULL,
    caption  = "Multivariate OLS regression."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 13),
    plot.subtitle   = element_text(color = "gray40", size = 10),
    plot.caption    = element_text(color = "gray50", size = 9),
    axis.text.x     = element_text(angle = 30, hjust = 1, size = 10),
    axis.text.y     = element_text(size = 10),
    panel.grid      = element_blank(),
    legend.position = "right"
  )

ggsave("assets/coef_heatmap_pc.png", coef_heatmap_pc,
       width = 10, height = 6.5, dpi = 150, bg = "white")

# --- 2.4 Diagnostic plots ---
walk(crime_outcomes, function(outcome) {
  par(mfrow = c(2, 2))
  plot(share_models[[outcome]]$model,
       main = paste("Share |", outcome_labels[outcome]))
  par(mfrow = c(1, 1))
})

# --- 2.5 VIF check ---
walk(crime_outcomes, function(outcome) {
  cat("\nVIF —", outcome_labels[outcome], "(share):\n")
  print(round(vif(share_models[[outcome]]$model), 2))
})

# =========================================================
# Step 3: Sensitivity check — controlling for year trend
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

comparison <- bind_rows(
  bivariate_share %>% mutate(spec = "share_no_year"),
  bivariate_share_year %>% mutate(spec = "share_year_controlled")
) %>%
  select(spec, predictor, outcome, estimate, p.value, r_squared) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  arrange(predictor, outcome, spec)

comparison %>%
  pivot_wider(
    id_cols     = c(predictor, outcome),
    names_from  = spec,
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
