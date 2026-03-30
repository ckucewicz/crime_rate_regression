# --- Analysis Notebook ---

library(ggplot2)
library(gganimate)
library(gifski)
library(tidyverse)
library(corrplot)
library(car)      # vif, durbinWatsonTest
library(lmtest)   # bptest

# --- Step 1: Exploratory Data Analysis ---

# 1.1 Visualizations of Variables

# (A) unpivot groups

df_long_pc <- model_data %>%
  select(year, ends_with("_pc")) %>%
  filter(year > 2002) %>%
  pivot_longer(-year, names_to = "category", values_to = "per_capita")

df_long_share <- model_data %>%
  select(year, ends_with("_share"), -cps_state_share) %>%
  filter(year > 2002) %>%
  pivot_longer(-year, names_to = "category", values_to = "share of budget") %>%
  mutate(year = as.integer(year)) %>%
  complete(year, category, fill = list(share = 0))

viz_long <- model_data %>%
  select(year, population, total_budget, rate_all_per_100k, rate_index_violent_per_100k, rate_index_property_per_100k, rate_homicide_all_per_100k) %>%
  pivot_longer(-year, names_to = "series", values_to = "value")

crime_rates_long <- model_data %>%
  select(year, starts_with("rate_")) %>%
  filter(year > 2002) %>%
  pivot_longer(-year, names_to = "category", values_to = "rate")

# line graph of population, budget, crime
ggplot(data = viz_long, aes(x = year, y = value)) +
  geom_line() +
  facet_wrap(~series, scales = "free_y") 

# line graph of crime rate trends 2003-2024
ggplot(data = crime_rates_long, aes(x = year, y = rate)) +
  geom_line() +
  facet_wrap(~category, scales = "free_y")

# animated bar graph of pcs that adjusts by year
p <- ggplot(
  df_long_pc, 
  aes(x = category, y = per_capita, fill = category, group = category)) +
  geom_col(show.legend = FALSE) + 
  labs(
    title = "Year: {closest_state}", 
    x = NULL, 
    y = "Per Capita Spending"
  ) + 
  theme_minimal(base_size = 16) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    plot.title = element_text(size = 24, hjust = 1, vjust = -1)
  )

anim <- p +
  transition_states(year, transition_length = 0.2, state_length = 0.8)

gif <- animate(
  anim,
  nframes = length(unique(df_long_pc$year)) *6,
  fps = 6,
  width = 900, height = 520,
  renderer = gifski_renderer()
)

anim_save("per_capita.gif", gif)

# animated bar graph of shares that adjusts by year
p <- ggplot(
  df_long_share, 
  aes(x = category, y = `share of budget`, fill = category, group = category)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent_format()) + 
  labs(
    title = "Year: {closest_state}", 
    x = NULL, 
    y = "Share of Budget"
  ) + 
  theme_minimal(base_size = 16) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    plot.title = element_text(size = 24, hjust = 1, vjust = -1)
  )

anim <- p +
  transition_states(year, transition_length = 0.2, state_length = 0.8)

gif <- animate(
  anim,
  nframes = length(unique(df_long_share$year)) *6,
  fps = 6,
  width = 900, height = 520,
  renderer = gifski_renderer()
)

anim_save("budget_share.gif", gif)

# 1.2 Summary Statistics
summary(model_data)

# =========================================================
# 1.3 Correlation Analysis
# =========================================================

# Define predictor sets consistently across both notebooks.
# internal_ops_infrastructure_allocation_share is the omitted reference category —
# it is dropped from share models to avoid perfect multicollinearity (shares sum to 1).
# It is KEPT in per-capita models because that constraint doesn't apply there.

share_predictors <- c(
  "police_allocation_share",
  "streets_sanit_allocation_share",
  "transportation_allocation_share",
  "planning_housing_commdev_allocation_share",
  "library_combined_allocation_share",
  "human_family_youth_services_allocation_share",
  "cps_state_share"
  # internal_ops_infrastructure_allocation_share omitted as reference
)

pc_predictors <- c(
  "police_allocation_pc",
  "streets_sanit_allocation_pc",
  "transportation_allocation_pc",
  "planning_housing_commdev_allocation_pc",
  "library_combined_allocation_pc",
  "human_family_youth_services_allocation_pc",
  "internal_ops_infrastructure_allocation_pc",
  "cps_state_share"  # no pc version exists; include as-is in both specs
)

crime_outcomes <- c(
  "rate_all_per_100k",
  "rate_index_violent_per_100k",
  "rate_index_property_per_100k",
  "rate_nonindex_per_100k",
  "rate_homicide_all_per_100k",
  "rate_homicide_index_per_100k"
)

# 1.3A — Correlation groups
raw_allocation <- model_data %>%
  select(all_of(c(
    "police_allocation", "streets_sanit_allocation", "transportation_allocation",
    "planning_housing_commdev_allocation", "library_combined_allocation",
    "human_family_youth_services_allocation", "internal_ops_infrastructure_allocation",
    crime_outcomes
  )))

per_capita <- model_data %>%
  select(all_of(c(pc_predictors, crime_outcomes)))

budget_share <- model_data %>%
  select(all_of(c(share_predictors, crime_outcomes)))

# 1.3B — Correlation matrices
cor_share <- cor(
  budget_share %>% filter(complete.cases(.)),
  method = "pearson", use = "pairwise.complete.obs"
)
round(cor_share, 2)
corrplot(cor_share, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45,
         title = "Correlation: Budget Shares vs Crime Rates", mar = c(0,0,2,0))

cor_pc <- cor(
  per_capita %>% filter(complete.cases(.)),
  method = "pearson", use = "pairwise.complete.obs"
)
round(cor_pc, 2)
corrplot(cor_pc, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45,
         title = "Correlation: Per Capita Spending vs Crime Rates", mar = c(0,0,2,0))

# =========================================================
# Step 2: Testing Linear Regression Assumptions
# (Uses a preliminary full-share model on all-crime rate)
# =========================================================

prelim_data <- model_data %>%
  select(all_of(c("rate_all_per_100k", share_predictors))) %>%
  filter(complete.cases(.))

prelim_model <- lm(rate_all_per_100k ~ ., data = prelim_data)

# 2.1 Linearity — scatter of each predictor vs outcome with fitted line
prelim_data %>%
  pivot_longer(-rate_all_per_100k, names_to = "predictor", values_to = "value") %>%
  ggplot(aes(x = value, y = rate_all_per_100k)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  facet_wrap(~predictor, scales = "free_x") +
  labs(title = "Linearity Check: Budget Shares vs All-Crime Rate",
       x = "Predictor Value", y = "Crime Rate per 100k") +
  theme_minimal()

# 2.2 Independence — Durbin-Watson test for autocorrelation
# DW ~ 2 means no autocorrelation; < 2 is positive autocorrelation (common in time series)
durbinWatsonTest(prelim_model)

# 2.3 Normality of residuals
par(mfrow = c(1, 2))
hist(residuals(prelim_model), breaks = 10,
     main = "Residuals Distribution", xlab = "Residuals")
qqnorm(residuals(prelim_model), main = "Q-Q Plot of Residuals")
qqline(residuals(prelim_model), col = "red")
par(mfrow = c(1, 1))
shapiro.test(residuals(prelim_model))  # W near 1 = normal; p > 0.05 = no evidence of non-normality

# 2.4 Homoscedasticity — residuals vs fitted
plot(fitted(prelim_model), residuals(prelim_model),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted (Homoscedasticity Check)")
abline(h = 0, col = "red", lty = 2)
bptest(prelim_model)  # Breusch-Pagan: p < 0.05 suggests heteroscedasticity

# 2.5 Multicollinearity — Variance Inflation Factor
# VIF > 5 is concerning; VIF > 10 is severe
vif(prelim_model)