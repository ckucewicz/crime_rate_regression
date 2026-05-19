# --- Analysis Notebook ---
library(ggplot2)
library(tidyverse)
library(corrplot)
library(car)      # vif, durbinWatsonTest
library(lmtest)   # bptest
library(scales)
library(plotly)
library(htmlwidgets)

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
  select(year, population, total_budget, rate_all_per_100k, rate_index_violent_per_100k,
         rate_index_property_per_100k, rate_homicide_all_per_100k) %>%
  pivot_longer(-year, names_to = "series", values_to = "value")

crime_rates_long <- model_data %>%
  select(year, starts_with("rate_")) %>%
  filter(year > 2002) %>%
  pivot_longer(-year, names_to = "category", values_to = "rate")

# Line graph of population, budget, crime
ggplot(data = viz_long, aes(x = year, y = value)) +
  geom_line() +
  facet_wrap(~series, scales = "free_y")

# Line graph of crime rate trends 2003-2024
ggplot(data = crime_rates_long, aes(x = year, y = rate)) +
  geom_line() +
  facet_wrap(~category, scales = "free_y")

# =========================
# Shared color palette — matches the website
# =========================
dept_colors <- c(
  "Police"                        = "#1a4a7a",
  "Human & Family Services"       = "#1e7e4a",
  "Streets & Sanitation"          = "#993c1d",
  "Planning & Housing"            = "#534ab7",
  "Library"                       = "#3b6d11",
  "Transportation"                = "#ba7517",
  "Internal Ops & Infrastructure" = "#888780"
)

# =========================
# Stacked area chart: budget shares over time (static PNG — unchanged)
# =========================
share_labels <- c(
  police_allocation_share                      = "Police",
  streets_sanit_allocation_share               = "Streets & Sanitation",
  transportation_allocation_share              = "Transportation",
  planning_housing_commdev_allocation_share    = "Planning & Housing",
  library_combined_allocation_share            = "Library",
  human_family_youth_services_allocation_share = "Human & Family Services",
  internal_ops_infrastructure_allocation_share = "Internal Ops & Infrastructure"
)

share_plot_data <- model_data %>%
  select(year, ends_with("_share"), -cps_state_share) %>%
  pivot_longer(-year, names_to = "category", values_to = "share") %>%
  filter(!is.na(share)) %>%
  mutate(
    category_label = dplyr::recode(category, !!!share_labels),
    category_label = factor(category_label, levels = c(
      "Internal Ops & Infrastructure",
      "Transportation",
      "Library",
      "Planning & Housing",
      "Streets & Sanitation",
      "Human & Family Services",
      "Police"
    ))
  )

budget_share_plot <- ggplot(share_plot_data,
    aes(x = year, y = share, fill = category_label)) +
  geom_area(alpha = 0.88, color = "white", linewidth = 0.3) +
  scale_fill_manual(values = dept_colors) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    breaks = seq(2001, 2024, by = 3),
    expand = c(0, 0)
  ) +
  labs(
    title    = "Chicago City Budget Composition, 2001–2024",
    subtitle = "Share of total budget by department · Inflation-adjusted to 2024 USD",
    x = NULL, y = NULL, fill = NULL,
    caption  = "Source: Chicago City Clerk Annual Appropriation Ordinances"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position    = "top",
    legend.justification = "left",
    legend.key.size    = unit(0.45, "cm"),
    legend.text        = element_text(size = 10),
    plot.title         = element_text(face = "bold", size = 14),
    plot.subtitle      = element_text(color = "gray40", size = 11),
    plot.caption       = element_text(color = "gray50", size = 9),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 2))

dir.create("assets", showWarnings = FALSE)
ggsave("assets/budget_share.png", budget_share_plot,
       width = 11, height = 6.5, dpi = 150, bg = "white")

# =========================
# Per capita spending — INTERACTIVE plotly version
# =========================

pc_labels <- c(
  police_allocation_pc                       = "Police",
  streets_sanit_allocation_pc                = "Streets & Sanitation",
  transportation_allocation_pc               = "Transportation",
  planning_housing_commdev_allocation_pc     = "Planning & Housing",
  library_combined_allocation_pc             = "Library",
  human_family_youth_services_allocation_pc  = "Human & Family Services",
  internal_ops_infrastructure_allocation_pc  = "Internal Ops & Infrastructure"
)

pc_plot_data <- model_data %>%
  select(year, ends_with("_pc")) %>%
  pivot_longer(-year, names_to = "category", values_to = "per_capita") %>%
  filter(!is.na(per_capita)) %>%
  mutate(
    category_label = dplyr::recode(category, !!!pc_labels),
    tooltip_text = paste0(
      "<b>", dplyr::recode(category, !!!pc_labels), "</b><br>",
      "Year: ", year, "<br>",
      "Per capita: $", formatC(per_capita, format = "f", digits = 0, big.mark = ",")
    )
  )

dept_order <- c(
  "Police",
  "Streets & Sanitation",
  "Internal Ops & Infrastructure",
  "Human & Family Services",
  "Transportation",
  "Planning & Housing",
  "Library"
)

fig <- plot_ly()

for (dept in dept_order) {
  dept_data <- pc_plot_data %>% filter(category_label == dept)
  fig <- fig %>%
    add_trace(
      data        = dept_data,
      x           = ~year,
      y           = ~per_capita,
      type        = "scatter",
      mode        = "lines+markers",
      name        = dept,
      text        = ~tooltip_text,
      hoverinfo   = "text",
      line        = list(color = dept_colors[[dept]], width = 2.2),
      marker      = list(color = dept_colors[[dept]], size = 5),
      legendgroup = dept
    )
}

pc_interactive <- fig %>%
  layout(
    title = list(
      text    = "<b>Per Capita City Spending by Department, 2001–2024</b><br><i style='font-size:13px;color:#5a6a7a'>Inflation-adjusted to 2024 USD · Click a department in the legend to show or hide it</i>",
      font    = list(family = "Arial", size = 20, color = "#000000"),
      x       = 0.03,
      y       = 0.875,
      xanchor = "left",
      pad     = list(l = 5)
    ),
    xaxis = list(
      title      = "",
      tickvals   = seq(2001, 2024, by = 3),
      tickformat = "d",
      tickfont   = list(family = "Arial", size = 14, color = "#000000"),
      showgrid   = FALSE,
      zeroline   = FALSE,
      linecolor  = "#d0dce8",
      linewidth  = 1
    ),
    yaxis = list(
      title      = "",
      tickprefix = "$",
      tickformat = ",",
      tickfont   = list(family = "Arial", size = 14, color = "#000000"),
      showgrid   = TRUE,
      gridcolor  = "#e8e8e8",
      gridwidth  = 1,
      zeroline   = FALSE
    ),
    legend = list(
      orientation   = "h",
      x             = 0,
      xanchor       = "left",
      y             = 1.05,
      yanchor       = "top",
      tracegroupgap = 5,
      font          = list(family = "Arial", size = 12, color = "#000000"),
      bgcolor       = "rgba(0,0,0,0)",
      borderwidth   = 0
    ),
    hovermode     = "closest",
    plot_bgcolor  = "#ffffff",
    paper_bgcolor = "#ffffff",
    #autosize      = TRUE,
    margin        = list(t = 150, b = 60, l = 60, r = 20),
    shapes = list(
      list(
        type      = "rect",
        xref      = "x", yref = "paper",
        x0        = 2019.5, x1 = 2021.5,
        y0        = 0, y1    = 0.9,
        fillcolor = "rgba(200,200,200,0.18)",
        line      = list(width = 0)
      )
    ),
    annotations = list(
      list(
        text      = "COVID-19",
        showarrow = FALSE,
        xref      = "x", yref = "paper",
        x         = 2020.5, y = 0.935,
        xanchor   = "center",
        font      = list(family = "Arial", size = 11, color = "#888888")
      ),
      list(
        text      = "Source: Chicago City Clerk Annual Appropriation Ordinances",
        showarrow = FALSE,
        xref      = "paper", yref = "paper",
        x         = 1, y = -0.12,
        xanchor   = "right",
        font      = list(family = "Arial", size = 11, color = "#5a6a7a")
      )
    )
  ) %>%
  config(
    displayModeBar         = TRUE,
    modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScale2d",
                               "hoverClosestCartesian", "hoverCompareCartesian"),
    displaylogo            = FALSE,
    responsive             = TRUE
  )

saveWidget(pc_interactive, "assets/per_capita_interactive.html",
           selfcontained = TRUE, title = "Per Capita City Spending by Department")

# Also keep the static PNG as fallback
pc_line_plot <- ggplot(pc_plot_data,
    aes(x = year, y = per_capita, color = category_label, group = category_label)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.8) +
  scale_color_manual(values = dept_colors) +
  scale_y_continuous(labels = dollar_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(2001, 2024, by = 3)) +
  labs(
    title    = "Per Capita City Spending by Department, 2001–2024",
    subtitle = "Inflation-adjusted to 2024 USD",
    x = NULL, y = "Per Capita Spending (2024 USD)", color = NULL,
    caption  = "Source: Chicago City Clerk Annual Appropriation Ordinances"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position  = "bottom",
    legend.key.size  = unit(0.45, "cm"),
    legend.text      = element_text(size = 10),
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(color = "gray40", size = 11),
    plot.caption     = element_text(color = "gray50", size = 9),
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(nrow = 2))

ggsave("assets/per_capita.png", pc_line_plot,
       width = 11, height = 6.5, dpi = 150, bg = "white")

# =========================
# CLEANED CRIME RATE SMALL MULTIPLES
# =========================

crime_rate_labels <- c(
  rate_all_per_100k            = "All Crime",
  rate_homicide_all_per_100k   = "Homicide (All)",
  rate_homicide_index_per_100k = "Homicide (Index)",
  rate_index_property_per_100k = "Index Property",
  rate_index_violent_per_100k  = "Index Violent",
  rate_nonindex_per_100k       = "Non-Index"
)

crime_rates_long_clean <- model_data %>%
  select(year, starts_with("rate_")) %>%
  filter(year > 2002) %>%
  pivot_longer(-year, names_to = "category", values_to = "rate") %>%
  mutate(
    category_label = dplyr::recode(category, !!!crime_rate_labels),
    category_label = factor(category_label, levels = c(
      "All Crime", "Index Property", "Index Violent",
      "Non-Index", "Homicide (All)", "Homicide (Index)"
    ))
  )

crime_trends_plot <- ggplot(crime_rates_long_clean,
    aes(x = year, y = rate)) +
  geom_line(color = "#1a4a7a", linewidth = 1) +
  geom_point(color = "#1a4a7a", size = 1.5) +
  facet_wrap(~category_label, scales = "free_y", ncol = 3) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 5)) +
  labs(
    title    = "Chicago Crime Rates per 100,000 Residents, 2003–2024",
    subtitle = "Each panel uses its own y-axis scale",
    x        = NULL,
    y        = "Rate per 100,000 residents",
    caption  = "Source: Chicago Police Department Open Data Portal"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 14),
    plot.subtitle      = element_text(color = "gray40", size = 10),
    plot.caption       = element_text(color = "gray50", size = 9),
    strip.text         = element_text(face = "bold", size = 11, color = "#0f2340"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_text(size = 9),
    axis.text.y        = element_text(size = 9)
  )

ggsave("assets/crime_rate_trends.png", crime_trends_plot,
       width = 11, height = 6, dpi = 150, bg = "white")

# 1.2 Summary Statistics
summary(model_data)

# =========================================================
# 1.3 Correlation Analysis
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
# =========================================================

prelim_data <- model_data %>%
  select(all_of(c("rate_all_per_100k", share_predictors))) %>%
  filter(complete.cases(.))

prelim_model <- lm(rate_all_per_100k ~ ., data = prelim_data)

# 2.1 Linearity
prelim_data %>%
  pivot_longer(-rate_all_per_100k, names_to = "predictor", values_to = "value") %>%
  ggplot(aes(x = value, y = rate_all_per_100k)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  facet_wrap(~predictor, scales = "free_x") +
  labs(title = "Linearity Check: Budget Shares vs All-Crime Rate",
       x = "Predictor Value", y = "Crime Rate per 100k") +
  theme_minimal()

# 2.2 Independence
durbinWatsonTest(prelim_model)

# 2.3 Normality of residuals
par(mfrow = c(1, 2))
hist(residuals(prelim_model), breaks = 10,
     main = "Residuals Distribution", xlab = "Residuals")
qqnorm(residuals(prelim_model), main = "Q-Q Plot of Residuals")
qqline(residuals(prelim_model), col = "red")
par(mfrow = c(1, 1))
shapiro.test(residuals(prelim_model))

# 2.4 Homoscedasticity
plot(fitted(prelim_model), residuals(prelim_model),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted (Homoscedasticity Check)")
abline(h = 0, col = "red", lty = 2)
bptest(prelim_model)

# 2.5 Multicollinearity
vif(prelim_model)
