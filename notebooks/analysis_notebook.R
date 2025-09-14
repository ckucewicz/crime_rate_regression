# --- Analysis Notebook ---

library(ggplot2)
library(gganimate)
library(gifski)
library(tidyverse)
library(corrplot)

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

# line graph of population, budget, crime
ggplot(data = viz_long, aes(x = year, y = value)) +
  geom_line() +
  facet_wrap(~series, scales = "free_y") 


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

# 1.3 Correlation Analysis
numeric_model_data <- model_data %>%
  select(where(is.numeric))

correlation_matrix <- cor(numeric_model_data, method = "pearson", use = "pairwise.complete.obs")

round(correlation_matrix, 2)

corrplot(correlation_matrix, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

# 1.4 Basic Data Visualizations

# --- Step 2: Testing for Linear Regression Assumptions ---

# 2.1 Linearity

# 2.2 Independence

# 2.3 Normality

# 2.4 Homoskcedasticity

# 2.5 Multicollinearity





