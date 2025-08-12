# Methodology

## Data Preparation

The goal of the data preparation phase was to produce a dataset that was complete, consistent, and ready for regression modeling. This involved several key steps: ensuring population data was available for all years, incorporating accurate crime counts, standardizing counts relative to population, adjusting dollar values for inflation, and creating consistent naming and formatting. The processes and justifications are outlined below.

### 1. General data cleaning

This step addressed structural issues in the dataset to ensure consistency and usability:

* **Removed the num_of_crimes column**: This column was dropped because crime counts were sourced directly from a separate, more complete crime dataset.

* **Filtered years to 2001–2024**: The original dataset extended back to 1980, but earlier years contained large amounts of missing or incomplete data. The year 2001 was chosen as the starting point because it was the first year with sufficiently complete data for the majority of variables, and it also aligned with the availability of detailed Chicago crime data from the city’s open data portal. This range provided ~20 years of data, which was adequate for robust modeling while minimizing missingness.

* **Standardized column names and missing value formats**: Used a consistent snake_case naming convention and standardized all missing values to NA to ensure compatibility with R functions during later processing.

* **Converted numeric columns stored as character strings to numeric**: Many numeric variables (including population, crime counts, budget allocations, and CPS revenue) were formatted as strings containing symbols such as $ or ,. These were stripped of non-numeric characters and converted to numeric types to allow mathematical operations.

* **Interpolated missing population values between census years**: Population data was missing for years between U.S. Census estimates. Linear interpolation was used to estimate these missing values, as it provides a straightforward method to approximate intermediate values between known data points without introducing additional model assumptions. This step ensured that population-based standardizations (e.g., per capita crime rates) could be calculated for all years.

* **Converted CPS revenue values from millions to actual dollars**: Chicago Public Schools revenue data was originally reported in millions. These values were multiplied by one million to express them as full dollar amounts, ensuring consistency across all budgetary variables.


### 2. Crime data processing
