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


2. Crime data processing

Crime incident data was sourced from the Chicago Police Department Open Data
Portal (2001–present). Each row in the raw data represents a single reported
crime record, identified by a unique Case Number. Because a single incident
can generate multiple records (e.g., multiple charges), the data was collapsed
to the incident level using Case Number as the unique identifier before
counting. Only incidents with a valid year between 2001 and 2024 were retained.

Crime types were classified using FBI Uniform Crime Reporting (UCR) Index
codes, joined onto the incident data via IUCR code lookup. Crimes were
categorized into four groups: index violent (homicide, criminal sexual assault,
robbery, aggravated assault/battery), index property (burglary, theft, motor
vehicle theft, arson), other index, and non-index. Homicides were also flagged
separately as a standalone outcome. Annual counts were then aggregated from
the incident-level data and joined into the main modeling dataset.

3. Regression analysis approach

**Outcome variables.** Six crime rate outcomes were modeled: all crime,
index violent, index property, non-index, homicide (all), and homicide (index),
each expressed as incidents per 100,000 residents.

**Predictor specifications.** Two parallel normalizations were used for all
budget predictors: share of total city budget and inflation-adjusted spending
per capita. These address different questions — share captures relative budget
priorities, while per capita captures absolute resource availability. Both
specifications were run to assess robustness across normalization choices.

**Reference category.** In share-of-budget models, Internal Operations &
Infrastructure allocation was omitted as the reference category. Because all
departmental shares sum to 1, including all categories simultaneously produces
perfect multicollinearity. The reference category was chosen as the least
theoretically central department for the research question. Coefficients in
share models are therefore interpreted as the association between a given
department's share and crime rates *relative to* the omitted category.

**Bivariate models as primary analysis.** Variance inflation factor (VIF)
diagnostics on the full multivariate share model revealed severe collinearity
among police, library, and human services shares (VIF = 9–16), which is
consistent with these departments' budget trajectories moving together over
the study period. Given the small sample size (n ≈ 24), multivariate models
with high-VIF predictors produce unstable and potentially misleading
coefficients. Bivariate models — one predictor at a time — were therefore
designated the primary analysis. Full multivariate models are reported as a
secondary robustness check.

4. Reporting bias and the homicide outcome

A known limitation of using aggregate crime counts as an outcome is reporting
or detection bias: higher police staffing may increase the rate at which minor
crimes are detected and recorded, mechanically inflating crime statistics
independently of underlying safety conditions. This is particularly relevant
for non-index and property crimes, where a large share of incidents go
unreported and detection depends heavily on patrol activity.

Homicide rates were examined as a less biased outcome. Homicides are
discovered and recorded at near-complete rates regardless of police staffing
levels, making them a cleaner signal of actual public safety conditions. If
budget effects are genuine rather than artifacts of reporting dynamics, they
should be visible in homicide rates as well as other crime types.

5. Controlling for the time trend

Chicago's crime rates declined substantially from 2001 to 2024, and the
composition of the city's budget shifted over the same period. This creates
a risk of spurious correlation: any budget category that trended upward (or
downward) over this period will appear correlated with crime simply by virtue
of sharing a time trend, not because of any causal relationship.

To assess robustness, all bivariate share models were re-estimated with
calendar year added as a covariate. Predictors whose significance survived
year control were interpreted as having associations with crime beyond the
shared temporal trend. Predictors that lost significance after year control
were interpreted as likely reflecting the time trend rather than an independent
budget effect. Results from both specifications are reported to allow
transparent comparison.
