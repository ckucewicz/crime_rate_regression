# Crime Rate Prediction using Chicago Budget Data
Regression analysis of Chicago crime rates against city budget data

<img src="images/chi_crime_rate_header_image.jpg" alt="Arial view of the Chicago skyline" width="80%"/>

## 1. Project Overview

This project explores the relationship between City of Chicago budget allocations and reported crime rates from 1980 to 2024. Public debates around safety often pit police funding against community services like housing, health, education, and parks. Using public data, this project examines whether changes in police spending correlate with reductions in crime, and whether other types of spending are more strongly associated with improved public safety outcomes.

## 2. Research Question(s)

What are the key contributing budgetary factors to crime rates in Chicago, and how is budgetary allocation to various public services (such as police, schools, parks, and libraries) related to crime rates?
To explore this, the project also asks:
* How have crime rates and department-level budgets changed over time?
* Is police funding more predictive of crime rates than other services?
* Which city departments show the strongest statistical associations with public safety?

## 3. Motivation

Crime is a central concern for many Chicago residents. Debates around how to reduce crime often center on how city budgets should be allocated. Proponents of increased police funding argue that it's essential for maintaining public safety. On the other hand, activists like Martin Luther King Jr. and Angela Davis have long advocated for redirecting police funding toward social programs, and this call gained renewed urgency after the 2020 murder of George Floyd. The argument: addressing the root causes of crime through investments in health, education, housing, and public services may be more effective than increasing police presence. This project enters that conversation with data.


## 4. Data Sources

**Population Data**

* Source: U.S. Census via Illinois Department of Public Health
* Coverage: Decennial population counts from 1980 to 2020, interpolated for all other years (1980–2024)
  
**Crime Data**

* Source 1: Chicago Police Department Annual Reports (1980–2000)
* Source 2: City of Chicago Open Data Portal (2001–present)
* Column: `num_of_crimes` manually compiled and merged across sources
  
**City Budget Data**

* Source: Chicago City Clerk Records (Annual Appropriation Ordinances)
* Departments included:
  * Police Department
  * Department of Public Health
  * Streets and Sanitation
  * Public Library
  * Department of Transportation
  * Department of Family and Support Services
  * Department of Housing
  * Department of Planning and Development
  * Office of Budget and Management (parks funding)
  
**CPS Budget Data**

* Source: Chicago Public Schools Annual Budget Reports
* Focus: State-level education funding only (excluded local property and federal funds)
* Column: `state_funding`

**Inflation Data**
* Source: U.S. Bureau of Labor Statistics (BLS) – Consumer Price Index (CPI-U), All Urban Consumers, U.S. City Average, All Items.
* Coverage: Monthly CPI data from 2001–2024. Annual averages were used for inflation adjustment to 2024 USD.

## 5. Methods
This analysis followed a multi-step process to prepare and standardize data before modeling. [View full methodology](https://github.com/ckucewicz/crime_rate_regression/blob/main/docs/methodology.md)

1.	Data Cleaning
   
Removed commas and currency symbols from numeric fields and converted all relevant columns to numeric types. Standardized missing values (e.g., blank strings, “n/a”) as NA.

2.	Population Interpolation
   
Because official population counts are available only for census years, missing values for intermediate years were estimated using linear interpolation. This method was chosen because Chicago’s population changes relatively gradually from year to year, making straight-line estimates between census points a reasonable and transparent assumption that avoids overfitting.

3.	Inflation Adjustment
   
Adjusted all dollar-based variables to constant 2024 USD using annual CPI data from the Bureau of Labor Statistics. This ensured that monetary values were directly comparable over time and not distorted by inflationary effects.

4.	Budget Normalization
   
Created two versions of budget variables:
  * Share of total budget — the primary normalization, aligning directly with the research question on relative spending priorities. This focuses on proportional allocation, which is often more meaningful for policy analysis than absolute dollar amounts.

  * Per capita — a secondary normalization to account for changes in population size. This matters because a larger population can dilute the per-person availability of resources, even if the total allocation rises. Including per capita measures allows for a robustness check against this effect.

5.	Crime Rate Standardization

Converted raw crime counts to rates per 100,000 residents. This is a common criminological standard that enables meaningful comparisons across years with varying population sizes.

6. Planned Analysis

The next stage will involve regression modeling, including potential lagged budget variables to account for the fact that changes in public spending may take years to influence crime rates.

## 6. Key Findings

Budget allocation shares correlate strongly with crime rates at the bivariate
level, but most of these associations disappear after controlling for the
overall time trend. Crime in Chicago declined substantially from 2001–2024,
and budget compositions shifted over the same period — meaning many
correlations reflect parallel trends rather than independent relationships.

After controlling for year:
- **Streets & Sanitation share** and **CPS state funding share** are the only
  budget variables with statistically significant independent associations
  with crime rates, though both in counterintuitive positive directions.
- **Police share, human services share, and library share** — despite high
  bivariate R² values (0.72–0.81) — do not show robust associations once
  the time trend is accounted for.
- **Homicides**, the outcome least susceptible to reporting bias, show the
  weakest associations with budget variables across all specifications
  (R² = 0.16–0.41 bivariate), with no budget predictor surviving year control.
- Results are sensitive to the choice of normalization: police funding share
  is positively correlated with crime, while police spending per capita is
  negatively correlated — reflecting how the overall budget grew relative to
  population over this period.

__View full methodology and regression results__

## 7. Limitations

**Small sample size.** The analysis covers 24 annual observations (2001–2024).
This limits statistical power and the number of predictors that can be
included in multivariate models without overfitting.

**Temporal confounding.** Crime in Chicago declined steadily over this period
while the city's budget composition shifted. Many budget-crime correlations
reflect this shared time trend rather than independent relationships. Year was
included as a control variable in a sensitivity analysis to assess robustness.

**Multicollinearity.** In the share-of-budget specification, police, library,
and human services shares move together over time (pairwise r = 0.89–0.92),
producing high variance inflation factors (VIF = 9–16) in multivariate models.
Bivariate models were used as the primary analysis to avoid this.

**Reporting bias.** More police presence may increase the detection and
recording of minor crimes, inflating crime counts for non-serious offenses
independently of actual safety conditions. Homicide rates were examined as
a less reporting-sensitive robustness check.

**Observational data.** No causal claims can be made. Unobserved confounders
— economic conditions, demographic shifts, housing markets, federal policy —
likely influence both budget decisions and crime rates simultaneously.

**Budget category aggregation.** Department allocations were grouped to
manage degrees of freedom. This aggregation may obscure variation within
categories (e.g., specific public health programs vs. administrative costs).

## 8. Conclusions & Policy Insights

This analysis finds little evidence that changes in budget allocation shares
are independently associated with changes in crime rates in Chicago, once the
overall time trend is accounted for. The strong correlations visible at the
bivariate level largely reflect the fact that both crime and budget
compositions shifted over the same two-decade period — not that one drove
the other.

This does not settle the policy debate around police funding versus community
investment. What it does suggest is that annual city budget shares alone are
a limited lens for answering that question. Budget decisions unfold alongside
economic cycles, demographic changes, federal policy, and other factors that
this analysis cannot isolate. A more granular analysis — examining
neighborhood-level spending, multi-year lagged effects, or natural experiments
around specific budget shifts — would be better positioned to detect causal
effects.

The homicide result is worth highlighting separately. Homicides are the least
susceptible to reporting bias and arguably the most socially significant
outcome. The weakness of budget-homicide associations across all specifications
suggests that the mechanisms driving lethal violence in Chicago operate
largely independently of how the city allocates its annual budget.

## 9. Reproducibility

**Requirements:** R (≥ 4.1), with the following packages:
`tidyverse`, `janitor`, `stringr`, `tidycensus`, `tigris`, `purrr`,
`jsonlite`, `lubridate`, `readr`, `httr`, `ggplot2`, `gganimate`, `gifski`,
`corrplot`, `car`, `lmtest`, `sandwich`, `broom`

**API keys required:**
- U.S. Census Bureau API key (for `tidycensus`). Register at
  https://api.census.gov/data/key_signup.html and add to `~/.Renviron`
  as `CENSUS_API_KEY=your_key_here`.
- BLS Public Data API (no key required for public endpoints).

**Data files required** (not included in repo due to size):
- `data/crime_rate_regression_data.csv` — city budget and population data
- `data/Crimes_-_2001_to_Present_20241225.csv` — Chicago crime incident data
- `data/IUCR_codes.csv` — FBI Uniform Crime Reporting code definitions

**Run order:**
1. `data_prep.R` — cleans data, pulls population and CPI, processes crime
   incidents, and outputs `model_data`
2. `analysis.R` — EDA, correlation analysis, and assumption testing
3. `modeling.R` — bivariate and multivariate regression models and evaluation
