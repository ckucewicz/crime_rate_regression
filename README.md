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
This analysis followed a multi-step process to prepare and standardize data before modeling:

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

## 7. Limitations

## 8. Conclusions & Policy Insights

## 9. Reproducibility
