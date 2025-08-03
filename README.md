# Crime Rate Prediction using Chicago Budget Data
Regression analysis of Chicago crime rates against city budget data


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

## 5. Methods

## 6. Key Findings

## 7. Limitations

## 8. Conclusions & Policy Insights

## 9. Reproducibility
