# Crime Rate Prediction using Chicago Budget Data

## Overview

Regression analysis of Chicago crime rates against city budget data

<img src="./images/chi_crime_rate_header_image.jpg" width="1280" height="640"/>

*Photo by [Pedro Lastra](https://unsplash.com/@peterlaster?utm_content=creditCopyText&utm_medium=referral&utm_source=unsplash) on Unsplash*

------------------------------------------------------------------------

## 1. Business Understanding

### 1.1 Background

Crime is a central concern for many city and suburban residents, often leading to contentious discussions regarding city budgets. Proponents of increased police funding argue that it is essential for maintaining public safety. Conversely, there is a growing movement, rooted in the advocacy of activists like Martin Luther King Jr. and Angela Davis, calling for defunding or even abolishing the police. While this idea has existed for many years, it has gained significant momentum in recent times, particularly following the tragic murder of George Floyd by Minneapolis police officer Derek Chauvin. 

This movement argues that redirecting funds from police departments toward public health programs, community centers, public parks, transportation, and affordable housing could create safer and more equitable communities by addressing the root causes of crime.

In cities like Chicago, these opposing viewpoints pose significant challenges for city officials tasked with determining annual budget allocations. Striking a balance between sectors such as education, parks, libraries, and public safety requires not only careful prioritization but also robust, data-driven decision-making to address both community needs and public safety concerns.

### 1.2 Goals

This project aims to develop an ***inferential*** model that predicts the crime rate in Chicago based on the city’s budget data. The primary goal is to identify which aspects of the budget—whether related to police funding, education, parks, libraries, or other services—are most strongly correlated with crime reduction. Specifically, this project seeks to evaluate the claim that increasing police spending directly results in a reduction in crime rates.

### 1.3 Success Criteria

The success of this project will be determined by the model’s ability to provide actionable insights and explanations regarding the relationship between budget allocations and crime rates. Key measures of success will include:

-   **Model performance**: The effectiveness of the model will be assessed using the Root Mean Squared Error (RMSE) to quantify prediction accuracy.

-   **Feature importance**: The model should identify key budgetary factors that significantly impact crime rates, particularly whether police funding plays a crucial role in crime reduction.

## 2. Data

### 2.1 Data Collection

This project utilized data from multiple sources, focusing on budget data (city and public school system), population data, and crime data. These datasets were compiled from different sources and formats to create a cohesive analysis.

**Population Data**

The population data was sourced from the Illinois Department of Public Health’s population census, compiled through the U.S. Department of Commerce’s 2020 Census. This dataset provided decennial population figures for the City of Chicago from 1980 to 2020.

-   I compiled this data into a spreadsheet under the column population, where each row represented a single year from 1980–2024.

-   Since the data only covered every 10 years, intermediate years were initially left blank. During the data preparation phase, I address how these gaps were interpolated.

**Crime Data**

Crime data was gathered from two main sources: 1. Chicago’s Open Data Portal: Provided recent crime data from 2001 to the present. 2. Chicago Police Department Annual Reports: Supplied historical crime data from 1980 to 2000.

In the compiled dataset, I added a column labeled `num_of_crimes`:

-   For years 1980–2000, I manually entered data from the annual police department reports.

-   For years 2001–present, I used the Open Data Portal to merge crime data into the spreadsheet during the data understanding phase.

**City Budget Data**

City budget data was collected from the Chicago City Clerk’s records database, which included annual appropriation ordinances dating back to 1982. From these reports, I extracted the following data points:

-   Total appropriated budget
-   Appropriations for specific departments:
-   Police Department
-   Department of Public Health
-   Streets and Sanitation
-   Public Library
-   Department of Transportation
-   Department of Family and Support Services
-   Department of Housing
-   Department of Planning and Development
-   Office of Budget and Management (which funds the Parks Department)

I chose to focus on these departments and bureaus based on my domain knowledge and it feels like these are the ones that feel closest to affecting on the ground human life.

Public School System Budget Data

Public school budget data was sourced from the Chicago Public Schools (CPS) annual budget reports. CPS revenue generally falls into three categories: local, state, and federal funding.

-   I chose to focus on state funding, as it is more adjustable than local property taxes and more locally relevant than federal funding.

-   This data was added to the spreadsheet under the column state_funding.

### 2.2 Data Understanding

### 2.3 Data Preparation

## 3. Exploratory Data Analysis

## 4. Modeling

## 5. Evaluation

## 6. Conclusion

### 6.1 Limitations

### 6.2 Recommendations

### 6.3 Next Steps

## 7. References
