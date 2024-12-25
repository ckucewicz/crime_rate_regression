# Crime Rate Prediction using Chicago Budget Data

## Overview

Regression analysis of Chicago crime rates against city budget data

<img src="./images/chi_crime_rate_header_image.jpg" width="1280" height="640"/>

*Photo by [Pedro Lastra](https://unsplash.com/@peterlaster?utm_content=creditCopyText&utm_medium=referral&utm_source=unsplash) on Unsplash*

------------------------------------------------------------------------

## 1. Business Understanding

### 1.1 Background

Crime is a central concern for many city and suburban residents, often leading to contentious discussions regarding city budgets. Proponents of increased police funding argue that it is essential for maintaining public safety. Conversely, there is a growing movement, rooted in the advocacy of activists like Martin Luther King Jr. and Angela Davis, calling for defunding or even abolishing the police. While this idea has existed for many years, it has gained significant momentum in recent times, particularly following the tragic murder of George Floyd by Minneapolis police officer Derek Chauvin.

These contrasting views create difficult challenges for city officials, particularly in cities like Chicago, when making decisions about annual budget allocations. Balancing the needs of various sectors—such as education, parks, libraries, etc.—while addressing calls for increased police funding requires careful consideration and data-driven decision-making.

### 1.2 Goals

This project aims to develop an ***inferential*** model that predicts the crime rate in Chicago based on the city’s budget data. The primary goal is to identify which aspects of the budget—whether related to police funding, education, parks, libraries, or other services—are most strongly correlated with crime reduction. Specifically, this project seeks to evaluate the claim that increasing police spending directly results in a reduction in crime rates.

### 1.3 Success Criteria

The success of this project will be determined by the model’s ability to provide actionable insights and explanations regarding the relationship between budget allocations and crime rates. Key measures of success will include:

-   **Model performance**: The effectiveness of the model will be assessed using the Root Mean Squared Error (RMSE) to quantify prediction accuracy.

-   **Feature importance**: The model should identify key budgetary factors that significantly impact crime rates, particularly whether police funding plays a crucial role in crime reduction.

## 2. Data

### 2.1 Data Collection

This project used data from multiple sources. Overall, I needed budget data, population data, and crime data for this project, which required pulling data from different sources.

The population data was taken from the Illinois Department of Public Health's population census, which was gathered through the U.S. Department of Commerce, 2020 Census. This contained population data for the City of Chicago every 10 years, starting in 1980, going until 2020. I compiled this data into a spreadsheet under the feature: `population`. Each row represented a single year from 1980-2024. Since the population data was only every 10 years, the intermediate years were left blank for now. During the data preparation phase, I address how I interpolated this missing population data.

Crime data was pulled from 2 sources: Most recent crime data (2001-present) was taken from Chicago's Open Data Portal. For historical crime data, it was pulled from the Chicago Police department annual reports dating back to 1980. In the same spreadsheet, I created a new column labeled `num_of_crimes`. Using the annual police department reports, I entered the number of crimes for each year from 1980-2000. For the 2001-present crime data during the data understsanding phase, I use the crime dataset from Chicago's open data portal to merge data into the original spreadsheet. 

The budget data was gathered from the city of Chicago's Clerk's records database. The database included the annual appropriation ordinances dating all the way back to 1982. From within each report, I extracted the following information: total approportioned budget, total police department appropriation, department of public health appropriation, department of streets and sanitation appropriation, public library appropriation, department of transportation, department for family and support services, department of housing, department of planning and development, office of budget and management (funds parks department)

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
