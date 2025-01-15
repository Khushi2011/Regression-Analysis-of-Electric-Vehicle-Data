# Final Project Report: Regression Analysis of Electric Vehicle Population Data

## Prepared By:
- Khushi Manishkumar Doshi  
- Nahimah Yakubu Suglo  
- Devang Shirodkar  
- Shicheng Wan  

### Presented To:  
Prof. Vivian Clements  

### Date:  
May 13th, 2024  

---

## Table of Contents
1. [Introduction](#introduction)
2. [Dataset Overview](#dataset-overview)
3. [Project Rationale and Objectives](#project-rationale-and-objectives)
4. [Methods and Data Cleaning](#methods-and-data-cleaning)
5. [Outlier Treatment](#outlier-treatment)
6. [Descriptive Statistics](#descriptive-statistics)
7. [Data Visualization](#data-visualization)
8. [Statistical Analysis](#statistical-analysis)
    - [Chi-Square Test](#chi-square-test)
    - [ANOVA](#anova)
    - [Ridge and Lasso Regression](#ridge-and-lasso-regression)
9. [Findings and Recommendations](#findings-and-recommendations)
10. [Future Work](#future-work)
11. [References](#references)

---

## Introduction
The global automotive industry is undergoing a significant transformation with the shift towards electric vehicles (EVs) to mitigate environmental concerns. This report explores factors influencing EV adoption using statistical and regression analyses.

---

## Dataset Overview
The dataset includes variables such as:
- **Categorical**: VIN, County, City, State, Make, Model, Vehicle Type, etc.
- **Numerical**: Electric Range, Base MSRP, Model Year, Legislative District, etc.

---

## Project Rationale and Objectives
### Objectives:
1. Identify key factors influencing EV adoption.
2. Understand the impact of vehicle characteristics like range and MSRP.
3. Analyze geographic and policy influences on EV adoption rates.

---

## Methods and Data Cleaning
1. **Data Cleaning**: Addressed missing values in postal codes and legislative districts using geocoding, kNN imputation, and median substitution.
2. **Outlier Treatment**: Applied K-Means clustering to identify and address anomalies.

---

## Outlier Treatment
Identified outliers using clustering. Segmented data into 5 clusters based on price and electric range.

---

## Descriptive Statistics
Summary statistics revealed:
- Median electric range: **57.83 miles**.
- Significant variability in MSRP and geographic spread.

---

## Data Visualization
- Correlation matrix: Revealed a moderate negative correlation (-0.48) between model year and electric range.
- Heatmaps and scatter plots: Highlighted geographic and temporal trends in EV adoption.

---

## Statistical Analysis

### Chi-Square Test
- Null Hypothesis: Vehicle type does not influence electric range.
- Result: **P-value < 0.05**, rejecting the null hypothesis.

### ANOVA
- Investigated effects of MSRP and legislative districts on electric range.
- Findings: Both factors significantly influence range (**P-value < 0.05**).

### Ridge and Lasso Regression
- Ridge: Mitigated multicollinearity by shrinking coefficients.
- Lasso: Identified key predictors by setting non-significant coefficients to zero.

---

## Findings and Recommendations
1. **Key Predictors**:
   - Electric range and MSRP significantly affect EV adoption.
   - Geographic location and infrastructure play critical roles.
2. **Recommendations**:
   - Improve battery technology and charging infrastructure.
   - Implement policies targeting regions with low adoption.

---

## Future Work
- Extend analysis by incorporating additional variables.
- Explore advanced forecasting techniques for EV adoption trends.

---

## References
1. Sperling, D., & Gordon, D. (2009). *Two billion cars: Driving toward sustainability*.
2. International Energy Agency. (2020). *Global EV Outlook 2020*.
3. Dataset Source: [Electric Vehicle Population Data](https://catalog.data.gov/dataset/electric-vehicle-population-data)
4. Additional References: Refer to detailed resources for statistical methods.

---

This report provides insights into the adoption dynamics of EVs, aiming to inform policymakers and industry stakeholders about strategic measures to enhance sustainable transportation.

