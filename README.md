# CAHPS Health Plan Survey Analysis

This project evaluates whether patients who feel listened to by their healthcare providers also report higher satisfaction with their health plan.

## 📊 Project Summary

The analysis uses CAHPS Health Plan Survey data from 3,000 respondents to test the statement:

> "Patients who feel listened to by healthcare staff report higher satisfaction with their care."

We used R for data cleaning, descriptive analysis, inferential statistics, and predictive modeling. The project also includes a React/Vite dashboard for exploring the findings.

## 🎯 Business Objective

To use survey analytics and statistical modeling to determine whether the perceived frequency of doctor listening is associated with overall health plan satisfaction.

## 📁 Dataset

The dataset includes:

- 3,000 survey respondents
- 36 variables
- Patient ratings for Health Plan, Doctor Listening, Doctor Rating, and Health Care Rating
- Demographic variables such as age, gender, and education

Key dataset facts:

- Zero duplicate rows in the analysis sample
- Zero missing values in the variables used for the final analysis
- Doctor Listening responses were roughly evenly distributed across Never, Sometimes, Usually, and Always

## 🧬 Analysis Objectives

1. Explore distributions, averages, and patterns across listening groups
2. Test whether observed differences are statistically significant
3. Build predictive models to quantify the explanatory power of communication variables
4. Make an evidence-based decision on the analytical statement

## 🔧 Analysis Workflow

### Data Preparation
- Checked for duplicate rows
- Cleaned text fields and standardised categories
- Investigated missingness and confirmed missing-at-random for unused fields
- Identified outliers using IQR; none were found for bounded rating scales
- Created encoded variables for modeling, including numeric Doctor Listening and binary high satisfaction

### Descriptive Analysis
- Measured mean Health Plan Ratings by listening category
- Compared proportions of respondents with high satisfaction (rating ≥ 7)
- Visualised group overlap with boxplots and distribution summaries

### Inferential Analysis
- Tested normality with Shapiro-Wilk
- Applied Kruskal-Wallis for group comparison
- Calculated Spearman correlation for listening and plan satisfaction
- Performed post-hoc Dunn tests with Bonferroni correction when needed

### Predictive Modeling
- Split data 80/20 into training and testing samples
- Built a communication-only model and a full model with demographics
- Evaluated R-squared, RMSE, cross-validation, and predictor significance
- Checked multicollinearity with VIF scores

## 📈 Key Findings

- Mean Health Plan Rating differences across listening groups were extremely small (max 0.196 on a 0–10 scale)
- High satisfaction rates ranged from 33.9% to 37.6% across groups
- The Kruskal-Wallis test showed no significant difference in Health Plan Rating by listening group (p = 0.698)
- Spearman correlation was negligible and not significant (rho = 0.013, p = 0.465)
- Predictive models produced essentially zero explanatory power (R-squared ≈ 0.0003 to 0.0004)
- Cross-validation confirmed the model performed no better than predicting the mean
- VIF scores were all below 2, so multicollinearity was not responsible for the null result

## ✅ Final Conclusion

The analytical statement is not supported by the evidence. In this CAHPS survey sample, feeling listened to by healthcare staff does not meaningfully predict higher health plan satisfaction.

### Practical interpretation

Health plan satisfaction appears to be driven more by factors such as coverage, cost, and administrative experience, rather than clinical communication alone.

## 🧠 What this project demonstrates

- Rigorous data preparation and cleaning
- Use of non-parametric statistics for non-normal survey data
- Evidence-based decision making from descriptive, inferential, and predictive analysis
- Clear reporting of a meaningful null result

## 📁 Project Structure

- `data/`: source CSV files used for analysis
- `scripts/r/`: R scripts for data processing, analysis, and modeling
- `images/`: plots and figures generated from the analysis
- `dashboard/`: React/Vite dashboard for interactive results
- `docs/`: supplementary documentation and outputs

## 🛠 Tools Used

- R
- dplyr
- ggplot2
- lubridate
- janitor
- zoo
- React
- Vite

---

This README captures the full analysis story from problem statement through null-result conclusion, aligned with the presentation script for your project.

