# Telco Customer Churn – End-to-End Analytics

## Overview
Built an end-to-end telco churn solution to help a subscription business understand and reduce customer attrition. The project connects SQL data modeling, R-based predictive modeling, and Tableau dashboards to surface churn drivers and revenue at risk.

## Tools & Technologies
- SQL (analytical view creation, feature engineering)
- R (tidyverse, caret, glmnet, ranger)
- Tableau (executive-facing dashboards)

## Repository structure
- `sql/` – SQL view and feature engineering
- `r/` – data cleaning and modeling scripts
- `docs/` – executive overview and analysis slides (PDF)

## Key results
- Achieved AUC ≈ 0.98 on holdout data
- Identified top churn drivers: satisfaction score, tenure, price bands, internet service
- Quantified revenue at risk by tenure cohort and service type
