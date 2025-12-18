# Blood-Glucose
Modeling Blood Glucose Dynamics Using Nonlinear Regression

Overview
This project models future blood glucose levels using time-series physiological and behavioral data.
It combines exploratory data analysis, multiple regression models, model selection, diagnostics, and Approximate Bayesian Computation (ABC) for uncertainty estimation.

Data
Key variables include:
- bg_mean – past glucose
- bg.1.00 – future glucose (target)
- hr_mean – heart rate
- insulin_sum, carbs_sum – intake
- steps_sum, cals_sum – activity
- Missing values are imputed, and all numeric variables are standardized.

Methods
- Time-series visualization & rolling averages
- Distribution and correlation analysis
- Multiple regression models (linear, polynomial, interaction terms)
- Model comparison using RSS, AIC, BIC
- Residual diagnostics & 70/30 train–test evaluation
- Rejection ABC to approximate posterior distributions of key parameters

Tools
- R
(tidyverse, ggplot2, corrplot, GGally, zoo, caret, Metrics)

Author
- Satish Adhikari
(MSc – Data Science & Computational Intelligence)
