# STATS 506 Final Project
**Title:** Trends in the Relationship between Happiness and Financial Satisfaction in the United States, 1980–2024  
**Author:** Zimo Shu

## Overview
This repository contains the analysis code for my STATS 506 final project. 

Happiness has long been a popular topic in social science research for years. Among its many determinants, financial satisfaction consistently emerges as a strong correlate of self-reported happiness. However, the social meaning of financial satisfaction may change over time due to evolving labor markets, rising inequality, shifting cultural values, and major historical events such as the pandemic. Motivated by these changes, this analysis explicitly tests whether the happiness–finance relationship exhibits meaningful variation over the past four decades, including potential shifts after major historical periods.

Specifically, this study asks whether the relationship between happiness and financial satisfaction has changed over time after controlling for demographic and socioeconomic characteristics.

## Key components
- Application of GSS survey weights
- Exploratory data analysis (EDA)
- Weighted linear regression models with and without interaction terms
- Model comparisons (AIC) and a nested-model F-test for interaction terms
- Hypothesis testing of interaction effects
- Vectorized Monte Carlo simulation to construct prediction bands
- Bayesian robustness check (brms)

## Data
GSS data are accessed via the `gssr` R package.  
Documentation consulted: https://kjhealy.github.io/gssr/
