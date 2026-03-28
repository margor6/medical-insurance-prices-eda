# Medical Insurance Prices - Exploratory Data Analysis

![R](https://img.shields.io/badge/R-4.0%2B-blue)
![RMarkdown](https://img.shields.io/badge/Document-RMarkdown-orange)
![Status](https://img.shields.io/badge/Status-Completed-success)

## Overview

The project presents an Exploratory Data Analysis (EDA) of medical insurance prices using **RMarkdown**. The goal was to investigate how various factors, such as age, BMI, smoking status, and region, **affect insurance charges**. 

The analysis discovers significant correlations, particularly the impact of lifestyle habits on medical costs, and discusses limitations within the dataset (e.g., lack of medical history).

## Dataset

The analysis uses the personal health dataset, which contains data for **1,338 people**.

**Dataset's Columns:**
* `age`: Age of the person.
* `sex`: Person's gender (female, male).
* `bmi`: Body mass index.
* `children`: Number of children covered by health insurance.
* `smoker`: Smoking status.
* `region`: The person's residential area in the US.
* `charges`: Individual costs billed by health insurance.

## Tech
* **Core:** R 
* **Libraries:** `tidyverse`, `ggplot2`, `dplyr`, `knitr`.
* **Format:** RMarkdown report generated to HTML.

## Key Findings
* **Smoking Impact:** Being a smoker is the strongest predictor of higher insurance charges.
* **Age Factor:** Charges generally increase with age, showing a linear trend for non-smokers.
* **BMI Correlation:** High BMI significantly increases costs, but primarily for smokers (interaction effect).
* **Data Limitations:** The analysis deduces that unobserved variables (likely chronic diseases or family's medical history) play a major role in pricing, creating distinct clusters in the data that simple demographic variables cannot fully explain.

<img width="1083" height="507" alt="image" src="https://github.com/user-attachments/assets/c1fc5b85-715b-4416-9a0a-f64b657b5012" />


# How to View

You can view the original report directly on GitHub:
**[Click here to view the Analysis](https://margor6.github.io/medical-insurance-prices-eda/medical_insurance_eda.html)**

## Author: Marcin Górski
