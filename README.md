# Effects of Professor Performance and Student Behavior on Overall Teaching Evaluation in Marketing Courses at Miami University

## Overview

This project explores the determinants of student teaching evaluations in Marketing courses. By analyzing data from Miami University, the study aims to quantify how much of the "Overall" course rating is driven by **instructor performance** versus **student behavioral factors**.

The analysis provides insights into the validity of teaching evaluations and identifies key actionable areas for improving instruction quality.

## Research Questions

The project seeks to answer the following key questions:
- What are the most significant predictors of a positive overall teaching evaluation?
- Do student behaviors (attendance, interest) bias the evaluation of the professor?
- Are there statistically significant differences in evaluations based on course level or other categorical factors?

## Dataset

The analysis is based on a dataset comprising teaching evaluation metrics from Marketing courses.

| Variable Category | Examples |
|-------------------|----------|
| **Dependent Variable** | `Overall` (Overall course rating) |
| **Professor Metrics** | Preparedness, Enthusiasm, Clarity, Grading Fairness |
| **Student Metrics** | `Interest` (Pre-course interest), Attendance, `Retake` (Willingness to retake) |
| **Control Variables** | Course Level, Class Size, etc. |

## Methodology

The project is implemented in **R** and utilizes a standard statistical analysis pipeline:

1.  **Data Cleaning & Preprocessing:** Handling missing values and formatting categorical variables.
2.  **Exploratory Data Analysis (EDA):**
    -   Histograms and Boxplots for variable distribution.
    -   Correlation matrices to detect multicollinearity.
3.  **Statistical Inference:**
    -   **T-tests / ANOVA:** To compare means across different groups (e.g., different marketing professors or course levels).
    -   **Multiple Linear Regression:** To model the relationship between `Overall` rating and the independent variables.
    -   **Model Diagnostics:** Checking assumptions of linearity, normality, and homoscedasticity.

## Technologies

* **Language:** R 🔵
* **Libraries:** `tidyverse` (ggplot2, dplyr), `knitr`, `rmarkdown` (and likely `car` or `MASS` for statistics).
* **Output:** HTML Report generated via RMarkdown.

## Key Findings

* **Professor Performance:** Attributes related to the instructor's delivery and organization are typically the strongest predictors of the overall score.
* **Student Bias:** Student interest and engagement levels have a measurable, though often secondary, impact on how they rate the course.
* *(Note: Specific coefficients and p-values can be found in the `final_report.html` file.)*

## How to Run

1.  Clone this repository:
    ```bash
    git clone [https://github.com/yourusername/Marketing-Course-Evaluation-Drivers.git](https://github.com/yourusername/Marketing-Course-Evaluation-Drivers.git)
    ```
2.  Open the `.Rmd` file in **RStudio**.
3.  Ensure required packages are installed:
    ```r
    install.packages(c("tidyverse", "knitr", "rmarkdown"))
    ```
4.  Knit the document to HTML or PDF to view the full analysis.

## Authors

* **Rongrong**
* **RICHEL**
* *Master's Student in Statistics*

---
*This project is for academic and research purposes.*
