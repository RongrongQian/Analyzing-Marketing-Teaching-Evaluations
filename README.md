# Effects of Professor Performance and Student Behavior on Overall Teaching Evaluation in Marketing Courses at Miami University

## Project Overview

This project analyzes the drivers of "Overall Instructor Rating" (`iRating`) using evaluation data from **Marketing (MKT) courses at Miami University** collected between 2013 and 2017. 

The study aims to deconstruct the validity of student evaluations by distinguishing between the influence of **instructor performance** (e.g., clarity, enthusiasm) and **student behavior** (e.g., interest, reason for taking the course).

## Research Questions

1.  **Professor Effect:** How much do specific measures of instructor performance influence the final "Overall Rating"?
2.  **Student Effect:** Is there a statistical link between student behavior/engagement and the rating they give?
3.  **Course Type Variation:** Do these relationships change depending on whether the course is a **Core Requirement** or an **Elective**?

## Data & Variables

The analysis is based on aggregated course-section data containing:
* **Dependent Variable:** `iRating` (Overall Instructor Rating, 0-4 scale).
* **Predictors:** * **Professor Performance (`i*`):** 15 items measuring attributes like preparedness and enthusiasm.
    * **Student Behavior (`s*`):** 6 items including Interest, Attendance, and Expected Grade.
    * **Control:** `CourseType` (MKT Core, MKT Elective, FSB Core, MKT Capstone).

## Methodology

The analysis employs advanced statistical modeling techniques implemented in **R**:

1.  **Feature Importance:** Used **Random Forest Regression** (1,000 trees) to identify top predictors and handle multicollinearity among instructor metrics.
2.  **Dimension Reduction:** Applied **Principal Component Analysis (PCA)** to address the high correlation between professor performance items.
3.  **Statistical Inference:** Developed **Hierarchical Linear Mixed Effects Models** (Weighted Two-Level Random Effects) to account for course sections nested within instructors.

## Key Findings

* **Instructor Dominance:** Instructor performance metrics (particularly `iInstructor` composite scores) are the strongest predictors of the overall rating.
* **Student Influence:** Student behavior is a statistically significant but secondary factor compared to teaching quality.
* **Course Type:** After controlling for professor and student factors, **Course Type was found NOT to be a significant predictor** of overall ratings.

## Files

* `MKT_Rongrong_RICHEL_final_report.html`: The full analysis report generated via RMarkdown.

## Authors

* **Rongrong**
* **RICHEL**
* *Master's Students in Statistics*
