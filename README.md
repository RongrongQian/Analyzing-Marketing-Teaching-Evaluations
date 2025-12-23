# Effects of Professor Performance and Student Behavior on Overall Teaching Evaluation in Marketing Courses at Miami University

## Project Overview

This project analyzes the drivers of "Overall Instructor Rating" (`iRating`) using evaluation data from **Marketing (MKT) courses at Miami University** collected between 2013 and 2017. 

The core statistical goal of this report is to explore the links between specific evaluation metrics and the student’s overall rating of the instructor. The aim is to build a model that explains the sources of variation in “instructor effectiveness” scores.

## Research Questions

1.  The effect of professor performance measures: How much do specific measurements of instructor teaching performance statistically influence the student’s final “overall instructor evaluation”?
2.  The effect of student behavior measures: Is there a statistical link between measurements related to student behavior or course engagement and the overall rating they give the instructor?
3.  Variation by course type: Do the relationships found in questions 1 and 2 change in a significant way depending on the type of course (for example, a core requirement versus an elective class)?


## Data & Variables

The analysis is based on aggregated course-section data containing:
* **Dependent Variable:** `iRating` (Overall Instructor Rating, 0-4 scale).
* **Predictors:** * **Professor Performance (`i*`):** 15 items measuring attributes like preparedness and enthusiasm.
    * **Student Behavior (`s*`):** 6 items including Interest, Attendance, and Expected Grade.
    * **Control:** `CourseType` (MKT Core, MKT Elective, FSB Core, MKT Capstone).

## Methodology

The analysis employs advanced statistical modeling techniques implemented in **R**:

1.  **Feature Importance:** Used **Random Forest Regression** (1,000 trees) to identify top predictors and handle multicollinearity among instructor metrics.
2.  **Dimension Reduction:** Applied **Conceptual Grouping** to combine highly correlated instructor items into composite constructs (e.g., `iInstructor`), reducing variance inflation factors (VIF).
3.  **Statistical Modeling:** Developed **Hierarchical Linear Mixed Effects Models** (Weighted Two-Level Random Effects) to estimate effects while accounting for the nesting of course sections within instructors.

## Key Findings

* **Instructor Dominance:** Instructor performance metrics (particularly `iInstructor` composite scores) are the strongest predictors of the overall rating.
* **Student Influence:** Student behavior is a statistically significant but secondary factor compared to teaching quality.
* **Course Type:** After controlling for professor and student factors, **Course Type was found NOT to be a significant predictor** of overall ratings.

## File

* `MKT_Rongrong_RICHEL_final_report.html`: The full analysis report generated via RMarkdown.

## Authors

* **Rongrong Qian**
* **Richel Attafuah**
* *Master's Students in Statistics*
