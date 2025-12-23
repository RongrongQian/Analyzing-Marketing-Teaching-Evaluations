# title: Effects of Professor Performance and Student Behavior on Overall Teaching Evaluation in Marketing Courses at Miami University
# Rongrong Qian and Richel Attafuah | 08 Oct 2025

# =========================================================
# 0) Load Packages
# =========================================================
library(tidyverse)
library(dplyr)
library(tidyr)
library(tibble)
library(knitr)
library(kableExtra)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(corrplot)
library(car)
library(tibble)
library(reshape2) 
library(stringr) 
library(purrr)
library(ranger) # for random forest
library(lmerTest) # for mixed models
library(lme4) 
library(broom.mixed)


# =========================================================
# 1) Read and prepare data
# =========================================================
# Please change the file path to your local path
mkt <- read.csv("/Users/rongrongqian/Desktop/660/2/CombinedData.csv")
#glimpse(mkt)
mkt <- mkt %>%
  mutate(
    # change CourseType to factor 
    CourseType = factor(CourseType, 
                        levels = c(1, 2, 3, 4),
                        labels = c("FSB Core", "MKT Core", "MKT Elective", "MKT Capstone"))
  )
# =========================================================
# 2) Outcome Distribution: Zero Cases & Histogram
# =========================================================
# Count zero cases
zero_cases <- mkt %>% filter(iRating == 0)
n_zero <- nrow(zero_cases)

# Plot distribution
ggplot(mkt, aes(x = iRating)) +
  geom_histogram(binwidth = 0.25, boundary = 0, closed = "left", fill = "steelblue", color = "black") +
  scale_x_continuous(breaks = seq(0, 4, 1), limits = c(0, 4)) +
  labs(x = "Overall Instructor Rating (0–4 Scale)",
       y = "Count",
       caption = paste0("Zero-rating sections detected: ", n_zero)) +
  theme_minimal()+
  theme(plot.caption = element_text(hjust = 0, size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"))

zero_tbl <- zero_cases  %>% 
  dplyr::mutate(
    Participation = if_else(Enrolled > 0, Completed / Enrolled, NA_real_)
  ) %>% 
  dplyr::select(Term, CourseType, Enrolled, Completed, Participation, iRating)  %>% 
  dplyr::arrange(dplyr::desc(Participation))

knitr::kable(zero_tbl, digits = 2,
             caption = "Table 3: Audit of iRating = 0 sections (check data quality/context)") |>
  kableExtra::kable_styling(bootstrap_options = c("striped","hover","condensed"),
                            full_width = FALSE, position = "center")

# =========================================================
# 3) Identify Item Columns & Participation
# =========================================================
# Identify evaluation item columns
i_cols <- names(mkt)[grepl("^i", names(mkt))]           # includes iRating
s_cols <- names(mkt)[grepl("^s", names(mkt))]
item_cols <- c(i_cols, s_cols)

# Ensure participation exists (used to detect structural missing)
mkt <- mkt %>%
  mutate(Participation = if_else(Enrolled > 0, Completed/Enrolled, NA_real_))

# =========================================================
# 4) Data Quality Checks 
# =========================================================
# Row-level checks
mkt_audit <- mkt %>%
  mutate(
    # Count of non-missing evaluation items
    n_items_nonmiss = rowSums(!is.na(pick(all_of(item_cols)))),
    # Among non-missing items, are ALL equal to zero?
    all_items_zero  = if_else(
      n_items_nonmiss > 0,
      rowSums(pick(all_of(item_cols)) == 0, na.rm = TRUE) == n_items_nonmiss,
      FALSE
    ),
    # Structural missing (no respondents → all evals should be NA after cleaning)
    structural_missing = (Completed == 0 | is.na(Completed)),
    # Usable row = has at least 1 non-missing, non-zero item (after cleaning zeros if needed)
    any_nonzero_item = if_else(
      n_items_nonmiss > 0,
      rowSums(pick(all_of(item_cols)) > 0, na.rm = TRUE) > 0,
      FALSE
    ),
    usable_section = (!structural_missing) & any_nonzero_item
  )

# =========================================================
# 5) Participation by Instructor
# =========================================================
# Calculate average participation rate by InstID
participation_rate <- mkt %>%
  group_by(InstID) %>%
  mutate(Participation = (Completed / Enrolled) * 100) %>%
  summarise(avg_participation = mean(Participation, na.rm = TRUE)) %>%
  arrange(desc(avg_participation))
# Plot average participation rate by InstID
ggplot(participation_rate, aes(x = reorder(InstID, -avg_participation), y = avg_participation)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Instructor ID (InstID)",
       y = "Average Participation Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"))

# =========================================================
# 6) Descriptive Statistics: Professor and Student Measures
# =========================================================
# Descriptive statistics for professor performance measures
prof_means <- mkt %>%
  # FIX: Exclude the non-numeric InstID column from the selection
  select(starts_with("i"), -InstID) %>%
  summarise(across(everything(), list(mean = ~mean(.x, na.rm = TRUE),
                                      sd = ~sd(.x, na.rm = TRUE),
                                      median = ~median(.x, na.rm = TRUE),
                                      min = ~min(.x, na.rm = TRUE),
                                      max = ~max(.x, na.rm = TRUE)))) %>%
  pivot_longer(everything(), names_to = c("Measure", ".value"), names_sep = "_")

# Descriptive statistics for student behavior measures (This code was already correct)
student_means <- mkt %>%
  select(starts_with("s")) %>%
  summarise(across(everything(), list(mean = ~mean(.x, na.rm = TRUE),
                                      sd = ~sd(.x, na.rm = TRUE),
                                      median = ~median(.x, na.rm = TRUE),
                                      min = ~min(.x, na.rm = TRUE),
                                      max = ~max(.x, na.rm = TRUE)))) %>%
  pivot_longer(everything(), names_to = c("Measure", ".value"), names_sep = "_")

# Combine both tables
descriptive_stats <- bind_rows(prof_means, student_means) %>%
  dplyr::mutate(dplyr::across(c(mean, sd), ~ round(.x, 2)))

# Display the table
kable(descriptive_stats, caption = "Table 4: Descriptive Statistics for Professor Performance and Student Behavior Measures") %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "center"
  )

# =========================================================
# 7) iRating by CourseType (Numerical Summaries + Boxplot)
# =========================================================
# Summary table: count, mean, sd by CourseType
course_rating_tbl <- mkt %>%
  group_by(CourseType) %>%
  summarise(
    n = n(),
    mean_iRating = mean(iRating, na.rm = TRUE),
    sd_iRating   = sd(iRating, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_iRating)) %>%
  mutate(across(c(mean_iRating, sd_iRating), ~round(.x, 2)))

kable(course_rating_tbl,
      caption = "Table 5: Overall Instructor Rating (iRating) by Course Type — count, mean, and SD") %>%
  kable_styling(bootstrap_options = c("striped","hover","condensed"),
                full_width = FALSE, position = "center")

# Box plot of iRating by CourseType
ggplot(mkt, aes(x = CourseType, y = iRating)) +
  geom_boxplot(outlier.shape = NA, width = 0.6, fill = "skyblue") +
  geom_jitter( width = 0.15, alpha = 0.35, size = 1) +
  geom_point(data=course_rating_tbl,
             aes(y = mean_iRating),
             shape = 23, size = 3, fill = "white", color = "black") +
  labs(x = "Course Type",
       y = "Overall Instructor Rating (iRating)"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank())

# =========================================================
# 8) Temporal Trend by Term (Spring vs Fall)
# =========================================================
# Summary table: count, mean, sd by Term
term_rating_tbl <- mkt %>%
  group_by(Term) %>%
  summarise(
    n = n(),
    mean_iRating = mean(iRating, na.rm = TRUE),
    sd_iRating   = sd(iRating, na.rm = TRUE)
  ) %>%
  arrange(Term) %>%
  mutate(across(c(mean_iRating, sd_iRating), ~round(.x, 2)))


# Prepare data for line plot
mkt_term <- mkt %>%
  mutate(
    Year    = as.integer(str_sub(Term, 1, 4)),
    SemCode = str_sub(Term, 5, 6),
    Semester = case_when(
      SemCode == "20" ~ "Spring",
      SemCode == "10" ~ "Fall",
      TRUE ~ "Other"
    ),
    sem_ord = case_when(  
      Semester == "Spring" ~ 1L,
      Semester == "Fall"   ~ 2L,
      TRUE                 ~ 3L
    ),
    Term_label = paste(Year, Semester)  
  )

# Summary statistics by Term
term_summary <- mkt_term %>%
  group_by(Year, Semester, sem_ord, Term_label) %>%
  summarise(
    n = n(),
    mean_iRating = mean(iRating, na.rm = TRUE),
    sd_iRating   = sd(iRating, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Year, sem_ord)

# Ensure Term_label is a factor with levels in the order of appearance
lvl_order <- unique(term_summary$Term_label)  
term_summary <- term_summary %>%
  mutate(Term_label = factor(Term_label, levels = lvl_order))


ggplot(term_summary, aes(x = Term_label, y = mean_iRating,
                         group = Semester, color = Semester)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_iRating - sd_iRating,
                    ymax = mean_iRating + sd_iRating),
                width = 0.1, alpha = 0.5) +
  labs(x = "Academic Term", y = "Mean iRating (± SD)", color = "Semester") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# =========================================================
# 9) Interaction-over-time: CourseType × Term
# =========================================================
# Summary statistics by CourseType and Term
interaction_summary <- mkt_term %>%
  group_by(CourseType, Year, Semester, sem_ord, Term_label) %>%
  summarise(
    n = n(),
    mean_iRating = mean(iRating, na.rm = TRUE),
    sd_iRating   = sd(iRating, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(CourseType, Year, sem_ord)
# Ensure Term_label is a factor with levels in the order of appearance
interaction_summary <- interaction_summary %>%
  mutate(Term_label = factor(Term_label, levels = lvl_order))
# Create interaction plot
ggplot(interaction_summary, aes(x = Term_label, y = mean_iRating,
                                group = CourseType, color = CourseType)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_iRating - sd_iRating,
                    ymax = mean_iRating + sd_iRating),
                width = 0.1, alpha = 0.5) +
  labs(x = "Academic Term", y = "Mean iRating (± SD)", color
       = "Course Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# =========================================================
# 10) Scatter Plots (Professor / Student) vs iRating
# =========================================================
# Scatter plots for professor performance measures vs iRating
mkt_clean <- mkt %>%
  filter(!is.na(iRating) & !dplyr::near(iRating, 0)) %>%
  mutate(CourseType = as.factor(CourseType))

# performance measures vs iRating
prof_vars <- setdiff(grep("^i", names(mkt_clean), value = TRUE), "iRating")
prof_vars <- prof_vars[sapply(mkt_clean[prof_vars], is.numeric)]
prof_vars <- prof_vars[sapply(mkt_clean[prof_vars], function(x) sd(x, na.rm = TRUE) > 0)]
student_vars <- grep("^s", names(mkt_clean), value = TRUE)
student_vars <- student_vars[sapply(mkt_clean[student_vars], is.numeric)]
student_vars <- student_vars[sapply(mkt_clean[student_vars], function(x) sd(x, na.rm = TRUE) > 0)]

# Function to create scatter plot with regression line
make_scatter <- function(df, xvar) {
  ggplot(df, aes(x = .data[[xvar]], y = .data[["iRating"]])) +
    geom_point(aes(color = CourseType), alpha = 0.3, na.rm = TRUE) +
    geom_smooth(
      aes(x = .data[[xvar]], y = .data[["iRating"]]),
      method = "lm", se = FALSE, inherit.aes = FALSE, na.rm = TRUE
    ) +
    labs(title = paste("Scatter Plot of", xvar, "vs iRating"),
         x = xvar, y = "iRating") +
    theme_minimal() +
    theme(legend.position = "none")
}

prof_plots    <- lapply(prof_vars,   make_scatter, df = mkt_clean)
student_plots <- lapply(student_vars, make_scatter, df = mkt_clean)

# Extract legend from one of the plots
last_x <- tail(student_vars, 1)
legend_plot <- ggplot(mkt_clean, aes(x = .data[[last_x]], y = .data[["iRating"]])) +
  geom_point(aes(color = CourseType), alpha = 0.6, na.rm = TRUE) +
  theme_minimal() +
  guides(color = guide_legend(title = "Course Type"))
legend <- cowplot::get_legend(legend_plot)

# Combine all plots into a grid
all_plots <- c(prof_plots, student_plots)

cowplot::plot_grid(
  cowplot::plot_grid(plotlist = all_plots, ncol = 3),
  legend,
  rel_widths = c(5, 0.7)
)


# =========================================================
# 11) Correlation Heatmap 
# =========================================================
# heat map for correlation matrix
cor_data <- mkt %>%
  select(starts_with("s"), 
         starts_with("i"), 
         -InstID)
# Calculate the correlation matrix
cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")

# Plot the correlation matrix using corrplot
corrplot(cor_matrix,
         method = "color",         
         type = "upper",           
         order = "hclust",         
         addCoef.col = "black",    
         tl.col = "black",         
         tl.srt = 45,              
         number.cex = 0.7,         
         diag = FALSE              
)

# =========================================================
# 12) OLS + VIF check
# =========================================================
# predictors: all i* except iRating, plus all s*
predictors <- c(
  names(mkt)[grepl("^i", names(mkt)) & names(mkt) != "iRating"],
  names(mkt)[grepl("^s", names(mkt))]
)

form <- as.formula(paste("iRating ~", paste(predictors, collapse = " + ")))
m <- lm(form, data = mkt)

v <- car::vif(m)

# Handle numeric vector (VIF) vs matrix (GVIF when factors are present)
if (is.matrix(v)) {
  vif_tbl <- as.data.frame(v) |>
    rownames_to_column("Variable") |>
    mutate(
      # Adjust GVIF to comparable VIF scale when df > 1
      VIF = ifelse(df > 1, GVIF^(1/(2*df)), GVIF)
    ) |>
    select(Variable, VIF)
} else {
  vif_tbl <- tibble(Variable = names(v), VIF = as.numeric(v))
}

vif_tbl <- vif_tbl |>
  arrange(desc(VIF)) |>
  mutate(
    VIF = round(VIF, 2),
    Flag = case_when(
      VIF > 10 ~ "High",
      VIF > 5  ~ "Moderate",
      TRUE     ~ "Acceptable"
    )
  )
# Display VIF table
vif_tbl %>%
  knitr::kable(
    caption = "Table 6: Collinearity (VIF) among all professor and student measures",
    align   = c("l","r","c"),
    format  = "html"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped","hover","condensed"),
    full_width = FALSE, position = "center"
  ) %>% 
  row_spec(which(vif_tbl$Flag == "High"),
           bold = TRUE, color = "white", background = "#d9534f")  %>% 
  row_spec(which(vif_tbl$Flag == "Moderate"),
           background = "#f0ad4e")

# =========================================================
# 13) Random Forest (Permutation Importance) 
# =========================================================
#  Random Forest (Permutation) — Feature Selection 
set.seed(20250922)  # keeping it reproducible

# ---- choose the data frame safely ----
.df <- mkt %>% mutate(.row_id = dplyr::row_number())

# ---- Outcome & predictors ----
# Outcome: overall instructor rating
preds_raw <- c(setdiff(i_cols, "iRating"), s_cols, "CourseType", "Completed", "Enrolled")
preds_keep <- intersect(preds_raw, names(.df))  

dat_rf <- .df %>%
  dplyr::select(dplyr::all_of(c("iRating", preds_keep))) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(
    CourseType = as.factor(CourseType)
  )

y_vec <- dat_rf[["iRating"]]
X_mat <- dat_rf %>% dplyr::select(-iRating, -Enrolled)

# ---- optional: use response-count weights if you have them (w_completed) ----
if ("Completed" %in% names(dat_rf)) {
  w_raw <- pmax(dat_rf$Completed, 0)
  w_completed <- if (sum(w_raw) > 0) w_raw / sum(w_raw) * nrow(dat_rf) else rep(1, nrow(dat_rf))
  X_mat <- X_mat %>% dplyr::select(-Completed)
} else {
  w_completed <- rep(1, nrow(dat_rf))
}
  
p <- ncol(X_mat)

# ---- fit random forest with Gini importance ----
rf <- ranger(
  x               = X_mat,
  y               = y_vec,
  num.trees       = 1000,
  mtry            = max(1, floor(sqrt(p))),
  importance      = "permutation",
  case.weights    = w_completed,
  seed            = 20250922
)

# ---- extract & rank importance cleanly ----
imp <- tibble::enframe(rf$variable.importance, name = "Variable", value = "PermDropMSE") %>%
  arrange(desc(PermDropMSE))

# ---- plot: top 20 predictors ----
ggplot(head(imp, 20), aes(x = reorder(Variable, PermDropMSE), y = PermDropMSE)) +
  geom_col() +
  coord_flip() +
  labs(
    x = NULL,
    y = "Permutation importance (increase in MSE if permuted)"
  ) +
  theme_minimal()


# =========================================================
# 14) Professor Items Standardization & Item-level VIF
# =========================================================
# Identify professor item columns (exclude the outcome)
prof_item_cols <- c(
  "iUnderstand",
  "iWelQues",
  "iTopic",
  "iDemo",
  "iQuesEffect",
  "iAskQues",
  "iAnalyProb",
  "iParticipate",
  "iChallenged",
  "iPrepared",
  "iRating"
)
prof_item_cols <- intersect(prof_item_cols, names(mkt))
prof_x_cols    <- setdiff(prof_item_cols, "iRating") 

# Standardize professor items
mkt_profz <- mkt %>%
  mutate(across(all_of(prof_x_cols), scale, .names = "{.col}"))

# VIF check
dat_vif_items <- mkt_profz %>%
  dplyr::select(all_of(c("iRating", prof_x_cols))) %>%
  drop_na()

form_items <- as.formula(paste("iRating ~", paste(prof_x_cols, collapse = " + ")))
lm_items   <- lm(form_items, data = dat_vif_items)

v_items <- car::vif(lm_items)
vif_items_tbl <- tibble(Variable = names(v_items), VIF = as.numeric(v_items)) %>%
  arrange(desc(VIF))

# Display VIF table
vif_tbl %>%
  mutate(VIF = round(VIF, 2)) %>%
  knitr::kable(
    caption = "Table 6: Item level VIFs for standardized professor items",
    align   = c("l","r","c"),
    format  = "html"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped","hover","condensed"),
    full_width = FALSE, position = "center"
  ) %>% 
  row_spec(which(vif_tbl$Flag == "High"),
           bold = TRUE, color = "white", background = "#d9534f")  %>% 
  row_spec(which(vif_tbl$Flag == "Moderate"),
           background = "#f0ad4e")

# =========================================================
# 15) Construct Building (Standardize → Average → Standardize)
# =========================================================
# Function to create standardized composite scores for constructs (standardize-then-average-then-standardize)
mk_construct <- function(df, items){
  items <- items[items %in% names(df)]
  if (length(items) == 0) return(rep(NA_real_, nrow(df)))
  if (length(items) == 1) {
    as.numeric(scale(df[[items]]))
  } else {
    comp <- rowMeans(scale(df[, items, drop = FALSE]), na.rm = TRUE)
    as.numeric(scale(comp))
  }
}

# create constructs
keep <- function(v) v[v %in% names(mkt_profz)]

grp_questions <- keep(c("iWelQues","iAskQues","iQuesEffect"))
grp_clarity   <- keep(c("iUnderstand","iTopic"))
grp_demo      <- keep(c("iDemo","iAnalyProb"))

# Create constructs in the data frame
mkt_constr <- mkt_profz %>%
  mutate(
    Ques_Z = mk_construct(., grp_questions),
    InstrQual_Z = mk_construct(., c(grp_clarity, grp_demo)),
    Participate_Z  = mk_construct(., keep("iParticipate")),
    Chall_Z= mk_construct(., keep("iChallenged")),
    Prep_Z = mk_construct(., keep("iPrepared"))
  )

# check correlations among constructs
construct_vars2 <- c("Ques_Z", "InstrQual_Z","Participate_Z","Chall_Z","Prep_Z") 

dat_vif2 <- mkt_constr %>%
  dplyr::select(iRating, dplyr::all_of(construct_vars2)) %>%
  tidyr::drop_na()

lm2 <- lm(as.formula(paste("iRating ~", paste(construct_vars2, collapse=" + "))), data = dat_vif2)

v_constr <- car::vif(lm2)
vif_constr_tbl <- tibble::tibble(
  Variable = names(v_constr),
  VIF = as.numeric(v_constr)
) %>%
  dplyr::arrange(desc(VIF))

knitr::kable(vif_constr_tbl, digits = 3,
             caption = "Table 7: Construct-level collinearity (VIF) after conceptual grouping") %>%
  kableExtra::kable_styling(full_width = FALSE)

# =========================================================
# 16) Mixed-model Dataset & Weights Setup
# =========================================================
# Ensure needed columns
mkt_constr <- mkt_constr %>%
  mutate(
    CourseType   = factor(CourseType),
    sEngaged_Z   = as.numeric(scale(sEngaged)),
    sPositive_Z  = as.numeric(scale(sPositive))
  )
# One analysis dataset for ALL models 
df_all <- mkt_constr %>%
  select(iRating, InstrQual_Z, Participate_Z, Chall_Z, Prep_Z, Ques_Z,
         sEngaged_Z, sPositive_Z, CourseType, InstID, Completed) %>%
  drop_na()

# Weights: w_i = Completed_i / sum(Completed) * N, where N = nrow(df_all)
eps <- 1e-6
w_all <- pmax(df_all$Completed, eps)
w_all <- (w_all / sum(w_all) )* nrow(df_all)

# =========================================================
# 17) Mixed Models (ML fits for LRT): Random Intercept vs Random Intercept and Random Slopes
# =========================================================
# Fit ML versions for valid LRTs
form_m1 <- iRating ~ InstrQual_Z + Participate_Z + Chall_Z + Prep_Z + Ques_Z + (1 | InstID)
form_m1_randslope <- update(form_m1, . ~ . + (InstrQual_Z + Participate_Z + Chall_Z + Prep_Z + Ques_Z | InstID))


m1_ML <- lmer(form_m1, data = df_all, weights = w_all, REML = FALSE)
m1_random_slope_ML <- lmer(form_m1_randslope, data = df_all, weights = w_all, REML = FALSE)


# Model comparison via LRT and AIC
anova(m1_ML, m1_random_slope_ML) 

# =========================================================
# 18) Extended Models (M2/M3/M4) + Global LRT Table
# =========================================================
# Further models
form_m2 <- update(form_m1_randslope, . ~ . + sEngaged_Z + sPositive_Z)
form_m3 <- update(form_m2, . ~ . + CourseType)
form_m4 <- update(form_m3, . ~ . + (InstrQual_Z + Participate_Z + Chall_Z + Prep_Z + Ques_Z + sEngaged_Z + sPositive_Z):CourseType)

m2_ML <- lmer(form_m2, data = df_all, weights = w_all, REML = FALSE)
m3_ML <- lmer(form_m3, data = df_all, weights = w_all, REML = FALSE)
m4_ML <- lmer(form_m4, data = df_all, weights = w_all, REML = FALSE)

# All Model comparison table
kable(anova(m1_random_slope_ML, m2_ML, m3_ML, m4_ML),
      caption = "Table 8: Model Comparison via Likelihood Ratio Tests") %>%
  kable_styling(bootstrap_options = c("striped","hover","condensed"),
                full_width = FALSE, position = "center")

# =========================================================
# 19) Refit with REML for Reporting
# =========================================================
# Refit REML for reporting (optional but standard)
m1_REML <- update(m1_random_slope_ML, REML = TRUE)
m2_REML <- update(m2_ML, REML = TRUE)
m3_REML <- update(m3_ML, REML = TRUE)
m4_REML <- update(m4_ML, REML = TRUE)

#summary(m1_REML)  # Q1
#summary(m2_REML)  # Q1 Q2
#summary(m3_REML)
#summary(m4_REML)  # Q3

# =========================================================
# 20) Helper Function: fixef_tbl() 
# =========================================================
# Function to extract fixed effects table with significance stars
fixef_tbl <- function(model) {
  coefs <- summary(model)$coefficients
  tibble::tibble(
    Term = rownames(coefs),
    Estimate = coefs[, "Estimate"],
    Std_Error = coefs[, "Std. Error"],
    t_value = coefs[, "t value"],
    p_value = 2 * (1 - pnorm(abs(coefs[, "t value"])))  # two-tailed
  ) %>%
    mutate(
      Estimate = round(Estimate, 3),
      Std_Error = round(Std_Error, 3),
      t_value = round(t_value, 3),
      p_value = round(p_value, 4),
      Significance = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01  ~ "**",
        p_value < 0.05  ~ "*",
        p_value < 0.1   ~ ".",
        TRUE           ~ ""
      )
    )
}

# Extract fixed effects tables for all models
m1_fixef <- fixef_tbl(m1_REML)
m2_fixef <- fixef_tbl(m2_REML)
m3_fixef <- fixef_tbl(m3_REML)
m4_fixef <- fixef_tbl(m4_REML)


# =========================================================
# 21) M1-2 Fixed Effects with 95% CI
# =========================================================
# CIs for fixed effects (Wald)
ci_m1rs_fix_wald <- confint(m1_random_slope_ML, parm = "beta_", method = "Wald", level = 0.95, oldNames = FALSE)

# table for coefficients with CIs
m1rs_fixef <- fixef_tbl(m1_random_slope_ML) %>%
  mutate(
    CI_Lower = round(ci_m1rs_fix_wald[, 1], 3),
    CI_Upper = round(ci_m1rs_fix_wald[, 2], 3)
  ) %>% select(Term, Estimate, CI_Lower, CI_Upper, t_value, p_value, Significance)
# Display fixed effects table for Model 1-2
kable(m1rs_fixef, caption = "Table 18: Fixed Effects Estimates with 95% CI for Model 1-2") %>%
  kable_styling(bootstrap_options = c("striped","hover","condensed"),
                full_width = FALSE, position = "center")

# =========================================================
# 22) M2 Fixed Effects with 95% CI
# =========================================================
# CIs and fixed effects for model 2
ci_m2_fix_wald <- confint(m2_REML, parm = "beta_", method = "Wald", level = 0.95, oldNames = FALSE)
# table for coefficients with CIs
m2_fixef_full <- fixef_tbl(m2_REML) %>%
  mutate(
    CI_Lower = round(ci_m2_fix_wald[, 1], 3),
    CI_Upper = round(ci_m2_fix_wald[, 2], 3)
  ) %>% select(Term, Estimate, CI_Lower, CI_Upper, t_value, p_value, Significance)
# Display fixed effects table for Model 2
kable(m2_fixef_full, caption = "Table 19: Fixed Effects Estimates with 95% CI for Model 2") %>%
  kable_styling(bootstrap_options = c("striped","hover","condensed"),
                full_width = FALSE, position = "center")

# =========================================================
# 23) M3 Fixed Effects with 95% CI
# =========================================================
# CIs and fixed effects for model 3
ci_m3_fix_wald <- confint(m3_REML, parm = "beta_", method = "Wald", level = 0.95, oldNames = FALSE)
# table for coefficients with CIs
m3_fixef_full <- fixef_tbl(m3_REML) %>%
  mutate(
    CI_Lower = round(ci_m3_fix_wald[, 1], 3),
    CI_Upper = round(ci_m3_fix_wald[, 2], 3)
  ) %>% select(Term, Estimate, CI_Lower, CI_Upper, t_value, p_value, Significance)
# Display fixed effects table for Model 3
kable(m3_fixef_full, caption = "Table 20: Fixed Effects Estimates with 95% CI for Model 3") %>%
  kable_styling(bootstrap_options = c("striped","hover","condensed"),
                full_width = FALSE, position = "center")

# =========================================================
# 24) M4 Fixed Effects with 95% CI
# =========================================================
# CIs and fixed effects for model 4
ci_m4_fix_wald <- confint(m4_REML, parm = "beta_", method = "Wald", level = 0.95, oldNames = FALSE)
# table for coefficients with CIs
m4_fixef_full <- fixef_tbl(m4_REML) %>%
  mutate(
    CI_Lower = round(ci_m4_fix_wald[, 1], 3),
    CI_Upper = round(ci_m4_fix_wald[, 2], 3)
  ) %>% select(Term, Estimate, CI_Lower, CI_Upper, t_value, p_value, Significance)
# Display fixed effects table for Model 4
kable(m4_fixef_full, caption = "Table 21: Fixed Effects Estimates with 95% CI for Model 4") %>%
  kable_styling(bootstrap_options = c("striped","hover","condensed"),
                full_width = FALSE, position = "center")











