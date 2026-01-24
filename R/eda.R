# ============================================================
# Exploratory Data Analysis (EDA)
#
# Purpose:
#   Validate outcome distributions, examine predictor behavior,
#   and assess suitability for predictive modeling.
#
# This script supports:
#   - Descriptive analysis section
#   - Model justification
#   - Interpretation of downstream SHAP results
#
# Input:
#   data/interim/aact_features.csv
#
# Output:
#   Console summaries + optional plots
# ============================================================

# ---- Libraries ----
library(dplyr)
library(ggplot2)

# ============================================================
# 1. LOAD FEATURE-ENGINEERED DATA
# ============================================================

df <- read.csv(
  "data/interim/aact_features.csv",
  stringsAsFactors = FALSE
)

# ============================================================
# 2. BASIC DATA SHAPE CHECKS
# ============================================================

cat("Number of studies:", nrow(df), "\n")
cat("Number of features:", ncol(df), "\n")

# ============================================================
# 3. OUTCOME DISTRIBUTIONS
# ============================================================

# ---- Reporting lag (continuous) ----
summary(df$reporting_lag_days)

# ---- Delayed vs on-time (binary) ----
table(df$delayed_reporting)
prop.table(table(df$delayed_reporting))

# Interpretation note:
# Expect substantial right skew and class imbalance.
# This justifies tree-based models and non-linear methods.

# ============================================================
# 4. VISUALIZE REPORTING LAG
# ============================================================

ggplot(df, aes(x = reporting_lag_days)) +
  geom_histogram(
    bins = 50,
    fill = "steelblue",
    color = "white"
  ) +
  labs(
    title = "Distribution of Results Reporting Lag",
    x = "Days from Primary Completion to Results Submission",
    y = "Number of Studies"
  ) +
  theme_minimal()

# ============================================================
# 5. KEY PREDICTOR SUMMARIES
# ============================================================

# ---- Trial phase ----
table(df$phase)
prop.table(table(df$phase))

# ---- Sponsor class ----
table(df$agency_class)
prop.table(table(df$agency_class))

# ---- Responsible party ----
table(df$responsible_party_type)

# ---- Operational complexity ----
summary(df$log_enrollment)
table(df$outcome_complexity)
table(df$multinational)

# ============================================================
# 6. BIVARIATE SIGNAL CHECKS
# ============================================================

# ---- Delay rate by sponsor class ----
df %>%
  group_by(agency_class) %>%
  summarise(
    n = n(),
    delay_rate = mean(delayed_reporting)
  ) %>%
  arrange(desc(delay_rate))

# ---- Delay rate by phase ----
df %>%
  group_by(phase) %>%
  summarise(
    n = n(),
    delay_rate = mean(delayed_reporting)
  )

# ============================================================
# 7. VISUAL SIGNAL CHECK (OPTIONAL)
# ============================================================

ggplot(df, aes(x = agency_class, y = reporting_lag_days)) +
  geom_boxplot(outlier.alpha = 0.1) +
  coord_cartesian(ylim = c(0, 1500)) +
  labs(
    title = "Reporting Lag by Sponsor Class",
    x = "Sponsor Class",
    y = "Reporting Lag (Days)"
  ) +
  theme_minimal()

# ============================================================
# 8. EDA CONCLUSION CHECKPOINT
# ============================================================

cat("EDA complete.\n")
cat("Observed heterogeneity supports predictive modeling.\n")

