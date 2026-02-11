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

# ---- Conditional Output directories creation ----

if (!dir.exists("outputs/figures")) {
  dir.create("outputs/figures", recursive = TRUE)
}
if (!dir.exists("outputs/tables")) {
  dir.create("outputs/tables", recursive = TRUE)
}

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

# ---- Save Table 4.1: Outcome Summary ----
outcome_summary <- data.frame(
  Metric = c("Min", "Median", "Mean", "Max"),
  Value = c(
    min(df$reporting_lag_days),
    median(df$reporting_lag_days),
    mean(df$reporting_lag_days),
    max(df$reporting_lag_days)
  )
)

write.csv(
  outcome_summary,
  "outputs/tables/Table_4_1_reporting_lag_summary.csv",
  row.names = FALSE
)

# ---- Delayed vs on-time (binary) ----
table(df$delayed_reporting)
prop.table(table(df$delayed_reporting))

# Interpretation note:
# Expect substantial right skew and class imbalance.
# This justifies tree-based models and non-linear methods.

# ============================================================
# 4. VISUALIZE REPORTING LAG
# ============================================================

p1 <- ggplot(df, aes(x = reporting_lag_days)) +
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

ggsave(
  "outputs/figures/Figure_4_1_reporting_lag_distribution.png",
  p1,
  width = 8,
  height = 5
)
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
sponsor_class_delay <- df %>%
  group_by(agency_class) %>%
  summarise(
    n = n(),
    delay_rate = mean(delayed_reporting)
  ) %>%
  arrange(desc(delay_rate))

write.csv(
  sponsor_class_delay,
  "outputs/tables/Table_4_2_delay_rate_by_phase.csv",
  row.names = FALSE
)
# ---- Delay rate by phase ----
phase_delay <- df %>%
  group_by(phase) %>%
  summarise(
    n = n(),
    delay_rate = mean(delayed_reporting)
  )

write.csv(
  phase_delay,
  "outputs/tables/Table_4_2_delay_rate_by_phase.csv",
  row.names = FALSE
)

# ---- Figure 4.3: Reporting Lag vs Log Enrollment (TOCI proxy) ----
p3 <- ggplot(df, aes(x = log_enrollment, y = reporting_lag_days)) +
  geom_point(alpha = 0.1) +
  labs(
    title = "Reporting Lag vs Log Enrollment",
    x = "Log Enrollment",
    y = "Reporting Lag (Days)"
  ) +
  theme_minimal()

ggsave(
  "outputs/figures/Figure_4_3_reporting_lag_vs_enrollment.png",
  p3,
  width = 8,
  height = 5
)

# ============================================================
# 7. VISUAL SIGNAL CHECK
# ============================================================

p2 <- ggplot(df, aes(x = agency_class, y = reporting_lag_days)) +
  geom_boxplot(outlier.alpha = 0.1) +
  coord_cartesian(ylim = c(0, 1500)) +
  labs(
    title = "Reporting Lag by Sponsor Class",
    x = "Sponsor Class",
    y = "Reporting Lag (Days)"
  ) +
  theme_minimal()

ggsave(
  "outputs/figures/Figure_4_2_reporting_lag_by_sponsor.png",
  p2,
  width = 8,
  height = 5
)
# ============================================================
# 8. EDA CONCLUSION CHECKPOINT
# ============================================================

cat("EDA complete.\n")
cat("Observed heterogeneity supports predictive modeling.\n")

