# ============================================================
# Exploratory Data Analysis (EDA)
# Updated to reflect COVID indicator and final structure
# ============================================================

library(dplyr)
library(ggplot2)
library(readr)

# ------------------------------------------------------------
# Create Output Directories
# ------------------------------------------------------------

if (!dir.exists("final_analysis/outputs/figures")) {
  dir.create("final_analysis/outputs/figures", recursive = TRUE)
}

if (!dir.exists("final_analysis/outputs/tables")) {
  dir.create("final_analysis/outputs/tables", recursive = TRUE)
}

# ------------------------------------------------------------
# Load Data (Interim Feature Dataset)
# ------------------------------------------------------------

df <- read_csv("data/interim/aact_features.csv", show_col_types = FALSE)

cat("Number of studies:", nrow(df), "\n")
cat("Number of features:", ncol(df), "\n")

# ------------------------------------------------------------
# PRIMARY ANALYTICAL SAMPLE (Pre-COVID)
# ------------------------------------------------------------

df_pre_covid <- df %>%
  filter(covid_period == 0)

cat("Pre-COVID sample size:", nrow(df_pre_covid), "\n")

# ============================================================
# 1. OUTCOME DISTRIBUTIONS
# ============================================================

summary(df$reporting_lag_days)

outcome_summary <- data.frame(
  Metric = c("Min", "Median", "Mean", "Max"),
  Value = c(
    min(df$reporting_lag_days),
    median(df$reporting_lag_days),
    mean(df$reporting_lag_days),
    max(df$reporting_lag_days)
  )
)

write_csv(
  outcome_summary,
  "final_analysis/outputs/tables/Table_4_1_reporting_lag_summary.csv"
)

# Delay class balance
delay_dist <- prop.table(table(df$delayed_reporting))

write_csv(
  as.data.frame(delay_dist),
  "final_analysis/outputs/tables/Table_4_2_delay_class_distribution.csv"
)

# ============================================================
# 2. VISUALIZE REPORTING LAG
# ============================================================

p1 <- ggplot(df, aes(x = reporting_lag_days)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Results Reporting Lag",
    x = "Reporting Lag (Days)",
    y = "Number of Studies"
  ) +
  theme_minimal()

ggsave(
  "final_analysis/outputs/figures/Figure_4_1_reporting_lag_distribution.png",
  p1,
  width = 8,
  height = 5
)

# ============================================================
# 3. KEY PREDICTOR SUMMARIES
# ============================================================

phase_dist <- prop.table(table(df$phase))
agency_dist <- prop.table(table(df$agency_class))

write_csv(
  as.data.frame(phase_dist),
  "final_analysis/outputs/tables/Table_4_3_phase_distribution.csv"
)

write_csv(
  as.data.frame(agency_dist),
  "final_analysis/outputs/tables/Table_4_4_agency_distribution.csv"
)

# ============================================================
# 4. BIVARIATE SIGNAL CHECKS
# ============================================================

# Delay rate by phase
phase_delay <- df %>%
  group_by(phase) %>%
  summarise(
    n = n(),
    delay_rate = mean(delayed_reporting)
  ) %>%
  arrange(desc(delay_rate))

write_csv(
  phase_delay,
  "final_analysis/outputs/tables/Table_4_5_delay_rate_by_phase.csv"
)

# Delay rate by sponsor class
sponsor_delay <- df %>%
  group_by(agency_class) %>%
  summarise(
    n = n(),
    delay_rate = mean(delayed_reporting)
  ) %>%
  arrange(desc(delay_rate))

write_csv(
  sponsor_delay,
  "final_analysis/outputs/tables/Table_4_6_delay_rate_by_sponsor.csv"
)

# ============================================================
# 5. COVID EFFECT CHECK
# ============================================================

covid_delay <- df %>%
  group_by(covid_period) %>%
  summarise(
    n = n(),
    delay_rate = mean(delayed_reporting),
    mean_lag = mean(reporting_lag_days)
  )

write_csv(
  covid_delay,
  "final_analysis/outputs/tables/Table_4_7_covid_effect_summary.csv"
)

# ============================================================
# 6. VISUAL SIGNAL CHECKS
# ============================================================

# Lag vs enrollment
p2 <- ggplot(df, aes(x = log_enrollment, y = reporting_lag_days)) +
  geom_point(alpha = 0.1) +
  labs(
    title = "Reporting Lag vs Log Enrollment",
    x = "Log Enrollment",
    y = "Reporting Lag (Days)"
  ) +
  theme_minimal()

ggsave(
  "final_analysis/outputs/figures/Figure_4_2_lag_vs_enrollment.png",
  p2,
  width = 8,
  height = 5
)

# Lag by sponsor
p3 <- ggplot(df, aes(x = agency_class, y = reporting_lag_days)) +
  geom_boxplot(outlier.alpha = 0.1) +
  coord_cartesian(ylim = c(0, 1500)) +
  labs(
    title = "Reporting Lag by Sponsor Class",
    x = "Sponsor Class",
    y = "Reporting Lag (Days)"
  ) +
  theme_minimal()

ggsave(
  "final_analysis/outputs/figures/Figure_4_3_reporting_lag_by_sponsor.png",
  p3,
  width = 8,
  height = 5
)

# ============================================================
# 7. EDA CONCLUSION
# ============================================================

cat("EDA complete.\n")
cat("Substantial heterogeneity observed across phase and sponsor class.\n")
cat("COVID period exhibits measurable delay shift.\n")
