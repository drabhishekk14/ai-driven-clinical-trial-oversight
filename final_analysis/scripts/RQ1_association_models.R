# ============================================================
# RQ1 — Association Analysis
# Primary (Pre-COVID) + Sensitivity (Full Sample)
# ============================================================

library(dplyr)
library(survival)
library(broom)
library(readr)

# ------------------------------------------------------------
# Create output directory if needed
# ------------------------------------------------------------

if (!dir.exists("final_analysis/outputs/tables")) {
  dir.create("final_analysis/outputs/tables", recursive = TRUE)
}

# ------------------------------------------------------------
# LOAD DATASETS
# ------------------------------------------------------------

full_data <- read_csv("data/processed/final_dataset.csv")
pre_covid_data <- read_csv("data/processed/pre_covid_dataset.csv")

# Safety check
stopifnot(sum(is.na(full_data)) == 0)
stopifnot(sum(is.na(pre_covid_data)) == 0)

cat("Full dataset rows:", nrow(full_data), "\n")
cat("Pre-COVID dataset rows:", nrow(pre_covid_data), "\n")

# ============================================================
# TABLE R1 — Descriptive Statistics (Full Sample)
# ============================================================

RQ1_desc <- full_data %>%
  summarise(
    mean_delay_days   = mean(reporting_lag_days),
    median_delay_days = median(reporting_lag_days),
    sd_delay_days     = sd(reporting_lag_days),
    pct_delayed       = mean(delayed_reporting) * 100,
    mean_TOCI         = mean(TOCI),
    sd_TOCI           = sd(TOCI),
    mean_SGMP         = mean(SGMP),
    sd_SGMP           = sd(SGMP)
  )

write_csv(
  RQ1_desc,
  "final_analysis/outputs/tables/RQ1_Table_R1_Descriptives_Full.csv"
)

# ============================================================
# PRIMARY MODEL — PRE-COVID
# ============================================================

# Logistic Regression (Pre-COVID)

logit_primary <- glm(
  delayed_reporting ~ TOCI + SGMP,
  data = pre_covid_data,
  family = binomial
)

RQ1_logit_primary <- tidy(
  logit_primary,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  select(
    Predictor = term,
    Odds_Ratio = estimate,
    CI_Lower = conf.low,
    CI_Upper = conf.high,
    p_value = p.value
  )

write_csv(
  RQ1_logit_primary,
  "final_analysis/outputs/tables/RQ1_Table_R2_Logistic_Primary_PreCOVID.csv"
)

# Cox Model (Pre-COVID)

cox_primary <- coxph(
  Surv(reporting_lag_days, delayed_reporting) ~ TOCI + SGMP,
  data = pre_covid_data
)

RQ1_cox_primary <- tidy(
  cox_primary,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  select(
    Predictor = term,
    Hazard_Ratio = estimate,
    CI_Lower = conf.low,
    CI_Upper = conf.high,
    p_value = p.value
  )

write_csv(
  RQ1_cox_primary,
  "final_analysis/outputs/tables/RQ1_Table_R3_Cox_Primary_PreCOVID.csv"
)

# ============================================================
# SENSITIVITY MODEL — FULL SAMPLE + COVID CONTROL
# ============================================================

# Logistic Regression (Full + COVID)

logit_sensitivity <- glm(
  delayed_reporting ~ TOCI + SGMP + covid_period,
  data = full_data,
  family = binomial
)

RQ1_logit_sensitivity <- tidy(
  logit_sensitivity,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  select(
    Predictor = term,
    Odds_Ratio = estimate,
    CI_Lower = conf.low,
    CI_Upper = conf.high,
    p_value = p.value
  )

write_csv(
  RQ1_logit_sensitivity,
  "final_analysis/outputs/tables/RQ1_Table_R4_Logistic_Sensitivity_Full.csv"
)

# Cox Model (Full + COVID)

cox_sensitivity <- coxph(
  Surv(reporting_lag_days, delayed_reporting) ~ TOCI + SGMP + covid_period,
  data = full_data
)

RQ1_cox_sensitivity <- tidy(
  cox_sensitivity,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  select(
    Predictor = term,
    Hazard_Ratio = estimate,
    CI_Lower = conf.low,
    CI_Upper = conf.high,
    p_value = p.value
  )

write_csv(
  RQ1_cox_sensitivity,
  "final_analysis/outputs/tables/RQ1_Table_R5_Cox_Sensitivity_Full.csv"
)

cat("\nRQ1 Primary and Sensitivity analyses complete.\n")
