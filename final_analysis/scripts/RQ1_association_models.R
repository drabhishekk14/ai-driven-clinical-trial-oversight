# ======================================================================
# RQ1 — Association Analysis
# Primary (Pre-COVID) + Sensitivity (Full Sample)+Inferential Statistics
# ======================================================================

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

library(dplyr)
library(survival)
library(broom)
library(readr)
library(tibble)

# ------------------------------------------------------------
# PRIMARY MODEL — PRE-COVID
# ------------------------------------------------------------

# =========================
# 1️ Logistic Regression
# =========================

logit_primary <- glm(
  delayed_reporting ~ TOCI + SGMP,
  data = pre_covid_data,
  family = binomial
)

# Odds Ratios table
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

# =========================
# Wald Test — Logistic
# =========================

wald_logit_primary <- summary(logit_primary)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("Predictor") %>%
  mutate(
    Wald_Z = Estimate / `Std. Error`,
    Wald_ChiSq = Wald_Z^2
  ) %>%
  select(
    Predictor,
    Estimate,
    `Std. Error`,
    Wald_Z,
    Wald_ChiSq,
    `Pr(>|z|)`
  )

write_csv(
  wald_logit_primary,
  "final_analysis/outputs/tables/RQ1_Table_R2A_Wald_Logistic_Primary.csv"
)

# =========================
# 2️ Cox Proportional Hazards
# =========================

cox_primary <- coxph(
  Surv(reporting_lag_days, delayed_reporting) ~ TOCI + SGMP,
  data = pre_covid_data
)

# Hazard Ratio table
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

# =========================
# Wald Test — Cox
# =========================

wald_cox_primary <- summary(cox_primary)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("Predictor") %>%
  mutate(
    Wald_Z = coef / `se(coef)`,
    Wald_ChiSq = Wald_Z^2
  ) %>%
  select(
    Predictor,
    coef,
    `se(coef)`,
    Wald_Z,
    Wald_ChiSq,
    `Pr(>|z|)`
  )

write_csv(
  wald_cox_primary,
  "final_analysis/outputs/tables/RQ1_Table_R3A_Wald_Cox_Primary.csv"
)

# ------------------------------------------------------------
# Proportional Hazards Test — Pre-COVID
# ------------------------------------------------------------

ph_test_primary <- cox.zph(cox_primary)

print(ph_test_primary)

# Optional: Save to CSV
write.csv(
  as.data.frame(ph_test_primary$table),
  "final_analysis/outputs/tables/RQ1_Table_R3B_PH_Test_Primary.csv"
)


# ------------------------------------------------------------
# SENSITIVITY MODEL — FULL SAMPLE + COVID CONTROL
# ------------------------------------------------------------

# =========================
# 3 Logistic Regression
# =========================

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

# ===========================
# Wald — Logistic Sensitivity
# ===========================

wald_logit_sensitivity <- summary(logit_sensitivity)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("Predictor") %>%
  mutate(
    Wald_Z = Estimate / `Std. Error`,
    Wald_ChiSq = Wald_Z^2
  ) %>%
  select(
    Predictor,
    Estimate,
    `Std. Error`,
    Wald_Z,
    Wald_ChiSq,
    `Pr(>|z|)`
  )

write_csv(
  wald_logit_sensitivity,
  "final_analysis/outputs/tables/RQ1_Table_R4A_Wald_Logistic_Sensitivity.csv"
)

# =========================
# 4 Cox Sensitivity
# =========================

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

# =======================
# Wald — Cox Sensitivity
# =======================

wald_cox_sensitivity <- summary(cox_sensitivity)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("Predictor") %>%
  mutate(
    Wald_Z = coef / `se(coef)`,
    Wald_ChiSq = Wald_Z^2
  ) %>%
  select(
    Predictor,
    coef,
    `se(coef)`,
    Wald_Z,
    Wald_ChiSq,
    `Pr(>|z|)`
  )

write_csv(
  wald_cox_sensitivity,
  "final_analysis/outputs/tables/RQ1_Table_R5A_Wald_Cox_Sensitivity.csv"
)

# ------------------------------------------------------------
# Proportional Hazards Test — Sensitivity
# ------------------------------------------------------------

ph_test_sensitivity <- cox.zph(cox_sensitivity)

print(ph_test_sensitivity)

write.csv(
  as.data.frame(ph_test_sensitivity$table),
  "final_analysis/outputs/tables/RQ1_Table_R5B_PH_Test_Sensitivity.csv"
)

#-------------------------------------------------------------------------------
# Create SGMP Groups (Quartiles)
#-------------------------------------------------------------------------------

full_data <- full_data %>%
  mutate(
    SGMP_group = ntile(SGMP, 4)
  )

#-------------------------------------------------------------------------------
# Summary Statistics by SGMP Group
#-------------------------------------------------------------------------------

sgmp_summary <- full_data %>%
  group_by(SGMP_group) %>%
  summarise(
    n = n(),
    mean_lag = mean(reporting_lag_days),
    median_lag = median(reporting_lag_days),
    pct_delayed = mean(delayed_reporting) * 100
  )

print(sgmp_summary)

write_csv(
  sgmp_summary,
  "final_analysis/outputs/tables/RQ1_Table_R6_SGMP_Lag_Distribution.csv"
)
#-------------------------------------------------------------------------------
# Visualize Distribution (Boxplot)
#-------------------------------------------------------------------------------

library(ggplot2)

ggplot(full_data, aes(x = factor(SGMP_group), y = reporting_lag_days)) +
  geom_boxplot() +
  labs(
    x = "SGMP Quartile",
    y = "Reporting Lag (Days)",
    title = "Distribution of Reporting Lag by SGMP Quartile"
  )



#-------------------------------------------------------------------------------
# Significance Testing
#-------------------------------------------------------------------------------

print(RQ1_logit_primary)
print(RQ1_cox_primary)

print(RQ1_logit_sensitivity)
print(RQ1_cox_sensitivity)


cat("\nRQ1 Association Models with Wald tests completed.\n")

