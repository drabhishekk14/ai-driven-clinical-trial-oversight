# ============================================================
# RQ3 — Interpretability & Governance Translation
# Governance RF vs Expanded RF
# ============================================================

library(dplyr)
library(ranger)
library(readr)

if (!dir.exists("final_analysis/outputs/tables")) {
  dir.create("final_analysis/outputs/tables", recursive = TRUE)
}

# ------------------------------------------------------------
# Load datasets
# ------------------------------------------------------------

gov_data  <- read_csv("data/processed/final_dataset.csv", show_col_types = FALSE)
full_data <- read_csv("data/processed/expanded_dataset.csv", show_col_types = FALSE)

gov_data  <- gov_data  %>% select(-nct_id)
full_data <- full_data %>% select(-nct_id)

# Convert outcome to factor
gov_data$delayed_reporting  <- factor(gov_data$delayed_reporting, levels=c(0,1))
full_data$delayed_reporting <- factor(full_data$delayed_reporting, levels=c(0,1))

set.seed(123)

# ============================================================
# GOVERNANCE-ONLY RF MODEL
# ============================================================

rf_gov <- ranger(
  delayed_reporting ~ TOCI + SGMP + covid_period,
  data = gov_data,
  num.trees = 500,
  importance = "permutation",
  probability = TRUE,
  seed = 123
)

gov_importance <- data.frame(
  Feature = names(rf_gov$variable.importance),
  Importance = rf_gov$variable.importance
) %>%
  arrange(desc(Importance))

write_csv(
  gov_importance,
  "final_analysis/outputs/tables/RQ3_Governance_Feature_Importance.csv"
)

# ============================================================
# EXPANDED RF MODEL (PRIMARY)
# ============================================================

rf_full <- ranger(
  delayed_reporting ~ . - reporting_lag_days,
  data = full_data,
  num.trees = 500,
  importance = "permutation",
  probability = TRUE,
  seed = 123
)

full_importance <- data.frame(
  Feature = names(rf_full$variable.importance),
  Importance = rf_full$variable.importance
) %>%
  arrange(desc(Importance))

write_csv(
  full_importance,
  "final_analysis/outputs/tables/RQ3_Expanded_Feature_Importance.csv"
)

# ============================================================
# GOVERNANCE VS EXPANDED COMPARISON
# ============================================================

top_features <- full_importance %>%
  slice(1:15)

write_csv(
  top_features,
  "final_analysis/outputs/tables/RQ3_Top15_Features.csv"
)

# ============================================================
# RISK TIER CONSTRUCTION
# ============================================================

rf_probs <- predict(rf_full, full_data)$predictions[,2]

risk_output <- full_data %>%
  mutate(
    delay_risk_prob = rf_probs,
    risk_tier = case_when(
      delay_risk_prob >= 0.70 ~ "High",
      delay_risk_prob >= 0.40 ~ "Medium",
      TRUE ~ "Low"
    )
  )

write_csv(
  risk_output %>% select(delay_risk_prob, risk_tier),
  "final_analysis/outputs/tables/RQ3_Risk_Tiers.csv"
)

cat("\nRQ3 Interpretability Analysis Complete.\n")
