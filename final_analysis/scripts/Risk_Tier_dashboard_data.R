#-------------------------------------------------------------------------------
# Modeling data for Risk Tiers
#-------------------------------------------------------------------------------

# Load expanded dataset WITH nct_id
full_data_with_id <- read_csv(
  "data/processed/expanded_dataset.csv",
  show_col_types = FALSE
)

# Create modeling version without ID
model_data <- full_data_with_id %>%
  select(-nct_id, -reporting_lag_days)

model_data$delayed_reporting <- factor(model_data$delayed_reporting)

# Fit RF model (if not already available)
rf_full <- ranger(
  delayed_reporting ~ .,
  data = model_data,
  num.trees = 500,
  probability = TRUE,
  seed = 123
)

# Predict probabilities
rf_probs <- predict(rf_full, model_data)$predictions[,2]

# Attach probabilities back to dataset WITH ID
risk_output <- full_data_with_id %>%
  mutate(
    delay_risk_prob = rf_probs,
    risk_tier = case_when(
      delay_risk_prob >= 0.70 ~ "High",
      delay_risk_prob >= 0.40 ~ "Medium",
      TRUE ~ "Low"
    )
  )

# ============================================================
# RISK TIER CONSTRUCTION
# ============================================================

risk_output <- risk_output %>%
  mutate(
    structural_flag = ifelse(
      phase %in% c("PHASE3","PHASE4") | multinational == 1,
      "Yes","No"
    ),
    sponsor_flag = ifelse(
      agency_class != "Industry",
      "Yes","No"
    ),
    operational_flag = ifelse(
      log_enrollment > median(log_enrollment, na.rm = TRUE),
      "Yes","No"
    )
  )


# ------------------------------------------------------------
# Save Row-Level Governance Risk Dataset
# ------------------------------------------------------------

write_csv(
  risk_output %>%
    select(
      nct_id,
      phase,
      agency_class,
      delay_risk_prob,
      risk_tier,
      structural_flag,
      sponsor_flag,
      operational_flag
    ),
  "final_analysis/outputs/tables/RQ4_RiskTier_RowLevel_Dashboard.csv"
)

# Continue after properly closing the function
total_n <- nrow(risk_output)

# ------------------------------------------------------------
# Risk Tier Distribution Summary
# ------------------------------------------------------------

total_n <- nrow(risk_output)

risk_tier_summary <- risk_output %>%
  group_by(risk_tier) %>%
  summarise(
    n = n(),
    pct = round(n / total_n * 100, 2),
    avg_delay_prob = round(mean(delay_risk_prob, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_delay_prob))

write_csv(
  risk_tier_summary,
  "final_analysis/outputs/tables/RQ4_RiskTier_Summary.csv"
)

#-------------------------------------------------------------------------------
# Monotonicity check
#-------------------------------------------------------------------------------

risk_output %>%
  group_by(risk_tier) %>%
  summarise(
    avg_structural_flag = mean(structural_flag == "Yes"),
    avg_sponsor_flag = mean(sponsor_flag == "Yes"),
    avg_operational_flag = mean(operational_flag == "Yes")
  )

# ------------------------------------------------------------
# Governance Flag Distribution by Risk Tier
# ------------------------------------------------------------

flag_distribution <- risk_output %>%
  group_by(risk_tier) %>%
  summarise(
    structural_flag_pct = round(mean(structural_flag == "Yes") * 100, 2),
    sponsor_flag_pct = round(mean(sponsor_flag == "Yes") * 100, 2),
    operational_flag_pct = round(mean(operational_flag == "Yes") * 100, 2),
    .groups = "drop"
  )

write_csv(
  flag_distribution,
  "final_analysis/outputs/tables/RQ4_Flag_Distribution_by_RiskTier.csv"
)

cat("\nRQ4 Risk_Tier_Dashboard Data Complete.\n")

