# ============================================================
# RQ4 — Governance Risk Tier Dashboard (FINAL VERSION)
# Leakage-Safe | Sponsor-Enhanced | Visual Governance Layer
# ============================================================

library(dplyr)
library(ranger)
library(readr)
library(ggplot2)
library(scales)
library(tidyr)

set.seed(123)

# ============================================================
# Create Output Folders
# ============================================================

dir.create("final_analysis/outputs/tables",
           recursive = TRUE,
           showWarnings = FALSE)

dir.create("final_analysis/outputs/plots",
           recursive = TRUE,
           showWarnings = FALSE)

# ============================================================
# Load Train & Test
# ============================================================

train <- read_csv(
  "data/processed/train_with_sponsor_history.csv",
  show_col_types = FALSE
)

test <- read_csv(
  "data/processed/test_with_sponsor_history.csv",
  show_col_types = FALSE
)

# ============================================================
# Fix Categorical Variables
# ============================================================

cat_vars <- c(
  "phase",
  "masking",
  "allocation",
  "intervention_type",
  "agency_class",
  "responsible_party_type",
  "gender",
  "healthy_volunteers"
)

for(v in cat_vars){
  
  train[[v]] <- as.factor(train[[v]])
  
  test[[v]] <- factor(
    test[[v]],
    levels = levels(train[[v]])
  )
  
}

# ============================================================
# Reconstruct Full Dataset (For Governance Dashboard)
# ============================================================

full_data_with_id <- bind_rows(train, test)

# ============================================================
# Convert Outcome for MODEL TRAINING ONLY
# ============================================================

train$delayed_reporting <- factor(
  train$delayed_reporting,
  levels = c(0,1),
  labels = c("No","Yes")
)

# ============================================================
# Train Sponsor-Enhanced Random Forest
# ============================================================

train_model_data <- train %>%
  select(-nct_id, -reporting_lag_days)

rf_sponsor <- ranger(
  delayed_reporting ~ .,
  data = train_model_data,
  num.trees = 500,
  mtry = floor(sqrt(ncol(train_model_data)-1)),
  probability = TRUE,
  importance = "permutation",
  seed = 123
)

# ============================================================
# Generate Predictions for FULL Dataset
# ============================================================

prediction_data <- full_data_with_id %>%
  select(-nct_id, -reporting_lag_days, -delayed_reporting)

rf_preds <- predict(
  rf_sponsor,
  data = prediction_data
)

# Extract probability for "Yes"
prob_cols <- colnames(rf_preds$predictions)

if("Yes" %in% prob_cols){
  rf_probs_full <- rf_preds$predictions[, "Yes"]
} else {
  rf_probs_full <- rf_preds$predictions[,2]
}

# ============================================================
# Attach Predictions + Risk Tiers
# ============================================================

risk_output <- full_data_with_id %>%
  mutate(
    delay_risk_prob = rf_probs_full,
    risk_tier = case_when(
      delay_risk_prob >= 0.70 ~ "High",
      delay_risk_prob >= 0.40 ~ "Moderate",
      TRUE ~ "Low"
    )
  )

# ============================================================
# Risk Tier Summary Table
# ============================================================

risk_tier_summary <- risk_output %>%
  group_by(risk_tier) %>%
  summarise(
    pct_trials = round(n()/nrow(risk_output) * 100, 2),
    observed_delay_rate = round(mean(delayed_reporting), 3),
    .groups = "drop"
  )

write_csv(
  risk_tier_summary,
  "final_analysis/outputs/tables/RQ4_RiskTier_Summary.csv"
)

print(risk_tier_summary)

# ============================================================
# Governance Flags
# ============================================================

risk_output <- risk_output %>%
  mutate(
    structural_flag = ifelse(
      phase %in% c("PHASE3","PHASE4") | multinational == 1,
      1, 0
    ),
    sponsor_flag = ifelse(
      sponsor_delay_rate >
        median(sponsor_delay_rate, na.rm = TRUE),
      1, 0
    ),
    operational_flag = ifelse(
      log_enrollment >
        median(log_enrollment, na.rm = TRUE),
      1, 0
    )
  )

# ============================================================
# Save Row-Level Dashboard Dataset
# ============================================================

write_csv(
  risk_output %>%
    select(
      nct_id,
      phase,
      agency_class,
      sponsor_delay_rate,
      delay_risk_prob,
      risk_tier,
      structural_flag,
      sponsor_flag,
      operational_flag
    ),
  "final_analysis/outputs/tables/RQ4_RiskTier_RowLevel_Dashboard.csv"
)

# ============================================================
# 1️⃣ Risk Tier Distribution Plot
# ============================================================

tier_dist <- risk_output %>%
  count(risk_tier)

p1 <- ggplot(tier_dist,
             aes(x = risk_tier, y = n, fill = risk_tier)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Risk Tier Distribution",
       x = "Risk Tier",
       y = "Number of Trials") +
  theme(legend.position = "none")

ggsave("final_analysis/outputs/plots/RQ4_RiskTier_Distribution.png",
       p1, width = 7, height = 5)

# ============================================================
# 2️⃣ Observed Delay Rate by Tier
# ============================================================

tier_observed <- risk_output %>%
  group_by(risk_tier) %>%
  summarise(
    observed_delay_rate = mean(delayed_reporting),
    .groups = "drop"
  )

p2 <- ggplot(tier_observed,
             aes(x = risk_tier,
                 y = observed_delay_rate,
                 fill = risk_tier)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = percent_format()) +
  theme_minimal() +
  labs(title = "Observed Delay Rate by Risk Tier",
       x = "Risk Tier",
       y = "Observed Delay Rate") +
  theme(legend.position = "none")

ggsave("final_analysis/outputs/plots/RQ4_Observed_Delay_by_Tier.png",
       p2, width = 7, height = 5)

# ============================================================
# 3️⃣ Predicted vs Observed Alignment
# ============================================================

tier_compare <- risk_output %>%
  group_by(risk_tier) %>%
  summarise(
    mean_predicted = mean(delay_risk_prob),
    observed = mean(delayed_reporting),
    .groups = "drop"
  )

p3 <- ggplot(tier_compare,
             aes(x = mean_predicted,
                 y = observed,
                 label = risk_tier)) +
  geom_point(size = 4) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed") +
  geom_text(nudge_y = 0.02) +
  theme_minimal() +
  labs(title = "Predicted vs Observed Delay by Tier",
       x = "Mean Predicted Probability",
       y = "Observed Delay Rate")

ggsave("final_analysis/outputs/plots/RQ4_Predicted_vs_Observed.png",
       p3, width = 7, height = 5)

# ============================================================
# 4️⃣ Sponsor Historical Delay by Tier
# ============================================================

p4 <- ggplot(risk_output,
             aes(x = risk_tier,
                 y = sponsor_delay_rate,
                 fill = risk_tier)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Sponsor Historical Delay Rate by Risk Tier",
       x = "Risk Tier",
       y = "Sponsor Delay Rate") +
  theme(legend.position = "none")

ggsave("final_analysis/outputs/plots/RQ4_SponsorHistory_by_Tier.png",
       p4, width = 7, height = 5)

# ============================================================
# 5️⃣ Probability Distribution Plot
# ============================================================

p5 <- ggplot(risk_output,
             aes(x = delay_risk_prob)) +
  geom_histogram(bins = 30,
                 fill = "steelblue",
                 color = "white") +
  theme_minimal() +
  labs(title = "Distribution of Predicted Delay Risk",
       x = "Predicted Probability",
       y = "Count")

ggsave("final_analysis/outputs/plots/RQ4_Probability_Distribution.png",
       p5, width = 7, height = 5)

# ============================================================
# 6️⃣ Governance Flag Heatmap
# ============================================================

flag_summary <- risk_output %>%
  group_by(risk_tier) %>%
  summarise(
    structural = mean(structural_flag),
    sponsor = mean(sponsor_flag),
    operational = mean(operational_flag),
    .groups = "drop"
  )

flag_long <- pivot_longer(
  flag_summary,
  cols = c(structural, sponsor, operational),
  names_to = "Flag",
  values_to = "Proportion"
)

p6 <- ggplot(flag_long,
             aes(x = Flag,
                 y = risk_tier,
                 fill = Proportion)) +
  geom_tile() +
  scale_fill_gradient(low = "white",
                      high = "darkred") +
  theme_minimal() +
  labs(title = "Governance Flag Intensity by Risk Tier",
       x = "Flag Type",
       y = "Risk Tier")

ggsave("final_analysis/outputs/plots/RQ4_Governance_Heatmap.png",
       p6, width = 7, height = 5)

cat("\nRQ4 Governance Risk Tier Dashboard COMPLETE.\n")