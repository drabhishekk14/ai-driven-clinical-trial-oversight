# ============================================================
# RQ3 — Interpretability & Governance Translation
# FINAL VERSION (Leakage-Safe & Aligned with RQ2)
# Uses Sponsor-Enhanced Model from Train Set
# ============================================================

library(dplyr)
library(ranger)
library(readr)
library(ggplot2)

set.seed(123)

# ============================================================
# Output Folders
# ============================================================

dir.create("final_analysis/outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("final_analysis/outputs/plots", recursive = TRUE, showWarnings = FALSE)

# ============================================================
# Load Leakage-Safe Train/Test Data
# ============================================================

train <- read_csv("data/processed/train_with_sponsor_history.csv", show_col_types = FALSE)
test  <- read_csv("data/processed/test_with_sponsor_history.csv", show_col_types = FALSE)

# Convert outcome to factor
train$delayed_reporting <- factor(train$delayed_reporting,
                                  levels = c(0,1),
                                  labels = c("No","Yes"))

test$delayed_reporting <- factor(test$delayed_reporting,
                                 levels = c(0,1),
                                 labels = c("No","Yes"))

# ============================================================
# Train Sponsor-Enhanced RF Model (Same Spec as RQ2)
# ============================================================

sponsor_vars <- train %>%
  select(-nct_id,
         -reporting_lag_days)

rf_sponsor <- ranger(
  delayed_reporting ~ .,
  data = sponsor_vars,
  num.trees = 500,
  mtry = floor(sqrt(ncol(sponsor_vars)-1)),
  importance = "permutation",
  probability = TRUE,
  seed = 123
)

# ============================================================
# FEATURE IMPORTANCE (PERMUTATION)
# ============================================================

importance_df <- data.frame(
  Feature = names(rf_sponsor$variable.importance),
  Importance = as.numeric(rf_sponsor$variable.importance)
) %>%
  arrange(desc(Importance))

write_csv(
  importance_df,
  "final_analysis/outputs/tables/RQ3_Sponsor_RF_Feature_Importance.csv"
)

# Save Top 15
top15 <- importance_df %>% slice(1:15)

write_csv(
  top15,
  "final_analysis/outputs/tables/RQ3_Top15_Features.csv"
)

# ============================================================
# Plot Top 10 Importance
# ============================================================

top10 <- importance_df %>% slice(1:10)

p <- ggplot(top10,
            aes(x = reorder(Feature, Importance),
                y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 10 Predictors — Sponsor-Enhanced RF",
       x = "Feature",
       y = "Permutation Importance")

ggsave("final_analysis/outputs/plots/RQ3_Top10_Feature_Importance.png",
       p, width = 8, height = 6)

# ============================================================
# STABILITY CHECK (Optional but Strong)
# Retrain Model with Different Seed
# ============================================================

set.seed(456)

rf_sponsor_alt <- ranger(
  delayed_reporting ~ .,
  data = sponsor_vars,
  num.trees = 500,
  mtry = floor(sqrt(ncol(sponsor_vars)-1)),
  importance = "permutation",
  probability = TRUE
)

importance_alt <- data.frame(
  Feature = names(rf_sponsor_alt$variable.importance),
  Importance_Alt = as.numeric(rf_sponsor_alt$variable.importance)
)

importance_stability <- importance_df %>%
  left_join(importance_alt, by = "Feature")

write_csv(
  importance_stability,
  "final_analysis/outputs/tables/RQ3_Importance_Stability_Check.csv"
)

# ============================================================
# RISK TIER CONSTRUCTION (Test Set Only — No Leakage)
# ============================================================

rf_test_probs <- predict(
  rf_sponsor,
  data = test %>%
    select(colnames(sponsor_vars))
)$predictions[,2]

risk_output <- test %>%
  mutate(
    delay_risk_prob = rf_test_probs,
    risk_tier = case_when(
      delay_risk_prob >= 0.70 ~ "High",
      delay_risk_prob >= 0.40 ~ "Moderate",
      TRUE ~ "Low"
    )
  )

# Save tier assignment
write_csv(
  risk_output %>% select(delay_risk_prob, risk_tier),
  "final_analysis/outputs/tables/RQ3_Risk_Tiers_TestSet.csv"
)

# ============================================================
# Tier-Level Observed Rates (Validation)
# ============================================================

tier_summary <- risk_output %>%
  mutate(actual = ifelse(delayed_reporting=="Yes",1,0)) %>%
  group_by(risk_tier) %>%
  summarise(
    N = n(),
    Observed_Delay_Rate = mean(actual)
  ) %>%
  arrange(desc(Observed_Delay_Rate))

write_csv(
  tier_summary,
  "final_analysis/outputs/tables/RQ3_Risk_Tier_Observed_Rates.csv"
)

# ============================================================
# Chi-Square Test of Tier Separation
# ============================================================

tier_table <- table(risk_output$risk_tier,
                    risk_output$delayed_reporting)

chisq_tier <- chisq.test(tier_table)

write_csv(
  as.data.frame(chisq_tier$observed),
  "final_analysis/outputs/tables/RQ3_Tier_Observed_Counts.csv"
)

write_csv(
  data.frame(
    Chi_Square = as.numeric(chisq_tier$statistic),
    DF = as.numeric(chisq_tier$parameter),
    P_Value = chisq_tier$p.value
  ),
  "final_analysis/outputs/tables/RQ3_Tier_ChiSquare_Test.csv"
)

cat("\nRQ3 FINAL INTERPRETABILITY PIPELINE COMPLETE.\n")