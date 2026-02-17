# ============================================================
# RQ2 — Predictive Utility Testing (Classification Only)
# Governance vs Expanded Model
# With DeLong Tests + ROC Curves
# ============================================================

library(dplyr)
library(caret)
library(ranger)
library(readr)
library(pROC)
library(ggplot2)

set.seed(123)

# ============================================================
# Create output folders
# ============================================================

if (!dir.exists("final_analysis/outputs/tables")) {
  dir.create("final_analysis/outputs/tables", recursive = TRUE)
}

if (!dir.exists("final_analysis/outputs/plots")) {
  dir.create("final_analysis/outputs/plots", recursive = TRUE)
}

# ============================================================
# Load datasets
# ============================================================

gov_data  <- read_csv("data/processed/final_dataset.csv", show_col_types = FALSE)
full_data <- read_csv("data/processed/expanded_dataset.csv", show_col_types = FALSE)

gov_data  <- gov_data  %>% select(-nct_id)
full_data <- full_data %>% select(-nct_id)

# ============================================================
# Train/Test Split
# ============================================================

train_idx <- createDataPartition(
  gov_data$delayed_reporting,
  p = 0.7,
  list = FALSE
)

train_gov  <- gov_data[train_idx, ]
test_gov   <- gov_data[-train_idx, ]

train_full <- full_data[train_idx, ]
test_full  <- full_data[-train_idx, ]

convert_factor <- function(df) {
  df$delayed_reporting <- factor(
    df$delayed_reporting,
    levels = c(0,1),
    labels = c("No","Yes")
  )
  return(df)
}

train_gov  <- convert_factor(train_gov)
test_gov   <- convert_factor(test_gov)
train_full <- convert_factor(train_full)
test_full  <- convert_factor(test_full)

# ============================================================
# Cross-validation
# ============================================================

control_class <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

# ============================================================
# Helper Function
# ============================================================

get_roc_metrics <- function(actual, probs) {
  
  roc_obj <- roc(
    response  = actual,
    predictor = probs,
    levels    = c("No", "Yes"),
    direction = "<"
  )
  
  auc_value <- as.numeric(auc(roc_obj))
  auc_ci    <- ci.auc(roc_obj, method = "delong")
  
  return(list(
    roc_obj = roc_obj,
    AUC = auc_value,
    AUC_Lower = as.numeric(auc_ci[1]),
    AUC_Upper = as.numeric(auc_ci[3])
  ))
}

# ============================================================
# GOVERNANCE MODELS
# ============================================================

rf_gov <- train(
  delayed_reporting ~ . - reporting_lag_days,
  data = train_gov,
  method = "ranger",
  metric = "ROC",
  trControl = control_class,
  tuneLength = 5,
  num.trees = 500
)

logit_gov <- glm(
  delayed_reporting ~ . - reporting_lag_days,
  data = train_gov,
  family = binomial
)

rf_gov_probs    <- predict(rf_gov, test_gov, type="prob")[,"Yes"]
logit_gov_probs <- predict(logit_gov, test_gov, type="response")

gov_rf_metrics    <- get_roc_metrics(test_gov$delayed_reporting, rf_gov_probs)
gov_logit_metrics <- get_roc_metrics(test_gov$delayed_reporting, logit_gov_probs)

# ============================================================
# EXPANDED MODELS
# ============================================================

rf_full <- train(
  delayed_reporting ~ . - reporting_lag_days,
  data = train_full,
  method = "ranger",
  metric = "ROC",
  trControl = control_class,
  tuneLength = 5,
  num.trees = 500
)

logit_full <- glm(
  delayed_reporting ~ . - reporting_lag_days,
  data = train_full,
  family = binomial
)

rf_full_probs    <- predict(rf_full, test_full, type="prob")[,"Yes"]
logit_full_probs <- predict(logit_full, test_full, type="response")

full_rf_metrics    <- get_roc_metrics(test_full$delayed_reporting, rf_full_probs)
full_logit_metrics <- get_roc_metrics(test_full$delayed_reporting, logit_full_probs)

# ============================================================
# Calibration Check — Expanded Random Forest (Decile-Based)
# ============================================================

library(dplyr)

# Create calibration dataframe
calibration_df <- data.frame(
  actual = test_full$delayed_reporting,
  predicted_prob = rf_full_probs
)

# Convert factor to numeric (Yes = 1, No = 0)
calibration_df$actual_numeric <- ifelse(calibration_df$actual == "Yes", 1, 0)

# Create deciles based on predicted probability
calibration_summary <- calibration_df %>%
  mutate(decile = ntile(predicted_prob, 10)) %>%
  group_by(decile) %>%
  summarise(
    mean_predicted = mean(predicted_prob),
    observed_rate = mean(actual_numeric),
    n = n()
  )

# Save calibration table
write_csv(
  calibration_summary,
  "final_analysis/outputs/tables/RQ2_Calibration_Expanded_RF.csv"
)

# Optional: Calibration Plot
calibration_plot <- ggplot(calibration_summary,
                           aes(x = mean_predicted,
                               y = observed_rate)) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Calibration Plot — Expanded Random Forest",
    x = "Mean Predicted Probability (Decile)",
    y = "Observed Delay Rate"
  ) +
  theme_minimal()

ggsave(
  "final_analysis/outputs/plots/RQ2_Calibration_Expanded_RF.png",
  calibration_plot,
  width = 7,
  height = 5
)

cat("Calibration analysis completed for Expanded RF model.\n")

# ============================================================
# DeLong Statistical Comparisons
# ============================================================

delong_gov <- roc.test(gov_logit_metrics$roc_obj,
                       gov_rf_metrics$roc_obj,
                       method="delong")

delong_full <- roc.test(full_logit_metrics$roc_obj,
                        full_rf_metrics$roc_obj,
                        method="delong")

# ============================================================
# Save Results Table
# ============================================================

RQ2_results <- data.frame(
  Model = c("Logistic (Governance)",
            "RF Tuned (Governance)",
            "Logistic (Expanded)",
            "RF Tuned (Expanded)"),
  AUC = c(gov_logit_metrics$AUC,
          gov_rf_metrics$AUC,
          full_logit_metrics$AUC,
          full_rf_metrics$AUC),
  AUC_Lower = c(gov_logit_metrics$AUC_Lower,
                gov_rf_metrics$AUC_Lower,
                full_logit_metrics$AUC_Lower,
                full_rf_metrics$AUC_Lower),
  AUC_Upper = c(gov_logit_metrics$AUC_Upper,
                gov_rf_metrics$AUC_Upper,
                full_logit_metrics$AUC_Upper,
                full_rf_metrics$AUC_Upper)
)

write_csv(
  RQ2_results,
  "final_analysis/outputs/tables/RQ2_Classification_Results.csv"
)

# Save DeLong p-values
write_csv(
  data.frame(
    Comparison = c("Governance: Logistic vs RF",
                   "Expanded: Logistic vs RF"),
    p_value = c(delong_gov$p.value,
                delong_full$p.value)
  ),
  "final_analysis/outputs/tables/RQ2_DeLong_Tests.csv"
)

# ============================================================
# Save ROC Plots
# ============================================================

png("final_analysis/outputs/plots/RQ2_ROC_Governance.png", width=800, height=600)
plot(gov_logit_metrics$roc_obj, col="blue", main="ROC - Governance Models")
plot(gov_rf_metrics$roc_obj, col="red", add=TRUE)
legend("bottomright", legend=c("Logistic","Random Forest"),
       col=c("blue","red"), lwd=2)
dev.off()

png("final_analysis/outputs/plots/RQ2_ROC_Expanded.png", width=800, height=600)
plot(full_logit_metrics$roc_obj, col="blue", main="ROC - Expanded Models")
plot(full_rf_metrics$roc_obj, col="red", add=TRUE)
legend("bottomright", legend=c("Logistic","Random Forest"),
       col=c("blue","red"), lwd=2)
dev.off()

cat("\nRQ2 Classification + DeLong Tests + ROC Plots Complete.\n")
