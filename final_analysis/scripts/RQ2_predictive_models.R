# ============================================================
# RQ2 — Predictive Utility Testing (Classification Only)
# Governance vs Expanded Model
# Parallelized + DeLong + ROC + Feature Importance
# ============================================================

library(dplyr)
library(caret)
library(ranger)
library(readr)
library(pROC)
library(ggplot2)
library(doParallel)

set.seed(123)

# ============================================================
# Parallel Backend Setup
# ============================================================

num_cores <- parallel::detectCores() - 2
cl <- makeCluster(num_cores)
registerDoParallel(cl)

cat("Using", num_cores, "CPU cores for parallel processing.\n")

# ============================================================
# Create output folders
# ============================================================

dir.create("final_analysis/outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("final_analysis/outputs/plots", recursive = TRUE, showWarnings = FALSE)

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
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)

# ============================================================
# ROC Helper
# ============================================================

get_roc_metrics <- function(actual, probs) {
  roc_obj <- roc(
    response  = actual,
    predictor = probs,
    levels    = c("No","Yes"),
    direction = "<"
  )
  auc_val <- as.numeric(auc(roc_obj))
  auc_ci  <- ci.auc(roc_obj, method="delong")
  
  list(
    roc_obj = roc_obj,
    AUC = auc_val,
    AUC_Lower = as.numeric(auc_ci[1]),
    AUC_Upper = as.numeric(auc_ci[3])
  )
}

# ============================================================
# GOVERNANCE MODELS
# ============================================================

rf_gov <- train(
  delayed_reporting ~ . - reporting_lag_days,
  data = train_gov,
  method = "ranger",
  importance = "permutation",
  metric = "ROC",
  trControl = control_class,
  tuneLength = 3,
  num.trees = 500,
  num.threads = num_cores
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
  importance = "permutation",
  metric = "ROC",
  trControl = control_class,
  tuneLength = 3,
  num.trees = 500,
  num.threads = num_cores
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
# FEATURE IMPORTANCE PLOTS
# ============================================================

plot_importance <- function(model, title, filename, color) {
  imp <- varImp(model, scale = FALSE)$importance
  imp$Feature <- rownames(imp)
  colnames(imp)[1] <- "Importance"
  
  imp <- imp %>%
    arrange(desc(Importance)) %>%
    slice(1:15)
  
  p <- ggplot(imp,
              aes(x = reorder(Feature, Importance),
                  y = Importance)) +
    geom_bar(stat="identity", fill=color) +
    coord_flip() +
    labs(title=title,
         x="Feature",
         y="Permutation Importance") +
    theme_minimal()
  
  ggsave(paste0("final_analysis/outputs/plots/", filename),
         p, width=8, height=6)
}

plot_importance(rf_gov,
                "Figure 5. Feature Importance — Governance RF",
                "Figure5_Governance_Feature_Importance.png",
                "steelblue")

plot_importance(rf_full,
                "Figure 6. Feature Importance — Expanded RF",
                "Figure6_Expanded_Feature_Importance.png",
                "darkred")

# ============================================================
# CALIBRATION (Expanded RF)
# ============================================================

calibration_df <- data.frame(
  actual = ifelse(test_full$delayed_reporting=="Yes",1,0),
  predicted = rf_full_probs
)

calibration_summary <- calibration_df %>%
  mutate(decile = ntile(predicted,10)) %>%
  group_by(decile) %>%
  summarise(
    mean_predicted = mean(predicted),
    observed_rate = mean(actual),
    n = n()
  )

write_csv(calibration_summary,
          "final_analysis/outputs/tables/RQ2_Calibration_Expanded_RF.csv")

cal_plot <- ggplot(calibration_summary,
                   aes(x=mean_predicted,y=observed_rate)) +
  geom_point(size=3) +
  geom_abline(slope=1, intercept=0, linetype="dashed") +
  theme_minimal() +
  labs(title="Calibration Plot — Expanded RF",
       x="Mean Predicted Probability",
       y="Observed Rate")

ggsave("final_analysis/outputs/plots/RQ2_Calibration_Expanded_RF.png",
       cal_plot, width=7, height=5)

# ============================================================
# DeLong Tests
# ============================================================

delong_gov  <- roc.test(gov_logit_metrics$roc_obj,
                        gov_rf_metrics$roc_obj,
                        method="delong")

delong_full <- roc.test(full_logit_metrics$roc_obj,
                        full_rf_metrics$roc_obj,
                        method="delong")

write_csv(data.frame(
  Comparison=c("Governance: Logistic vs RF",
               "Expanded: Logistic vs RF"),
  p_value=c(delong_gov$p.value,
            delong_full$p.value)
),
"final_analysis/outputs/tables/RQ2_DeLong_Tests.csv")

# ============================================================
# Save AUC Table
# ============================================================

RQ2_results <- data.frame(
  Model=c("Logistic (Governance)",
          "RF Tuned (Governance)",
          "Logistic (Expanded)",
          "RF Tuned (Expanded)"),
  AUC=c(gov_logit_metrics$AUC,
        gov_rf_metrics$AUC,
        full_logit_metrics$AUC,
        full_rf_metrics$AUC),
  Lower=c(gov_logit_metrics$AUC_Lower,
          gov_rf_metrics$AUC_Lower,
          full_logit_metrics$AUC_Lower,
          full_rf_metrics$AUC_Lower),
  Upper=c(gov_logit_metrics$AUC_Upper,
          gov_rf_metrics$AUC_Upper,
          full_logit_metrics$AUC_Upper,
          full_rf_metrics$AUC_Upper)
)

write_csv(RQ2_results,
          "final_analysis/outputs/tables/RQ2_Classification_Results.csv")

# ============================================================
# ROC PLOTS
# ============================================================

png("final_analysis/outputs/plots/RQ2_ROC_Governance.png",800,600)
plot(gov_logit_metrics$roc_obj,col="blue",
     main="ROC — Governance Models")
plot(gov_rf_metrics$roc_obj,col="red",add=TRUE)
legend("bottomright",legend=c("Logistic","RF"),
       col=c("blue","red"),lwd=2)
dev.off()

png("final_analysis/outputs/plots/RQ2_ROC_Expanded.png",800,600)
plot(full_logit_metrics$roc_obj,col="blue",
     main="ROC — Expanded Models")
plot(full_rf_metrics$roc_obj,col="red",add=TRUE)
legend("bottomright",legend=c("Logistic","RF"),
       col=c("blue","red"),lwd=2)
dev.off()

cat("\nRQ2 Pipeline Complete (Parallelized).\n")

# ============================================================
# Stop Cluster
# ============================================================

stopCluster(cl)