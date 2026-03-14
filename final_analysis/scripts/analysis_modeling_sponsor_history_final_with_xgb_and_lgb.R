# ============================================================
# Author: Abhishek Kadam
# Capstone Project
# Version: v1.0.0
# Date: 2026-03-08
# ANALYSIS MODELING PIPELINE (LEAKAGE-SAFE VERSION)
#
# Research Questions Covered:
#   RQ1: Association (Logistic Regression, Cox PH)
#   RQ2: Prediction (Random Forest – Classification & Regression)
#   RQ3: Interpretability (Permutation Importance)
#
# This script assumes:
#   data/processed/train_with_sponsor_history.csv
#   data/processed/test_with_sponsor_history.csv
#   were created using feature_engineering_sponsor_history_final.R
# ============================================================

# ---- Libraries ----
library(dplyr)
library(survival)
library(ranger)
library(pROC)
library(ggplot2)
library(xgboost)
library(lightgbm)
library(Matrix)


set.seed(123)


# ============================================================
# CREATE OUTPUT DIRECTORIES
# ============================================================

if (!dir.exists("final_analysis/outputs/tables")) {
  dir.create("final_analysis/outputs/tables", recursive = TRUE)
}

if (!dir.exists("final_analysis/outputs/figures")) {
  dir.create("final_analysis/outputs/figures", recursive = TRUE)
}

# ============================================================
# 1. LOAD LEAKAGE-SAFE DATA
# ============================================================

train <- read.csv("data/processed/train_with_sponsor_history.csv")
test  <- read.csv("data/processed/test_with_sponsor_history.csv")

# ============================================================
# FEATURE INTERACTIONS FOR BOOSTING MODELS
# ============================================================

create_interactions <- function(df){
  
  # Operational complexity × governance
  if(all(c("TOCI","SGMP") %in% colnames(df))){
    df$TOCI_SGMP <- df$TOCI * df$SGMP
  }
  
  # Sponsor history × complexity
  if(all(c("TOCI","sponsor_delay_rate") %in% colnames(df))){
    df$TOCI_SponsorHistory <- df$TOCI * df$sponsor_delay_rate
  }
  
  # Sponsor governance amplification
  if(all(c("SGMP","sponsor_delay_rate") %in% colnames(df))){
    df$Governance_Risk <- df$SGMP * df$sponsor_delay_rate
  }
  
  # Scale interaction for large trials
  if(all(c("num_sites","enrollment") %in% colnames(df))){
    df$Trial_Scale <- df$num_sites * df$enrollment
  }
  
  return(df)
}

train <- create_interactions(train)
test  <- create_interactions(test)


# Convert outcome
train$delayed_reporting <- as.numeric(train$delayed_reporting)
test$delayed_reporting  <- as.numeric(test$delayed_reporting)

# Classification factor version
train$delayed_reporting_f <- factor(train$delayed_reporting)
test$delayed_reporting_f  <- factor(test$delayed_reporting)

cat("Train rows:", nrow(train), "\n")
cat("Test rows :", nrow(test), "\n")

# ============================================================
# 2. RQ1 — ASSOCIATION MODELS (NO ARTIFICIAL BALANCING)
# ============================================================

cat("\n================ RQ1: Association Models ================\n")

# Logistic Regression
logit_model <- glm(
  delayed_reporting ~ .,
  data = train %>% select(-nct_id, -reporting_lag_days, -delayed_reporting_f),
  family = binomial
)

cat("\n--- Logistic Regression Summary ---\n")
print(summary(logit_model))

# Cox Proportional Hazards
cox_model <- coxph(
  Surv(reporting_lag_days, delayed_reporting) ~ .,
  data = train %>% select(-nct_id, -delayed_reporting_f)
)

cat("\n--- Cox PH Summary ---\n")
print(summary(cox_model))


# ============================================================
# 3. RQ2 — RANDOM FOREST CLASSIFICATION (BASELINE VS SPONSOR)
# ============================================================

cat("\n================ RQ2: Random Forest Comparison ================\n")

# -----------------------------
# BASELINE MODEL (STRUCTURAL ONLY)
# -----------------------------

baseline_vars <- train %>%
  select(-nct_id,
         -reporting_lag_days,
         -delayed_reporting,
         -delayed_reporting_f,
         -sponsor_delay_rate)

rf_base <- ranger(
  delayed_reporting_f ~ .,
  data = cbind(baseline_vars,
               delayed_reporting_f = train$delayed_reporting_f),
  num.trees = 2500,
  mtry = floor(sqrt(ncol(baseline_vars))),
  min.node.size = 5,
  importance = "permutation",
  probability = TRUE,
  seed = 123
)

rf_probs_base <- predict(
  rf_base,
  data = test %>% select(colnames(baseline_vars))
)$predictions[,2]

roc_base <- roc(test$delayed_reporting, rf_probs_base)
auc_base <- as.numeric(auc(roc_base))

cat("Baseline AUC:", round(auc_base, 4), "\n")

saveRDS(
  rf_base,
  "final_analysis/outputs/tables/rf_base_structural_model_final_v1.0.0.rds"
)

saveRDS(roc_base,
        "final_analysis/outputs/tables/roc_base_structural.rds")

# -----------------------------
# SPONSOR MODEL (STRUCTURAL + HISTORY) & Save Trained Model
# -----------------------------

sponsor_vars <- train %>%
  select(-nct_id,
         -reporting_lag_days,
         -delayed_reporting,
         -delayed_reporting_f)

rf_sponsor <- ranger(
  delayed_reporting_f ~ .,
  data = cbind(sponsor_vars,
               delayed_reporting_f = train$delayed_reporting_f),
  num.trees = 2500,
  mtry = floor(sqrt(ncol(sponsor_vars))),
  min.node.size = 5,
  importance = "permutation",
  probability = TRUE,
  seed = 123
)

rf_probs_sponsor <- predict(
  rf_sponsor,
  data = test %>% select(colnames(sponsor_vars))
)$predictions[,2]

roc_sponsor <- roc(test$delayed_reporting, rf_probs_sponsor)
auc_sponsor <- as.numeric(auc(roc_sponsor))

cat("Sponsor Model AUC:", round(auc_sponsor, 4), "\n")

rf_pred <- ifelse(rf_probs_sponsor > 0.5, 1, 0)

conf_matrix <- table(
  Predicted = rf_pred,
  Actual = test$delayed_reporting
)

print(conf_matrix)

rf_sponsor$forest$independent.variable.names
saveRDS(
  rf_sponsor,
  "final_analysis/outputs/tables/rf_sponsor_model_final_v1.0.0.rds"
)

saveRDS(roc_sponsor,
        "final_analysis/outputs/tables/roc_rf_sponsor.rds")

# ============================================================
# DELONG TEST
# ============================================================

delong_test <- roc.test(roc_base, roc_sponsor, method = "delong")

print(delong_test)

auc_diff <- auc_sponsor - auc_base
p_value  <- delong_test$p.value

ci_base <- ci.auc(roc_base)
ci_sponsor <- ci.auc(roc_sponsor)

delong_results <- data.frame(
  AUC_Baseline = auc_base,
  AUC_Sponsor_Model = auc_sponsor,
  AUC_Difference = auc_diff,
  CI_Base_Lower = ci_base[1],
  CI_Base_Upper = ci_base[3],
  CI_Sponsor_Lower = ci_sponsor[1],
  CI_Sponsor_Upper = ci_sponsor[3],
  DeLong_p_value = p_value
)

write.csv(
  delong_results,
  "final_analysis/outputs/tables/delong_test_results.csv",
  row.names = FALSE
)

cat("\nDeLong test results saved successfully.\n")

cat("\n================ RQ2: Boosted Models ================\n")

# ------------------------------------------------------------
# Prepare matrices for boosting models
# ------------------------------------------------------------

boost_train <- train %>%
  select(-nct_id,
         -reporting_lag_days,
         -delayed_reporting_f)

boost_test <- test %>%
  select(-nct_id,
         -reporting_lag_days,
         -delayed_reporting_f)


train_matrix <- model.matrix(delayed_reporting ~ . -1, data = boost_train)
test_matrix  <- model.matrix(delayed_reporting ~ . -1, data = boost_test)

dtrain <- xgb.DMatrix(data = train_matrix, label = boost_train$delayed_reporting)
dtest  <- xgb.DMatrix(data = test_matrix, label = boost_test$delayed_reporting)

# ------------------------------------------------------------
# XGBOOST MODEL
# ------------------------------------------------------------

params_xgb <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 4,
  eta = 0.02,
  subsample = 0.8,
  colsample_bytree = 0.7,
  min_child_weight = 5,
  gamma = 0.2
)

xgb_model <- xgb.train(
  params = params_xgb,
  data = dtrain,
  nrounds = 1200,
  verbose = 0
)

xgb_probs <- predict(xgb_model, dtest)

roc_xgb <- roc(test$delayed_reporting, xgb_probs)
auc_xgb <- as.numeric(auc(roc_xgb))

cat("XGBoost AUC:", round(auc_xgb, 4), "\n")

saveRDS(
  xgb_model,
  "final_analysis/outputs/tables/xgboost_model_final_v1.0.0.rds"
)

saveRDS(roc_xgb,
        "final_analysis/outputs/tables/roc_xgboost.rds")

# ------------------------------------------------------------
# LIGHTGBM MODEL
# ------------------------------------------------------------

dtrain_lgb <- lgb.Dataset(
  data = train_matrix,
  label = boost_train$delayed_reporting
)

params_lgb <- list(
  objective = "binary",
  metric = "auc",
  learning_rate = 0.02,
  num_leaves = 31,
  feature_fraction = 0.75,
  bagging_fraction = 0.8,
  bagging_freq = 5,
  min_data_in_leaf = 30
)

lgb_model <- lgb.train(
  params = params_lgb,
  data = dtrain_lgb,
  nrounds = 1200
)

lgb_probs <- predict(lgb_model, test_matrix)

roc_lgb <- roc(test$delayed_reporting, lgb_probs)
auc_lgb <- as.numeric(auc(roc_lgb))

cat("LightGBM AUC:", round(auc_lgb, 4), "\n")

saveRDS(
  lgb_model,
  "final_analysis/outputs/tables/lightgbm_model_final_v1.0.0.rds"
)

saveRDS(roc_lgb,
        "final_analysis/outputs/tables/roc_lightgbm.rds")

# ============================================================
# ADD LOGISTIC REGRESSION MODELS FOR ROC COMPARISON
# ============================================================

# -----------------------------
# LOGISTIC REGRESSION — BASELINE
# -----------------------------

logit_base <- glm(
  delayed_reporting ~ .,
  data = train %>%
    select(-nct_id,
           -reporting_lag_days,
           -delayed_reporting_f,
           -sponsor_delay_rate),
  family = binomial
)

logit_probs_base <- predict(
  logit_base,
  newdata = test %>%
    select(-nct_id,
           -reporting_lag_days,
           -delayed_reporting_f,
           -sponsor_delay_rate),
  type = "response"
)

roc_logit_base <- roc(test$delayed_reporting, logit_probs_base)
auc_logit_base <- as.numeric(auc(roc_logit_base))

saveRDS(
  logit_base,
  "final_analysis/outputs/tables/logit_base_model_final_v1.0.0.rds"
)

saveRDS(roc_logit_base,
        "final_analysis/outputs/tables/roc_logit_base.rds")


# -----------------------------
# LOGISTIC REGRESSION — SPONSOR
# -----------------------------

logit_sponsor <- glm(
  delayed_reporting ~ .,
  data = train %>%
    select(-nct_id,
           -reporting_lag_days,
           -delayed_reporting_f),
  family = binomial
)

logit_probs_sponsor <- predict(
  logit_sponsor,
  newdata = test %>%
    select(-nct_id,
           -reporting_lag_days,
           -delayed_reporting_f),
  type = "response"
)

roc_logit_sponsor <- roc(test$delayed_reporting, logit_probs_sponsor)
auc_logit_sponsor <- as.numeric(auc(roc_logit_sponsor))

cat("Logistic Baseline AUC:", round(auc_logit_base, 4), "\n")
cat("Logistic Sponsor AUC:", round(auc_logit_sponsor, 4), "\n")

saveRDS(
  logit_sponsor,
  "final_analysis/outputs/tables/logit_sponsor_model_final_v1.0.0.rds"
)

saveRDS(roc_logit_sponsor,
        "final_analysis/outputs/tables/roc_logit_sponsor.rds")

# ============================================================
# CREATE FIGURE DIRECTORY
# ============================================================

if (!dir.exists("final_analysis/outputs/figures")) {
  dir.create("final_analysis/outputs/figures", recursive = TRUE)
}


# ============================================================
# ROC PLOT 1 — STRUCTURAL MODELS
# RF vs Logistic
# ============================================================

png("final_analysis/outputs/figures/ROC_Structural_RF_vs_LR.png", width = 1200, height = 900, res = 150)

plot(roc_base,
     col = "blue",
     lwd = 3,
     main = "ROC Curve — Structural Models",
     legacy.axes = TRUE)

plot(roc_logit_base,
     col = "red",
     lwd = 3,
     add = TRUE)

legend("bottomright",
       legend = c(
         paste0("RF (AUC = ", round(auc_base, 3), ")"),
         paste0("LR (AUC = ", round(auc_logit_base, 3), ")")
       ),
       col = c("blue", "red"),
       lwd = 3)

dev.off()


# ============================================================
# ROC PLOT 2 — SPONSOR-ENHANCED MODELS
# RF vs Logistic
# ============================================================

png("final_analysis/outputs/figures/ROC_Sponsor_RF_vs_LR.png", width = 1200, height = 900, res = 150)

plot(roc_sponsor,
     col = "darkgreen",
     lwd = 3,
     main = "ROC Curve — Sponsor-Enhanced Models",
     legacy.axes = TRUE)

plot(roc_logit_sponsor,
     col = "purple",
     lwd = 3,
     add = TRUE)

legend("bottomright",
       legend = c(
         paste0("RF (AUC = ", round(auc_sponsor, 3), ")"),
         paste0("LR (AUC = ", round(auc_logit_sponsor, 3), ")")
       ),
       col = c("darkgreen", "purple"),
       lwd = 3)

dev.off()

# ============================================================
# ROC PLOT 3 — ALL MODELS COMPARISON
# ============================================================

png("final_analysis/outputs/figures/ROC_All_Models_Comparison.png",
    width = 1200,
    height = 900,
    res = 150)

plot(roc_base,
     col = "blue",
     lwd = 3,
     main = "ROC Curve — All Models Comparison",
     legacy.axes = TRUE)

plot(roc_logit_base,
     col = "red",
     lwd = 3,
     add = TRUE)

plot(roc_sponsor,
     col = "darkgreen",
     lwd = 3,
     add = TRUE)

plot(roc_logit_sponsor,
     col = "purple",
     lwd = 3,
     add = TRUE)

plot(roc_xgb,
     col = "orange",
     lwd = 3,
     add = TRUE)

plot(roc_lgb,
     col = "black",
     lwd = 3,
     add = TRUE)

legend("bottomright",
       legend = c(
         paste0("RF Structural (AUC=", round(auc_base,3), ")"),
         paste0("LR Structural (AUC=", round(auc_logit_base,3), ")"),
         paste0("RF Sponsor (AUC=", round(auc_sponsor,3), ")"),
         paste0("LR Sponsor (AUC=", round(auc_logit_sponsor,3), ")"),
         paste0("XGBoost (AUC=", round(auc_xgb,3), ")"),
         paste0("LightGBM (AUC=", round(auc_lgb,3), ")")
       ),
       col = c("blue","red","darkgreen","purple","orange","black"),
       lwd = 3)

dev.off()



# ============================================================
# EXPORT AUC COMPARISON TABLE
# ============================================================

auc_comparison <- data.frame(
  Model = c(
    "RF Structural",
    "LR Structural",
    "RF Sponsor",
    "LR Sponsor",
    "XGBoost",
    "LightGBM"
  ),
  AUC = c(
    auc_base,
    auc_logit_base,
    auc_sponsor,
    auc_logit_sponsor,
    auc_xgb,
    auc_lgb
  )
)

write.csv(
  auc_comparison,
  "final_analysis/outputs/tables/auc_comparison_all_models.csv",
  row.names = FALSE
)

# ============================================================
# COMPUTE 95% CI FOR ALL MODELS
# ============================================================

ci_base <- ci.auc(roc_base)
ci_logit_base <- ci.auc(roc_logit_base)
ci_sponsor <- ci.auc(roc_sponsor)
ci_logit_sponsor <- ci.auc(roc_logit_sponsor)
ci_xgb <- ci.auc(roc_xgb)
ci_lgb <- ci.auc(roc_lgb)


#-------------------------------------------------
# Model summary with AUC + 95% CI
#-------------------------------------------------

model_summary <- data.frame(
  Model = c(
    "RF Structural",
    "RF Sponsor",
    "LR Structural",
    "LR Sponsor",
    "XGBoost",
    "LightGBM"
  ),
  
  AUC = c(
    auc_base,
    auc_sponsor,
    auc_logit_base,
    auc_logit_sponsor,
    auc_xgb,
    auc_lgb
  ),
  
  CI_Lower = c(
    ci_base[1],
    ci_sponsor[1],
    ci_logit_base[1],
    ci_logit_sponsor[1],
    ci_xgb[1],
    ci_lgb[1]
  ),
  
  CI_Upper = c(
    ci_base[3],
    ci_sponsor[3],
    ci_logit_base[3],
    ci_logit_sponsor[3],
    ci_xgb[3],
    ci_lgb[3]
  )
)

# ============================================================
# Create formatted AUC (95% CI) column for report
# ============================================================

model_summary$AUC_CI <- paste0(
  round(model_summary$AUC,3),
  " (",
  round(model_summary$CI_Lower,3),
  "-",
  round(model_summary$CI_Upper,3),
  ")"
)

write.csv(
  model_summary,
  "final_analysis/outputs/tables/model_comparison_all_models.csv",
  row.names = FALSE
)

write.csv(
  model_summary,
  "final_analysis/outputs/tables/model_comparison_all_models.csv",
  row.names = FALSE
)

cat("Model comparison table saved successfully.\n")

cat("\nROC curves and AUC comparison saved successfully.\n")

cat("\nAnalysis modeling pipeline completed successfully.\n")