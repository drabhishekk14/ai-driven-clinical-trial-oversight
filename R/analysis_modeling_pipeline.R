# ============================================================
# Analysis Modeling Pipeline
#
# Research Questions Covered:
#   RQ1: Association (Logistic Regression, Cox PH)
#   RQ2: Prediction (Random Forest – Classification & Regression)
#   RQ3: Explainability (Feature Importance)
#
# ============================================================

# ---- Libraries ----
library(dplyr)
library(survival)
library(caret)
library(ranger)

# ============================================================
# 1. LOAD FEATURE-ENGINEERED DATA
# ============================================================

df <- read.csv(
  "data/interim/aact_features.csv",
  stringsAsFactors = FALSE
)

# ============================================================
# 2. PREPARE MODELING DATASET
# ============================================================

model_df <- df %>%
  select(-nct_id)

# Ensure outcome is numeric (required for Cox)
model_df$delayed_reporting <- as.numeric(model_df$delayed_reporting)

# ---- Handle categorical predictors ----
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

for (v in cat_vars) {
  model_df[[v]] <- as.character(model_df[[v]])
  model_df[[v]][is.na(model_df[[v]])] <- "Missing"
  model_df[[v]] <- factor(model_df[[v]])
}

# ---- Handle numeric predictors ----
model_df$log_enrollment[is.na(model_df$log_enrollment)] <-
  median(model_df$log_enrollment, na.rm = TRUE)

# ---- Safety check ----
stopifnot(sum(is.na(model_df)) == 0)

cat("ML dataset ready. Rows:", nrow(model_df), "\n")

# ============================================================
# BASELINE SAMPLE FOR STATISTICAL MODELS
# ============================================================

set.seed(123)

baseline_df <- model_df %>%
  group_by(delayed_reporting) %>%
  sample_n(size = min(25000, n()), replace = FALSE) %>%
  ungroup()

cat("Baseline models sample size:", nrow(baseline_df), "\n")

# ============================================================
# 3. RQ1 — ASSOCIATION MODELS
# ============================================================

# ---- Logistic regression ----
logit_model <- glm(
  delayed_reporting ~ .,
  data = baseline_df %>% select(-reporting_lag_days),
  family = binomial
)

cat("\n--- Logistic Regression Summary ---\n")
print(summary(logit_model))

# ---- Cox proportional hazards ----
cox_model <- coxph(
  Surv(reporting_lag_days, delayed_reporting) ~ .,
  data = baseline_df
)

cat("\n--- Cox Proportional Hazards Summary ---\n")
print(summary(cox_model))

# ============================================================
# 4. TRAIN / TEST SPLIT (ML)
# ============================================================

set.seed(123)

train_idx <- createDataPartition(
  model_df$delayed_reporting,
  p = 0.7,
  list = FALSE
)

train <- model_df[train_idx, ]
test  <- model_df[-train_idx, ]

# Factor outcome for classification only
train$delayed_reporting_f <- factor(train$delayed_reporting)
test$delayed_reporting_f  <- factor(test$delayed_reporting)

# ============================================================
# 5. RQ2 — RANDOM FOREST MODELS (RANGER)
# ============================================================

# ---- Classification ----
cat("\nStarting Ranger RF (classification)...\n")
t1 <- Sys.time()

rf_class <- ranger(
  delayed_reporting_f ~ .,
  data = train %>% select(-reporting_lag_days, -delayed_reporting),
  num.trees = 300,
  mtry = 4,
  min.node.size = 5,
  importance = "impurity",
  probability = TRUE,
  seed = 123
)

cat(
  "RF classification finished in",
  round(difftime(Sys.time(), t1, units = "mins"), 2),
  "minutes\n"
)

rf_probs <- predict(rf_class, test)$predictions[, 2]
rf_pred  <- ifelse(rf_probs > 0.5, 1, 0)

cat("\n--- Confusion Matrix ---\n")
cm <- caret::confusionMatrix(
  factor(rf_pred),
  factor(test$delayed_reporting)
)
print(cm)

# ---- Regression ----
cat("\nStarting Ranger RF (regression)...\n")
t2 <- Sys.time()

rf_reg <- ranger(
  reporting_lag_days ~ .,
  data = train %>% select(-delayed_reporting),
  num.trees = 300,
  mtry = 4,
  min.node.size = 5,
  importance = "impurity",
  seed = 123
)

cat(
  "RF regression finished in",
  round(difftime(Sys.time(), t2, units = "mins"), 2),
  "minutes\n"
)

rf_pred_days <- predict(rf_reg, test)$predictions

MAE  <- mean(abs(rf_pred_days - test$reporting_lag_days))
RMSE <- sqrt(mean((rf_pred_days - test$reporting_lag_days)^2))

cat("\n--- Regression Metrics ---\n")
cat("MAE :", round(MAE, 2), "\n")
cat("RMSE:", round(RMSE, 2), "\n")

# ============================================================
# 6. RQ3 — EXPLAINABILITY
# ============================================================

importance_df <- sort(
  rf_class$variable.importance,
  decreasing = TRUE
)

cat("\n--- Top 10 Important Features ---\n")
print(head(importance_df, 10))

# ============================================================
# 7. SAVE OUTPUTS FOR REPORTING
# ============================================================

if (!dir.exists("outputs/tables")) {
  dir.create("outputs/tables", recursive = TRUE)
}

write.csv(
  data.frame(
    MAE = MAE,
    RMSE = RMSE,
    Accuracy = cm$overall["Accuracy"],
    Kappa = cm$overall["Kappa"]
  ),
  "outputs/tables/model_performance.csv",
  row.names = FALSE
)

write.csv(
  data.frame(
    feature = names(importance_df),
    importance = importance_df
  ),
  "outputs/tables/feature_importance.csv",
  row.names = FALSE
)

cat("\nModeling complete.\n")
