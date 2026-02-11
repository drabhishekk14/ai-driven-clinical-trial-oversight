#------------------------------------------------------------------------------- 
# Load Libraries
#-------------------------------------------------------------------------------

library(dplyr)
library(caret)
library(ranger)
library(readr)

#------------------------------------------------------------------------------- 
# Load the final dataset
#-------------------------------------------------------------------------------
final <- read_csv("data/processed/final_dataset.csv")
stopifnot(sum(is.na(final)) == 0)

#------------------------------------------------------------------------------- 
# Train/Test Split, must be identical for both  the tasks
#-------------------------------------------------------------------------------

set.seed(123)

train_idx <- createDataPartition(
  final$delayed_reporting,
  p = 0.7,
  list = FALSE
)

train <- final[train_idx, ]
test  <- final[-train_idx, ]

#------------------------------------------------------------------------------- 
# Predicting Likelihood of Delay (Classification)
#-------------------------------------------------------------------------------

# **Baseline model: Logistic regression**

logit_base <- glm(
  delayed_reporting ~ TOCI + SGMP,
  data = train,
  family = binomial
)

logit_probs <- predict(logit_base, test, type = "response")
logit_pred  <- ifelse(logit_probs > 0.5, 1, 0)

# Evaluate baseline model

logit_cm <- confusionMatrix(
  factor(logit_pred),
  factor(test$delayed_reporting)
)

logit_auc <- pROC::auc(
  test$delayed_reporting,
  logit_probs
)

# ML model: Random Forest (primary)

train$delayed_reporting_f <- factor(train$delayed_reporting)
test$delayed_reporting_f  <- factor(test$delayed_reporting)

rf_class <- ranger(
  delayed_reporting_f ~ TOCI + SGMP,
  data = train,
  num.trees = 500,
  mtry = 2,
  min.node.size = 10,
  importance = "impurity",
  probability = TRUE,
  seed = 123
)

rf_probs <- predict(rf_class, test)$predictions[, 2]
rf_pred  <- ifelse(rf_probs > 0.5, 1, 0)

rf_cm <- confusionMatrix(
  factor(rf_pred),
  factor(test$delayed_reporting)
)

rf_auc <- pROC::auc(
  test$delayed_reporting,
  rf_probs
)

#------------------------------------------------------------------------------- 
# Save Classification Results Table R4
#-------------------------------------------------------------------------------
RQ2_class_results <- data.frame(
  Model = c("Logistic Regression", "Random Forest"),
  Accuracy = c(
    logit_cm$overall["Accuracy"],
    rf_cm$overall["Accuracy"]
  ),
  AUC = c(
    as.numeric(logit_auc),
    as.numeric(rf_auc)
  ),
  Precision = c(
    logit_cm$byClass["Precision"],
    rf_cm$byClass["Precision"]
  ),
  Recall = c(
    logit_cm$byClass["Recall"],
    rf_cm$byClass["Recall"]
  )
)

write_csv(
  RQ2_class_results,
  "outputs/tables/RQ2_Table_R4_Classification_Performance.csv"
)

#------------------------------------------------------------------------------- 
# Predicting Expected Delay (Regression)
#-------------------------------------------------------------------------------

#Baseline: Linear regression

lm_base <- lm(
  reporting_lag_days ~ TOCI + SGMP,
  data = train
)

lm_pred <- predict(lm_base, test)

lm_MAE  <- mean(abs(lm_pred - test$reporting_lag_days))
lm_RMSE <- sqrt(mean((lm_pred - test$reporting_lag_days)^2))

# ML model: Random Forest regression

rf_reg <- ranger(
  reporting_lag_days ~ TOCI + SGMP,
  data = train,
  num.trees = 500,
  mtry = 2,
  min.node.size = 10,
  importance = "impurity",
  seed = 123
)

rf_pred_days <- predict(rf_reg, test)$predictions

rf_MAE  <- mean(abs(rf_pred_days - test$reporting_lag_days))
rf_RMSE <- sqrt(mean((rf_pred_days - test$reporting_lag_days)^2))

#-------------------------------------------------------------------------------
# Save Regression Results (Table R5)
#-------------------------------------------------------------------------------

RQ2_reg_results <- data.frame(
  Model = c("Linear Regression", "Random Forest"),
  MAE = c(lm_MAE, rf_MAE),
  RMSE = c(lm_RMSE, rf_RMSE)
)

write_csv(
  RQ2_reg_results,
  "outputs/tables/RQ2_Table_R5_Regression_Performance.csv"
)


