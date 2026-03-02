# ============================================================
# RQ2 — Predictive Utility Testing (FINAL STABLE VERSION)
# Governance vs Expanded Models
# ROC + DeLong + Importance (Impurity + Permutation)
# Calibration + Brier Score
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
# Parallel Setup
# ============================================================

num_cores <- parallel::detectCores() - 2
cl <- makeCluster(num_cores)
registerDoParallel(cl)

cat("Using", num_cores, "CPU cores.\n")

# ============================================================
# Output Folders
# ============================================================

dir.create("final_analysis/outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("final_analysis/outputs/plots", recursive = TRUE, showWarnings = FALSE)

# ============================================================
# Load Data
# ============================================================

gov_data  <- read_csv("data/processed/final_dataset.csv", show_col_types = FALSE)
full_data <- read_csv("data/processed/expanded_dataset.csv", show_col_types = FALSE)

gov_data  <- gov_data  %>% select(-nct_id)
full_data <- full_data %>% select(-nct_id)

# ============================================================
# Train/Test Split (same split for both datasets)
# ============================================================

train_idx <- createDataPartition(full_data$delayed_reporting, p = 0.7, list = FALSE)

train_gov  <- gov_data[train_idx, ]
test_gov   <- gov_data[-train_idx, ]

train_full <- full_data[train_idx, ]
test_full  <- full_data[-train_idx, ]

convert_factor <- function(df) {
  df$delayed_reporting <- factor(df$delayed_reporting,
                                 levels = c(0,1),
                                 labels = c("No","Yes"))
  df
}

train_gov  <- convert_factor(train_gov)
test_gov   <- convert_factor(test_gov)
train_full <- convert_factor(train_full)
test_full  <- convert_factor(test_full)

# ============================================================
# Cross-validation
# ============================================================

control_class <- trainControl(
  method="cv",
  number=5,
  classProbs=TRUE,
  summaryFunction=twoClassSummary,
  allowParallel=TRUE
)

# ============================================================
# ROC Helper
# ============================================================

get_roc_metrics <- function(actual, probs) {
  roc_obj <- roc(actual, probs,
                 levels=c("No","Yes"),
                 direction="<")
  auc_val <- as.numeric(auc(roc_obj))
  auc_ci  <- ci.auc(roc_obj, method="delong")
  list(
    roc_obj=roc_obj,
    AUC=auc_val,
    AUC_Lower=as.numeric(auc_ci[1]),
    AUC_Upper=as.numeric(auc_ci[3])
  )
}

# ============================================================
# MODEL FUNCTION
# ============================================================

run_models <- function(train_data, test_data, label_prefix) {
  
  # Logistic
  logit_model <- glm(
    delayed_reporting ~ . - reporting_lag_days,
    data=train_data,
    family=binomial
  )
  
  # RF for Prediction
  rf_predict <- train(
    delayed_reporting ~ . - reporting_lag_days,
    data=train_data,
    method="ranger",
    importance="none",
    metric="ROC",
    trControl=control_class,
    tuneLength=3,
    num.trees=500,
    num.threads=num_cores
  )
  
  # RF for Importance (Impurity)
  rf_impurity <- train(
    delayed_reporting ~ . - reporting_lag_days,
    data=train_data,
    method="ranger",
    importance="impurity_corrected",
    metric="ROC",
    trControl=control_class,
    tuneLength=3,
    num.trees=500,
    num.threads=num_cores
  )
  
  # RF for Importance (Permutation)
  rf_perm <- train(
    delayed_reporting ~ . - reporting_lag_days,
    data=train_data,
    method="ranger",
    importance="permutation",
    metric="ROC",
    trControl=control_class,
    tuneLength=3,
    num.trees=500,
    num.threads=num_cores
  )
  
  # Predictions
  rf_probs    <- predict(rf_predict, test_data, type="prob")[,"Yes"]
  logit_probs <- predict(logit_model, test_data, type="response")
  
  rf_metrics    <- get_roc_metrics(test_data$delayed_reporting, rf_probs)
  logit_metrics <- get_roc_metrics(test_data$delayed_reporting, logit_probs)
  
  delong_test <- roc.test(logit_metrics$roc_obj,
                          rf_metrics$roc_obj,
                          method="delong")
  
  # ROC Plot
  png(paste0("final_analysis/outputs/plots/RQ2_ROC_",label_prefix,".png"),900,700)
  
  plot(rf_metrics$roc_obj,
       col="red", lwd=3,
       legacy.axes=FALSE,
       main=paste("ROC Curve —",label_prefix))
  
  plot(logit_metrics$roc_obj,
       col="blue", lwd=3, add=TRUE)
  
  abline(a=0,b=1,lty=2,col="gray50",lwd=2)
  
  legend("bottomright",
         legend=c(
           paste0("RF (AUC=",round(rf_metrics$AUC,3),")"),
           paste0("Logistic (AUC=",round(logit_metrics$AUC,3),")"),
           paste0("ΔAUC=",round(rf_metrics$AUC-logit_metrics$AUC,3)),
           paste0("DeLong p=",signif(delong_test$p.value,3))
         ),
         col=c("red","blue",NA,NA),
         lwd=c(3,3,NA,NA),
         bty="n")
  
  dev.off()
  
  # Importance Data
  imp_df <- data.frame(
    Feature = names(rf_impurity$finalModel$variable.importance),
    Impurity = as.numeric(rf_impurity$finalModel$variable.importance),
    Permutation = as.numeric(rf_perm$finalModel$variable.importance)
  )
  
  write_csv(imp_df,
            paste0("final_analysis/outputs/tables/RF_Importance_",label_prefix,".csv"))
  
  # Plot Impurity
  top_imp <- imp_df[order(-imp_df$Impurity), ][1:10, ]
  
  p1 <- ggplot(top_imp,
               aes(x=reorder(Feature, Impurity), y=Impurity)) +
    geom_bar(stat="identity", fill="darkred") +
    coord_flip() +
    theme_minimal() +
    labs(title=paste("Feature Importance —",label_prefix,"(Impurity)"),
         x="Feature", y="Importance")
  
  ggsave(paste0("final_analysis/outputs/plots/RQ2_Importance_Impurity_",label_prefix,".png"),
         p1, width=8, height=6)
  
  # Plot Permutation
  top_perm <- imp_df[order(-imp_df$Permutation), ][1:10, ]
  
  p2 <- ggplot(top_perm,
               aes(x=reorder(Feature, Permutation), y=Permutation)) +
    geom_bar(stat="identity", fill="steelblue") +
    coord_flip() +
    theme_minimal() +
    labs(title=paste("Feature Importance —",label_prefix,"(Permutation)"),
         x="Feature", y="Importance")
  
  ggsave(paste0("final_analysis/outputs/plots/RQ2_Importance_Permutation_",label_prefix,".png"),
         p2, width=8, height=6)
  
  return(list(
    summary=data.frame(
      Model=label_prefix,
      AUC_Logistic=logit_metrics$AUC,
      AUC_RF=rf_metrics$AUC,
      Delta_AUC=rf_metrics$AUC-logit_metrics$AUC,
      DeLong_p=delong_test$p.value
    ),
    rf_predict_model=rf_predict
  ))
}

# ============================================================
# Run Models
# ============================================================

results_gov  <- run_models(train_gov,  test_gov,  "Governance")
results_full <- run_models(train_full, test_full, "Expanded")

# ============================================================
# Calibration — Expanded
# ============================================================

rf_full_probs <- predict(results_full$rf_predict_model,
                         test_full, type="prob")[,"Yes"]

calibration_df <- data.frame(
  actual = ifelse(test_full$delayed_reporting=="Yes",1,0),
  predicted = rf_full_probs
)

calibration_summary <- calibration_df %>%
  mutate(decile = ntile(predicted,10)) %>%
  group_by(decile) %>%
  summarise(
    mean_predicted = mean(predicted),
    observed_rate = mean(actual)
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
       y="Observed Delay Rate")

ggsave("final_analysis/outputs/plots/RQ2_Calibration_Expanded_RF.png",
       cal_plot, width=7, height=5)

brier_score <- mean((rf_full_probs - calibration_df$actual)^2)

# ============================================================
# Final Summary
# ============================================================

final_results <- bind_rows(results_gov$summary,
                           results_full$summary)

final_results$Brier_Score_Expanded_RF <- NA
final_results$Brier_Score_Expanded_RF[
  final_results$Model=="Expanded"] <- brier_score

write_csv(final_results,
          "final_analysis/outputs/tables/RQ2_Final_Model_Comparison.csv")

cat("\nRQ2 FINAL PIPELINE COMPLETE.\n")

stopCluster(cl)