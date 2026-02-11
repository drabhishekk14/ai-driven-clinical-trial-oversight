#------------------------------------------------------------------------------- 
# Load Libraries
#-------------------------------------------------------------------------------

library(ranger)
library(dplyr)
library(readr)
library(rpart)
library(rpart.plot)

#------------------------------------------------------------------------------- 
# Load the final dataset
#-------------------------------------------------------------------------------

final <- read_csv("data/processed/final_dataset.csv")

# Ensure classification target is a factor (CRITICAL FIX)
final <- final %>%
  mutate(
    delayed_reporting_f = factor(
      delayed_reporting,
      levels = c(0, 1),
      labels = c("On-Time", "Delayed")
    )
  )

#-------------------------------------------------------------------------------
# Global Feature Importance (Model-level)
#-------------------------------------------------------------------------------

set.seed(123)

rf_class <- ranger(
  delayed_reporting_f ~ TOCI + SGMP,
  data = final,
  num.trees = 500,
  importance = "impurity",
  probability = TRUE
)

importance_df <- data.frame(
  Feature = names(rf_class$variable.importance),
  Importance = rf_class$variable.importance
) %>%
  arrange(desc(Importance))

write_csv(
  importance_df,
  "outputs/tables/RQ3_Table_R6_Feature_Importance.csv"
)

#-------------------------------------------------------------------------------
# Decision-Tree Surrogate (Human-Readable Rules)
#-------------------------------------------------------------------------------

# Ensure factor outcome (critical)
final <- final %>%
  mutate(
    delayed_reporting_f = factor(
      delayed_reporting,
      levels = c(0, 1),
      labels = c("On-Time", "Delayed")
    )
  )

# Train surrogate decision tree
tree_surrogate <- rpart(
  delayed_reporting_f ~ TOCI + SGMP,
  data = final,
  method = "class",
  control = rpart.control(maxdepth = 3, minbucket = 5000)
)

# Optional visualization
rpart.plot(tree_surrogate)

# Identify terminal (leaf) nodes
leaf_nodes <- rownames(
  tree_surrogate$frame[tree_surrogate$frame$var == "<leaf>", ]
)

# Extract rule paths
rule_list <- lapply(leaf_nodes, function(node) {
  path.rpart(tree_surrogate, nodes = as.numeric(node), print.it = FALSE)
})

# Build rule table
rule_table <- data.frame(
  Rule_ID = paste0("Rule_", seq_along(rule_list)),
  Rule = sapply(rule_list, function(x) paste(unlist(x), collapse = " AND ")),
  stringsAsFactors = FALSE
)

# Extract leaf information
leaf_info <- tree_surrogate$frame[leaf_nodes, ]

# ------------------------------------------------------------------------------
# Predicted class (robust)
# ------------------------------------------------------------------------------
rule_table$Predicted_Class <- ifelse(
  leaf_info$yval == 2,
  "Delayed",
  "On-Time"
)

# ------------------------------------------------------------------------------
# Probability of delayed reporting
# ------------------------------------------------------------------------------
rule_table$Probability <- round(
  leaf_info$yval2[, 5],
  3
)

# Save decision rules
write_csv(
  rule_table,
  "outputs/tables/RQ3_Table_R7_Decision_Rules.csv"
)

print(rule_table)

#-------------------------------------------------------------------------------
# Risk Tier Construction (Decision-Ready Output)
#-------------------------------------------------------------------------------

rf_probs <- predict(rf_class, final)$predictions[, "Delayed"]

final <- final %>%
  mutate(
    delay_risk_prob = rf_probs,
    risk_tier = case_when(
      delay_risk_prob >= 0.70 ~ "High",
      delay_risk_prob >= 0.40 ~ "Medium",
      TRUE ~ "Low"
    )
  )

write_csv(
  final %>% select(nct_id, delay_risk_prob, risk_tier),
  "outputs/tables/RQ3_Table_R8_Risk_Tiers.csv"
)
