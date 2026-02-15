
#-------------------------------------------------------------------------------
#Checking for colinearlity
#-------------------------------------------------------------------------------

library(car)

#-------------------------------------------------------------------------------
# Loading data
#-------------------------------------------------------------------------------

full_data <- read_csv("data/processed/expanded_dataset.csv", show_col_types = FALSE)

full_data <- full_data %>%
  select(-nct_id, -reporting_lag_days)

full_data$delayed_reporting <- factor(full_data$delayed_reporting)

#-------------------------------------------------------------------------------
# Fit Logistic modle
#-------------------------------------------------------------------------------
logit_expanded <- glm(
  delayed_reporting ~ .,
  data = full_data,
  family = binomial
)
#-------------------------------------------------------------------------------
# Compute Variable importance
#-------------------------------------------------------------------------------
vif_values <- vif(logit_expanded)

# Convert to data frame properly
vif_table <- as.data.frame(vif_values)

# Add variable names
vif_table$Variable <- rownames(vif_table)

# If factors exist, compute adjusted GVIF
if ("GVIF" %in% colnames(vif_table)) {
  vif_table$Adjusted_VIF <- vif_table$GVIF^(1/(2*vif_table$Df))
} else {
  vif_table$Adjusted_VIF <- vif_table$VIF
}

# Sort by adjusted VIF
vif_table <- vif_table[order(-vif_table$Adjusted_VIF), ]

print(vif_table)






