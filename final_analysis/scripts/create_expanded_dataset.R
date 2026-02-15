# ============================================================
# Create Expanded Modeling Dataset
# Governance Indicators + Registry Features
# ============================================================

library(dplyr)
library(readr)

features <- read_csv("data/interim/aact_features.csv")

# ------------------------------------------------------------
# Remove ID column for modeling version (keep separate copy)
# ------------------------------------------------------------

expanded <- features

# ------------------------------------------------------------
# Convert logical to factor
# ------------------------------------------------------------

expanded$healthy_volunteers[is.na(expanded$healthy_volunteers)] <- "Missing"
expanded$healthy_volunteers <- factor(expanded$healthy_volunteers)


# ------------------------------------------------------------
# Convert character variables to factor
# ------------------------------------------------------------

char_vars <- names(expanded)[sapply(expanded, is.character)]

for (v in char_vars) {
  expanded[[v]][is.na(expanded[[v]])] <- "Missing"
  expanded[[v]] <- factor(expanded[[v]])
}

# ------------------------------------------------------------
# Handle numeric missing with median imputation
# ------------------------------------------------------------

num_vars <- names(expanded)[sapply(expanded, is.numeric)]

for (v in num_vars) {
  med <- median(expanded[[v]], na.rm = TRUE)
  expanded[[v]][is.na(expanded[[v]])] <- med
}

# ------------------------------------------------------------
# Final NA safety check
# ------------------------------------------------------------

stopifnot(sum(is.na(expanded)) == 0)

# ------------------------------------------------------------
# Save expanded dataset
# ------------------------------------------------------------

write_csv(
  expanded,
  "data/processed/expanded_dataset.csv"
)

cat("Expanded dataset created. Rows:", nrow(expanded), "\n")
