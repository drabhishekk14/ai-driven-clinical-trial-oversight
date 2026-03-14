# ============================================================
# FEATURE ENGINEERING — SPONSOR HISTORICAL COMPLIANCE (SAFE)
# Author: Abhishek Kadam
# Capstone Project
# Version: v1.2.0
# Date: 2026-03-08
# 
# Purpose:
#   Create leakage-safe modeling datasets while also producing
#   a full interim dataset for exploratory data analysis (EDA).
#
# Outputs:
#   data/interim/aact_features_eda.csv
#   data/processed/train_with_sponsor_history.csv
#   data/processed/test_with_sponsor_history.csv
# ============================================================

library(dplyr)
library(lubridate)
library(caret)

set.seed(123)

# ============================================================
# 1. LOAD RAW DATA
# ============================================================

raw <- read.csv(
  "data/raw/aact_capstone_raw.csv",
  stringsAsFactors = FALSE
)

stopifnot(!any(duplicated(raw$nct_id)))

# ============================================================
# 2. DATE PARSING
# ============================================================

raw <- raw %>%
  mutate(
    primary_completion_date = as.Date(primary_completion_date),
    results_first_submitted_date = as.Date(results_first_submitted_date)
  )

# ============================================================
# 3. OUTCOME CONSTRUCTION
# ============================================================

raw <- raw %>%
  mutate(
    reporting_lag_days =
      as.numeric(results_first_submitted_date -
                   primary_completion_date),
    
    delayed_reporting =
      ifelse(reporting_lag_days > 365, 1, 0)
  ) %>%
  filter(
    !is.na(reporting_lag_days),
    reporting_lag_days >= 0,
    reporting_lag_days <= 5 * 365
  )

# Replace missing sponsor names
raw$lead_sponsor_name[is.na(raw$lead_sponsor_name)] <- "Unknown"

# ============================================================
# 4. BASIC FEATURE ENGINEERING
# ============================================================

raw <- raw %>%
  mutate(
    log_enrollment =
      ifelse(is.na(enrollment), 0, log1p(enrollment)),
    
    outcome_complexity =
      ifelse(is.na(num_outcomes), 0,
             ifelse(num_outcomes > 1, 1, 0)),
    
    multinational =
      ifelse(is.na(num_countries), 0,
             ifelse(num_countries > 1, 1, 0)),
    
    covid_period = as.numeric(covid_period)
  )

# ============================================================
# 4A. COMPOSITE INDICATORS FOR RQ1 ANALYSIS
# ============================================================

# ---- SGMP: Sponsor Governance Maturity Proxy ----

raw <- raw %>%
  group_by(agency_class, responsible_party_type) %>%
  mutate(
    sponsor_delay_rate_group =
      mean(delayed_reporting, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    SGMP = 1 - sponsor_delay_rate_group
  )

# ---- TOCI: Trial Operational Complexity Index ----

# ============================================================
# TOCI — Standardized Trial Operational Complexity Index
# ============================================================

raw <- raw %>%
  mutate(
    phase_clean = toupper(as.character(phase)),
    phase_clean = gsub(" ", "", phase_clean),
    
    phase_complexity =
      ifelse(
        !is.na(phase_clean) &
          phase_clean %in% c("PHASE3","PHASE4"),
        1,
        0
      )
  )

# Standardize log_enrollment

raw$log_enrollment_scaled <-
  scale(raw$log_enrollment)

# Construct TOCI

raw <- raw %>%
  mutate(
    TOCI =
      phase_complexity +
      log_enrollment_scaled +
      outcome_complexity +
      multinational
  )

cat("Missing TOCI values:", sum(is.na(raw$TOCI)), "\n")
stopifnot(sum(is.na(raw$TOCI)) == 0)
# ============================================================
# 5. CREATE INTERIM DATASET FOR EDA (FULL DATASET)
# ============================================================

eda_vars <- c(
  "nct_id",
  "reporting_lag_days",
  "delayed_reporting",
  "covid_period",
  "TOCI",
  "SGMP",
  "phase",
  "masking",
  "allocation",
  "intervention_type",
  "log_enrollment",
  "outcome_complexity",
  "multinational",
  "agency_class",
  "responsible_party_type",
  "gender",
  "healthy_volunteers"
)

eda_dataset <- raw %>%
  select(all_of(eda_vars))

if (!dir.exists("data/interim")) {
  dir.create("data/interim", recursive = TRUE)
}

write.csv(
  eda_dataset,
  "data/interim/aact_features_eda.csv",
  row.names = FALSE
)

cat("EDA dataset created successfully.\n")

# ============================================================
# CREATE RQ1 ANALYSIS DATASETS
# ============================================================

final_dataset <- raw %>%
  select(
    nct_id,
    reporting_lag_days,
    delayed_reporting,
    covid_period,
    TOCI,
    SGMP
  )

write.csv(
  final_dataset,
  "data/processed/final_dataset_RQ1.csv",
  row.names = FALSE
)

pre_covid_dataset <- final_dataset %>%
  filter(covid_period == 0)

write.csv(
  pre_covid_dataset,
  "data/processed/pre_covid_dataset.csv",
  row.names = FALSE
)

cat("RQ1 analysis datasets created successfully.\n")


# ============================================================
# 6. TRAIN / TEST SPLIT (BEFORE HISTORY COMPUTATION)
# ============================================================

train_idx <- createDataPartition(
  raw$delayed_reporting,
  p = 0.7,
  list = FALSE
)

train <- raw[train_idx, ]
test  <- raw[-train_idx, ]

# ============================================================
# 7. TRUE SPONSOR HISTORICAL COMPLIANCE (TRAIN ONLY)
# ============================================================

train <- train %>%
  arrange(lead_sponsor_name, primary_completion_date) %>%
  group_by(lead_sponsor_name) %>%
  mutate(
    sponsor_prior_trials = row_number() - 1,
    
    sponsor_prior_delays =
      lag(cumsum(delayed_reporting), default = 0),
    
    sponsor_delay_rate =
      ifelse(
        sponsor_prior_trials > 0,
        sponsor_prior_delays / sponsor_prior_trials,
        0
      )
  ) %>%
  ungroup()

# ============================================================
# 8. APPLY TRAIN-BASED SPONSOR RATES TO TEST
# ============================================================

sponsor_summary <- train %>%
  group_by(lead_sponsor_name) %>%
  summarise(
    sponsor_delay_rate = mean(delayed_reporting),
    .groups = "drop"
  )

test <- test %>%
  left_join(
    sponsor_summary,
    by = "lead_sponsor_name"
  )

test$sponsor_delay_rate[is.na(test$sponsor_delay_rate)] <- 0

# ============================================================
# 9. KEEP MODELING VARIABLES
# ============================================================

model_vars <- c(
  "nct_id",
  "reporting_lag_days",
  "delayed_reporting",
  "covid_period",
  "phase",
  "masking",
  "allocation",
  "intervention_type",
  "log_enrollment",
  "outcome_complexity",
  "multinational",
  "agency_class",
  "responsible_party_type",
  "gender",
  "healthy_volunteers",
  "sponsor_delay_rate"
)

train <- train %>% select(all_of(model_vars))
test  <- test  %>% select(all_of(model_vars))

# ============================================================
# 10. HANDLE MISSING CATEGORICAL VALUES (TRAIN-FITTED)
# ============================================================

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
  
  train[[v]][is.na(train[[v]])] <- "Missing"
  test[[v]][is.na(test[[v]])]  <- "Missing"
  
  train[[v]] <- factor(train[[v]])
  
  test[[v]] <- factor(
    test[[v]],
    levels = levels(train[[v]])
  )
}

# ============================================================
# 11. NUMERIC IMPUTATION (TRAIN ONLY)
# ============================================================

train_median <- median(train$log_enrollment, na.rm = TRUE)

train$log_enrollment[is.na(train$log_enrollment)] <- train_median
test$log_enrollment[is.na(test$log_enrollment)]  <- train_median

# ============================================================
# 12. FINAL NA VALIDATION
# ============================================================

stopifnot(sum(is.na(train)) == 0)
stopifnot(sum(is.na(test)) == 0)

# ============================================================
# 13. SAVE PROCESSED DATASETS
# ============================================================

if (!dir.exists("data/processed")) {
  dir.create("data/processed", recursive = TRUE)
}

write.csv(
  train,
  "data/processed/train_with_sponsor_history.csv",
  row.names = FALSE
)

write.csv(
  test,
  "data/processed/test_with_sponsor_history.csv",
  row.names = FALSE
)

# ============================================================
# 14. DATASET DIAGNOSTICS
# ============================================================

cat("Feature engineering completed successfully.\n")

cat("EDA dataset rows:", nrow(eda_dataset), "\n")
cat("Train rows:", nrow(train), "\n")
cat("Test rows :", nrow(test), "\n")

cat("\nMissing values in train dataset:\n")
print(colSums(is.na(train)))

cat("\nMissing values in test dataset:\n")
print(colSums(is.na(test)))

cat("EDA dataset creation and Feature engineering completed successfully\n")
