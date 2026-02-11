# ============================================================
# Feature Engineering Pipeline (FINAL)
#
# Purpose:
#   Transform raw AACT registry data into a reproducible,
#   analytically robust dataset for modeling clinical trial
#   results reporting delays.
#
# Outputs:
#   1. data/interim/aact_features.csv   (engineered features)
#   2. data/processed/final_dataset.csv (frozen analysis dataset)
#
# ============================================================

# ---- Libraries ----
library(dplyr)
library(lubridate)

# ============================================================
# 1. LOAD RAW DATA
# ============================================================

raw <- read.csv(
  "data/raw/aact_capstone_raw.csv",
  stringsAsFactors = FALSE
)

# ============================================================
# 2. DATE PARSING
# ============================================================

date_vars <- c(
  "primary_completion_date",
  "results_first_submitted_date"
)

raw <- raw %>%
  mutate(across(all_of(date_vars), as.Date))

# ============================================================
# 3. OUTCOME CONSTRUCTION
# ============================================================

raw <- raw %>%
  mutate(
    reporting_lag_days =
      as.numeric(
        results_first_submitted_date -
          primary_completion_date
      )
  )

# ============================================================
# 4. ANALYTICAL ELIGIBILITY FILTERS
# ============================================================

analysis_df <- raw %>%
  filter(
    !is.na(reporting_lag_days),
    reporting_lag_days >= 0,
    reporting_lag_days <= 5 * 365
  )

# ============================================================
# 5. BINARY DELAY INDICATOR
# ============================================================

analysis_df <- analysis_df %>%
  mutate(
    delayed_reporting = ifelse(reporting_lag_days > 365, 1, 0)
  )

# ============================================================
# 6. HANDLE MISSING CATEGORICAL VALUES (UPSTREAM)
# ============================================================

analysis_df <- analysis_df %>%
  mutate(
    phase = ifelse(is.na(phase), "Unknown", as.character(phase)),
    responsible_party_type =
      ifelse(is.na(responsible_party_type), "Unknown", as.character(responsible_party_type)),
    
    phase = factor(phase),
    masking = factor(masking),
    allocation = factor(allocation),
    intervention_type = factor(intervention_type),
    agency_class = factor(agency_class),
    responsible_party_type = factor(responsible_party_type),
    gender = factor(gender),
    healthy_volunteers = factor(healthy_volunteers)
  )

# ============================================================
# 7. OPERATIONAL COMPLEXITY FEATURES
# ============================================================

analysis_df <- analysis_df %>%
  mutate(
    log_enrollment = log1p(enrollment),
    outcome_complexity = ifelse(num_outcomes > 1, 1, 0),
    multinational = ifelse(!is.na(country), 1, 0)
  )

# ============================================================
# 8. SPONSOR-LEVEL GOVERNANCE METRIC (SGMP)
# ============================================================

analysis_df <- analysis_df %>%
  group_by(agency_class, responsible_party_type) %>%
  mutate(
    sponsor_delay_rate = mean(delayed_reporting, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    SGMP = 1 - sponsor_delay_rate
  )

# ============================================================
# 9. COMPOSITE INDICATOR: TOCI
# ============================================================

analysis_df <- analysis_df %>%
  mutate(
    phase = toupper(phase),
    phase = gsub(" ", "", phase),
    
    log_enrollment =
      ifelse(is.na(log_enrollment),
             median(log_enrollment, na.rm = TRUE),
             log_enrollment),
    
    outcome_complexity =
      ifelse(is.na(outcome_complexity), 0, outcome_complexity),
    
    multinational =
      ifelse(is.na(multinational), 0, multinational),
    
    TOCI =
      as.numeric(phase %in% c("PHASE3", "PHASE4")) +
      log_enrollment +
      outcome_complexity +
      multinational
  )

# ============================================================
# 10. SAVE INTERIM DATASET (OPTIONAL, TRACEABILITY)
# ============================================================

interim_df <- analysis_df %>%
  select(
    nct_id,
    reporting_lag_days,
    delayed_reporting,
    phase,
    masking,
    allocation,
    intervention_type,
    log_enrollment,
    outcome_complexity,
    multinational,
    agency_class,
    responsible_party_type,
    gender,
    healthy_volunteers
  )

if (!dir.exists("data/interim")) {
  dir.create("data/interim", recursive = TRUE)
}

write.csv(
  interim_df,
  "data/interim/aact_features.csv",
  row.names = FALSE
)

# ============================================================
# 11. CREATE FINAL FROZEN DATASET
# ============================================================

final_dataset <- analysis_df %>%
  select(
    nct_id,
    reporting_lag_days,
    delayed_reporting,
    TOCI,
    SGMP,
    phase,
    agency_class,
    responsible_party_type
  )

if (!dir.exists("data/processed")) {
  dir.create("data/processed", recursive = TRUE)
}

write.csv(
  final_dataset,
  "data/processed/final_dataset.csv",
  row.names = FALSE
)

# ============================================================
# 12. FINAL VALIDATION CHECKS
# ============================================================

cat("Final dataset rows:", nrow(final_dataset), "\n")
cat("Missing values by column:\n")
print(colSums(is.na(final_dataset)))
summary(final_dataset$SGMP)
summary(final_dataset$TOCI)
