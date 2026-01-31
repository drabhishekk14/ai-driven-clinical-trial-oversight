# ============================================================
# Feature Engineering
#
# Purpose:
#   Transform raw AACT registry data into an analytically
#   usable dataset for modeling clinical trial results
#   reporting delays.
#
# Conceptual role in pipeline:
#   - Defines the outcome variable (reporting lag)
#   - Applies eligibility and plausibility filters
#   - Engineers interpretable predictors
#   - Produces a clean, model-ready interim dataset
#
#
# NOTE:
# The engineered features in this script directly support:
#   - Exploratory Data Analysis
#   - Predictive Modeling
#   - Construction of composite indicators
#
# Output:
#   data/interim/aact_features.csv
# ============================================================

# ---- Libraries ----
# dplyr: declarative data manipulation
# lubridate: robust date handling
library(dplyr)
library(lubridate)

# ============================================================
# 1. LOAD RAW DATA
# ============================================================
# Raw data represents untouched registry extracts from AACT.
# No exclusions or transformations have been applied prior
# to this point.
raw <- read.csv(
  "data/raw/aact_capstone_raw.csv",
  stringsAsFactors = FALSE
)

# ============================================================
# 2. DATE PARSING
# ============================================================
# Convert key registry date fields into Date objects.
# These fields form the temporal backbone for outcome
# construction.
date_vars <- c(
  "primary_completion_date",
  "results_first_submitted_date"
)

raw <- raw %>%
  mutate(across(all_of(date_vars), as.Date))

# ============================================================
# 3. OUTCOME CONSTRUCTION: REPORTING LAG
# ============================================================
# Reporting lag is defined as the number of days between
# primary completion and first submission of results.
#
# This operationalization aligns with FDAAA expectations
# and prior literature on results reporting compliance.
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
# Apply deterministic filters to remove records that
# cannot meaningfully contribute to the analysis.
#
# Rationale:
#   - Missing lag cannot be modeled
#   - Negative lag indicates registry inconsistencies
#   - Extremely large lags likely reflect data artifacts
analysis_df <- raw %>%
  filter(
    !is.na(reporting_lag_days),
    reporting_lag_days >= 0,
    reporting_lag_days <= 5 * 365   # cap at 5 years
  )

# ============================================================
# 5. BINARY COMPLIANCE OUTCOME
# ============================================================
# Create a binary indicator for delayed reporting.
#
# Threshold:
#   > 365 days post primary completion
#
# This threshold reflects FDAAA reporting timelines
# and is widely used in compliance studies.
analysis_df <- analysis_df %>%
  mutate(
    delayed_reporting =
      ifelse(reporting_lag_days > 365, 1, 0)
  )

# ============================================================
# 6. CATEGORICAL FEATURE STANDARDIZATION
# ============================================================
# Convert registry descriptors into factors to:
#   - Preserve interpretability
#   - Enable tree-based and regression models
#   - Support explainability methods (e.g., SHAP)
analysis_df <- analysis_df %>%
  mutate(
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
# 7. OPERATIONAL COMPLEXITY PROXIES
# ============================================================
# These features form the empirical foundation for
# composite indices such as TOCI.
#
# They are intentionally simple, transparent, and
# regulator-interpretable.
analysis_df <- analysis_df %>%
  mutate(
    # Enrollment often exhibits heavy right skew
    log_enrollment = log1p(enrollment),
    
    # Multiple outcomes increase reporting burden
    outcome_complexity =
      ifelse(num_outcomes > 1, 1, 0),
    
    # Presence of at least one country indicates
    # potential multinational operations
    multinational =
      ifelse(!is.na(country), 1, 0)
  )

# ============================================================
# 8. FINAL FEATURE SELECTION
# ============================================================
# Retain only variables required for:
#   - Descriptive analysis
#   - Composite indicator construction
#   - Predictive modeling
#
# This ensures a clear separation between
# raw registry fields and engineered features.
final_df <- analysis_df %>%
  select(
    nct_id,
    
    # outcomes
    reporting_lag_days,
    delayed_reporting,
    
    # trial design
    phase,
    masking,
    allocation,
    intervention_type,
    
    # operational scale
    log_enrollment,
    outcome_complexity,
    multinational,
    
    # governance
    agency_class,
    responsible_party_type,
    
    # eligibility
    gender,
    healthy_volunteers
  )

# ============================================================
# 9. SAVE INTERIM DATASET
# ============================================================
# The interim dataset is:
#   - Cleaned
#   - Feature-engineered
#   - Model-ready
#
# It remains reproducible and is excluded from
# version control by design.
if (!dir.exists("data/interim")) {
  dir.create("data/interim", recursive = TRUE)
}

write.csv(
  final_df,
  "data/interim/aact_features.csv",
  row.names = FALSE
)

cat(
  "Feature engineering complete:",
  nrow(final_df),
  "eligible studies\n"
)
