library(dplyr)
library(survival)
library(broom)
library(readr)

final <- read_csv("data/processed/final_dataset.csv")

# quick sanity check
stopifnot(sum(is.na(final)) == 0)
cat("Final dataset loaded. Rows:", nrow(final), "\n")

#-------------------------------------------------------------------------------
# Table R1 — Descriptive Statistics
#-------------------------------------------------------------------------------

RQ1_desc <- final %>%
  summarise(
    mean_delay_days   = mean(reporting_lag_days),
    median_delay_days = median(reporting_lag_days),
    sd_delay_days     = sd(reporting_lag_days),
    pct_delayed       = mean(delayed_reporting) * 100,
    mean_TOCI         = mean(TOCI),
    sd_TOCI           = sd(TOCI),
    mean_SGMP         = mean(SGMP),
    sd_SGMP           = sd(SGMP)
  )

write_csv(
  RQ1_desc,
  "outputs/tables/RQ1_Table_R1_Descriptives.csv"
)

#-------------------------------------------------------------------------------
# Logistic Regression (RQ1 — Association)
#-------------------------------------------------------------------------------

# Outcome: delayed_reporting (yes/no)
# Hypotheses covered
# H1a (TOCI ↑ → delay odds ↑)
# H1b (SGMP ↓ → delay odds ↑)


logit_RQ1 <- glm(
  delayed_reporting ~ TOCI + SGMP,
  data = final,
  family = binomial
)

RQ1_logit <- tidy(
  logit_RQ1,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  select(
    Predictor = term,
    Odds_Ratio = estimate,
    CI_Lower = conf.low,
    CI_Upper = conf.high,
    p_value = p.value
  )

write_csv(
  RQ1_logit,
  "outputs/tables/RQ1_Table_R2_Logistic.csv"
)

summary(logit_RQ1)

#-------------------------------------------------------------------------------
# Cox Proportional Hazards Model (Time-to-Reporting)
#-------------------------------------------------------------------------------

# Interpretation logic -
# Hazards Ratio < 1 → slower reporting
# Hazards Ratio > 1 → faster reporting

cox_RQ1 <- coxph(
  Surv(reporting_lag_days, delayed_reporting) ~ TOCI + SGMP,
  data = final
)

RQ1_cox <- tidy(
  cox_RQ1,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  select(
    Predictor = term,
    Hazard_Ratio = estimate,
    CI_Lower = conf.low,
    CI_Upper = conf.high,
    p_value = p.value
  )

write_csv(
  RQ1_cox,
  "outputs/tables/RQ1_Table_R3_Cox.csv"
)

summary(cox_RQ1)
