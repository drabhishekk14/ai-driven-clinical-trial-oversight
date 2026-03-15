# ==========================================================
# Minimum Sample Size Computation (No powerROC Required)
# ==========================================================

# install.packages("pwr")
library(pwr)

alpha <- 0.05
power_target <- 0.95

z_alpha <- qnorm(1 - alpha/2)
z_beta  <- qnorm(power_target)

# ==========================================================
# RQ1 — Logistic Regression (EPV Method)
# ==========================================================

num_predictors_rq1 <- 15
event_rate <- 0.60
epv_required <- 20

required_events_rq1 <- epv_required * num_predictors_rq1
N_rq1 <- ceiling(required_events_rq1 / event_rate)

# ==========================================================
# RQ2 — ROC AUC Power (Hanley & McNeil Approximation)
# ==========================================================

expected_auc <- 0.65
auc_null <- 0.5

# Assume balanced case-control for calculation
ratio <- 1

# Hanley & McNeil variance components
Q1 <- expected_auc / (2 - expected_auc)
Q2 <- (2 * expected_auc^2) / (1 + expected_auc)

# Sample size formula for equal groups
numerator <- (z_alpha + z_beta)^2 * 
  (expected_auc*(1-expected_auc) +
     (ratio)*(Q1 - expected_auc^2) +
     (ratio)*(Q2 - expected_auc^2))

denominator <- (expected_auc - auc_null)^2

n_cases <- numerator / denominator

# Adjust for real event rate (30%)
controls <- n_cases * (1 / event_rate - 1)
N_rq2 <- ceiling(n_cases + controls)

# ==========================================================
# RQ3 — Linear Regression (pwr.f2.test)
# ==========================================================

num_predictors_rq3 <- 15
effect_size_f2 <- 0.35

rq3_power <- pwr.f2.test(
  u = num_predictors_rq3,
  f2 = effect_size_f2,
  sig.level = alpha,
  power = power_target
)

N_rq3 <- ceiling(rq3_power$v + num_predictors_rq3 + 1)

# ==========================================================
# RQ4 — Chi-Square Test of Independence
# ==========================================================

# Medium effect size (Cohen’s w ≈ 0.30)
effect_size_w <- 0.50

rq4_power <- pwr.chisq.test(
  w = effect_size_w,
  df = 2,            # (3 tiers - 1)*(2 outcomes - 1) = 2
  sig.level = alpha,
  power = power_target
)

N_rq4 <- ceiling(rq4_power$N)

# ==========================================================
# Final Summary Table
# ==========================================================

summary_table <- data.frame(
  RQ = c("RQ1", "RQ2", "RQ3", "RQ4"),
  Model_Test = c(
    "Logistic Regression (EPV=10)",
    paste0("ROC AUC Test (Expected AUC=", expected_auc, ")"),
    "Linear Regression (f²=0.35)",
    "Chi-Square Test (Risk Tier × Delay)"
  ),
  Minimum_Sample_Size = c(N_rq1, N_rq2, N_rq3, N_rq4)
)

print(summary_table)
# ==========================================================
# Export Summary Table to CSV
# ==========================================================

write.csv(
  summary_table,
  "final_analysis/outputs/tables/Minimum_Sample_Size_Summary.csv",
)

cat("sample size calcuations completed successfully.\n")