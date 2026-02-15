# ============================================================
# RISK TIER CONSTRUCTION
# ============================================================

risk_output <- risk_output %>%
  mutate(
    structural_risk = ifelse(
      phase %in% c("PHASE3","PHASE4") | multinational == 1,
      1, 0
    ),
    sponsor_risk = ifelse(
      agency_class != "Industry",
      1, 0
    ),
    operational_risk = ifelse(
      log_enrollment > median(log_enrollment),
      1, 0
    ),
    governance_risk_score = structural_risk +
      sponsor_risk +
      operational_risk
  )
# ------------------------------------------------------------
# Save Row-Level Governance Risk Dataset
# ------------------------------------------------------------

write_csv(
  risk_output %>%
    select(
      nct_id,
      phase,
      agency_class,
      delay_risk_prob,
      risk_tier,
      structural_risk,
      sponsor_risk,
      operational_risk,
      governance_risk_score
    ),
  "final_analysis/outputs/tables/RQ4_Governance_Risk_RowLevel.csv"
)

# ------------------------------------------------------------
# Governance Risk Score Distribution
# ------------------------------------------------------------

governance_summary <- risk_output %>%
  group_by(governance_risk_score) %>%
  summarise(
    n = n(),
    pct = round(n / nrow(risk_output) * 100, 2),
    avg_delay_prob = round(mean(delay_risk_prob), 3)
  ) %>%
  arrange(desc(governance_risk_score))

write_csv(
  governance_summary,
  "final_analysis/outputs/tables/RQ4_Governance_Risk_Summary.csv"
)

