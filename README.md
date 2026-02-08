# AI-Driven Clinical Trial Oversight

This repository supports research into AI/ML-driven oversight of clinical trials, with a focus on results reporting timeliness, quality signals, and regulatory transparency.

## Data Source
- Aggregate Analysis of ClinicalTrials.gov (AACT) PostgreSQL database

## Research Focus
- Time lag in clinical trial results reporting
- Trial-level and sponsor-level determinants
- Implications for risk-based quality management (RBQM)

## Technology Stack
- R (DBI, RPostgres, Random Forest, SHAP)
- PostgreSQL / SQL
- GitHub for version control

## Reproducibility & Security
- Database credentials managed via environment variables
- Raw data excluded from version control
- Modular scripts for extraction, cleaning, and modeling

The analysis pipeline should be executed in the following order:

1. connect_aact.R – Configure AACT database connection
2. extract_aact.R – Extract raw registry data
3. feature_engineering.R – Clean and engineer features
4. eda.R – Perform exploratory data analysis and generate figures/tables
5. analysis_modeling_pipeline.R – Train models and evaluate performance

## Documentation
- Quick Reference Guide to Analytical Methods: `docs/Quick_Reference_Analytical_Methods.md`

## Status
Active development.

