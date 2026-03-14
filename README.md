# AI-Driven Clinical Trial Oversight 

This project is licensed under the MIT License.

**Predicting and Stratifying Delays in Clinical Trial Results Reporting**
*An Interpretable Machine Learning Framework for Governance Risk Stratification*

This repository contains the analytical pipeline supporting the capstone research project analyzing delays in clinical trial results reporting using data from **ClinicalTrials.gov**.

The study integrates **statistical inference, machine learning, and explainable analytics** to identify structural and governance drivers of reporting delays and translate predictive outputs into actionable governance risk tiers.

---

# Research Objectives

The project investigates four research questions:

### RQ1 — Determinants of Delay

Assess whether **trial operational complexity** and **sponsor governance characteristics** are associated with reporting delays using statistical models.

### RQ2 — Predictive Modeling

Evaluate whether **machine learning models improve prediction of reporting delays** relative to traditional regression approaches.

### RQ3 — Model Interpretability

Determine whether predictive models identify **stable and interpretable drivers of delay risk**.

### RQ4 — Governance Translation

Convert predicted delay probabilities into **operational risk tiers** supporting governance oversight aligned with **Risk-Based Quality Management (RBQM)**.

---

# Key Findings (TL;DR)

• Reporting delays remain common, with substantial variation across sponsor types and trial structures.
• **Sponsor historical compliance behavior** emerged as the strongest predictor of delay risk.
• Machine learning models improved predictive discrimination relative to regression baselines (AUC ≈ 0.68).
• Predicted delay probabilities can be translated into **Low, Moderate, and High governance risk tiers**, enabling proactive oversight of clinical trial reporting compliance.

---

# Data Source

The analysis uses data from:

**Aggregate Analysis of ClinicalTrials.gov (AACT)**
A publicly available relational database maintained by the **Clinical Trials Transformation Initiative (CTTI)**.

The dataset integrates multiple registry tables including:

* studies
* sponsors
* interventions
* facilities
* outcomes
* responsible_parties

The final analytic dataset contains approximately:

**71,934 completed interventional clinical trials**

---

# Analytical Framework

The analytical workflow combines **statistical modeling, machine learning, interpretability techniques, and governance-oriented risk stratification**.

### Statistical Analysis (RQ1)

* Descriptive statistics
* Logistic regression (probability of delay)
* Cox proportional hazards model (time-to-reporting)

### Predictive Modeling (RQ2)

* Random Forest classification
* Gradient boosting models (XGBoost, LightGBM)
* Model evaluation using ROC AUC

### Interpretability (RQ3)

* Permutation feature importance
* Identification of dominant delay drivers

### Governance Translation (RQ4)

* Risk tier stratification (Low / Moderate / High)
* Chi-square validation of risk tier separation

---

# Methodology Overview

The project implements an end-to-end analytical pipeline from registry data extraction to governance-oriented risk stratification.

![Clinical Trial Oversight Pipeline](docs\workflow_pipeline.png)

### Pipeline Stages

1. **Data Extraction**

   * ClinicalTrials.gov data retrieved from the AACT PostgreSQL database

2. **Feature Engineering**

   * Construction of governance and structural indicators:

     * Trial Operational Complexity Index (**TOCI**)
     * Sponsor Governance Maturity Proxy (**SGMP**)
     * Historical sponsor delay rate

3. **Statistical Modeling**

   * Logistic regression
   * Cox proportional hazards models

4. **Predictive Modeling**

   * Random Forest
   * XGBoost / LightGBM benchmarking

5. **Model Interpretability**

   * Permutation feature importance

6. **Governance Risk Stratification**

   * Conversion of predicted probabilities into risk tiers
   * Validation using chi-square testing

---

# Technology Stack

### Programming

* **R**

### Key Libraries

* DBI
* RPostgres
* randomForest
* xgboost
* lightgbm
* pROC
* tidyverse

### Database

* PostgreSQL (AACT)

### Version Control

* Git
* GitHub

---

# Reproducibility & Security

To ensure reproducible and secure analysis:

* Database credentials are stored using **environment variables**
* Raw AACT database files are **excluded from version control**
* Scripts are modularized for extraction, cleaning, and modeling
* The full analytical pipeline can be reproduced using the provided scripts

---

# Analysis Pipeline

The analytical workflow should be executed in the following order:

### 1. connect_aact.R

Configure connection to the AACT PostgreSQL database.

### 2. extract_aact.R

Extract raw clinical trial registry tables.

### 3. feature_engineering.R

Data cleaning, transformation, and feature construction including:

* TOCI
* SGMP
* sponsor_delay_rate

### 4. eda.R

Exploratory data analysis and generation of descriptive statistics and figures.

### 5. analysis_modeling_pipeline.R

Train predictive models, evaluate performance, and generate governance risk tiers.

---

# Repository Structure

```
AI-Driven-Clinical-Trial-Oversight
│
├── scripts
│   ├── connect_aact.R
│   ├── extract_aact.R
│   ├── feature_engineering.R
│   ├── eda.R
│   └── analysis_modeling_pipeline.R
│
├── figures
│   └── workflow_pipeline.png
│
├── docs
│   └── Quick_Reference_Analytical_Methods.md
│
├── outputs
│   └── tables, model summaries, visualizations
│
└── README.md
```

---

# Governance Insight

This project demonstrates how **interpretable machine learning can support proactive oversight of clinical trial reporting compliance**.

By combining structural trial characteristics with sponsor historical behavior, the framework enables:

* early identification of high-risk trials
* targeted oversight interventions
* portfolio-level compliance monitoring

The approach aligns with **Risk-Based Quality Management (RBQM)** principles used in modern clinical research governance.

---

# Documentation

Additional documentation available in:

```
docs/Quick_Reference_Analytical_Methods.md
```

This guide summarizes the statistical and machine learning methods used in the analysis.

---

# How to Run the Project Locally

### 1. Clone the Repository

```bash
git clone https://github.com/drabhishekk14/ai-driven-clinical-trial-oversight.git
cd ai-driven-clinical-trial-oversight
```

---

### 2. Install Required R Packages

Run the following in R:

```r
install.packages(c(
  "DBI",
  "RPostgres",
  "tidyverse",
  "randomForest",
  "xgboost",
  "lightgbm",
  "pROC"
))
```

---

### 3. Configure Database Connection

Set your AACT PostgreSQL credentials using environment variables.

Example `.Renviron` configuration:

```r
AACT_DB_HOST=localhost
AACT_DB_PORT=5432
AACT_DB_NAME=aact
AACT_DB_USER=your_username
AACT_DB_PASSWORD=your_password
```

---

### 4. Run the Analysis Pipeline

Execute the scripts in the following order:

```r
source("scripts/connect_aact.R")
source("scripts/extract_aact.R")
source("scripts/feature_engineering.R")
source("scripts/eda.R")
source("scripts/analysis_modeling_pipeline.R")
```

---

### 5. Review Outputs

Generated outputs include:

* descriptive statistics and figures
* model performance metrics
* feature importance plots
* governance risk tier classification

Outputs are stored in the `outputs/` or `figures/` directories.

---

# Citation

If referencing this work:

**Abhishek Kadam (2026)**
*Predicting and Stratifying Delays in Clinical Trial Results Reporting:
An Interpretable Machine Learning Framework for Governance Risk Stratification.*

Walsh College — QM640 Data Analytics Capstone.

---

# Status

Capstone Research Project
**QM640 Data Analytics Capstone — Walsh College**
Winter 2025 Term

The repository reflects the **final analytical pipeline used in the study**.

---


