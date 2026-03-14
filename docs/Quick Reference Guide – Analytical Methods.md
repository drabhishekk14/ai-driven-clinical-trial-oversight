
Your **Quick Reference Guide is good**, but it no longer perfectly matches the **final report structure** in three places:

1. You **do not predict delay days anymore** → remove “Regression (Expected Delay)”.
2. You **did not use Tree Surrogate Models or PDPs in the final report** → remove them.
3. You **added statistical testing (DeLong, Chi-square)** → include them.
4. RQ structure in the final report is **RQ1–RQ4 only** (no RQ5). 

So the guide should be **simplified and aligned with the actual analysis used**.

Below is a **clean revised version** you can paste into the report or appendix.

---

# Quick Reference Guide

**Analytical Methods Used in the Capstone**

---

# Descriptive Statistics

*(Used in RQ1 – Diagnostic Exploration)*

### What it is

Summarizes data to understand distribution, central tendency, and spread.

### Typical outputs

• Mean, median, standard deviation
• Frequency tables
• Proportions (e.g., % delayed trials)

### Why you use it

• First sanity check of the data
• Understand baseline delay patterns
• Provide context for regression results

### What it does NOT do

• Does not test causality
• Does not control for confounders

---

# Logistic Regression

*(Used in RQ1 – Association Testing)*

### What it is

A probabilistic classification model for binary outcomes.

[
\log\left(\frac{p}{1-p}\right) = \beta_0 + \beta_1 X_1 + ... + \beta_k X_k
]

### Outcome in this study

• Delayed vs Not Delayed (>365 days)

### Key outputs

• Odds Ratios (OR)
• p-values
• Confidence intervals

### Why it is used

• Tests association between TOCI / SGMP and reporting delay
• Controls for multiple predictors simultaneously

### Interpretation

• OR > 1 → higher odds of delay
• OR < 1 → lower odds of delay

### Limitation

• Association only — does not imply causality

---

# Cox Proportional Hazards Model

*(Used in RQ1 – Time-to-Reporting Analysis)*

### What it is

A survival analysis model estimating the rate of event occurrence over time.

[
h(t|X) = h_0(t)\exp(\beta X)
]

### Outcome in this study

• Time to results reporting

### Key outputs

• Hazard Ratios (HR)

### Why it is used

• Accounts for timing of reporting
• Provides complementary insight to logistic regression

### Interpretation

• HR > 1 → faster reporting
• HR < 1 → slower reporting

### Key assumption

• Proportional hazards

---

# Random Forest (Classification)

*(Used in RQ2 – Prediction)*

### What it is

An ensemble learning method combining many decision trees.

### Key idea

Many weak trees → one strong predictive model.

### Outcome in this study

• Probability of reporting delay

### Why it is used

• Captures nonlinear relationships
• Automatically models interactions
• Robust predictive performance

### Key outputs

• Predicted probabilities
• Feature importance

### Evaluation metrics

• AUC (Area Under ROC Curve)

---

# Gradient Boosting (XGBoost / LightGBM)

*(Used in RQ2 – Model Benchmarking)*

### What it is

An ensemble method that sequentially improves prediction errors.

### Why it is included

• Strong benchmark for Random Forest
• Tests robustness of predictive performance

### Key concept

| Random Forest  | Gradient Boosting |
| -------------- | ----------------- |
| Parallel trees | Sequential trees  |
| Lower variance | Lower bias        |

### Evaluation metric

• AUC

---

# DeLong Test

*(Used in RQ2 – Model Comparison)*

### What it is

A statistical test used to compare ROC AUC values between models.

### Why it is used

• Determines whether ML models significantly outperform regression baselines.

### Interpretation

• p < 0.05 → statistically significant improvement in discrimination.

---

# Permutation Feature Importance

*(Used in RQ3 – Interpretability)*

### What it is

A model-agnostic technique that measures the impact of each feature on prediction accuracy.

### Why it is used

• Identifies dominant drivers of delay risk
• Ensures predictors remain interpretable

### Key insight in this study

Sponsor historical compliance emerged as the dominant predictor.

### Limitation

• Shows importance but not causal direction.

---

# Risk Stratification

*(Used in RQ4 – Governance Translation)*

### What it is

Mapping predicted probabilities into operational risk tiers.

### Inputs

• Predicted probability of reporting delay

### Output

• Low risk
• Moderate risk
• High risk

### Why this matters

• Enables targeted oversight
• Supports Risk-Based Quality Management (RBQM)

---

# Chi-Square Test

*(Used in RQ4 – Risk Tier Validation)*

### What it is

A statistical test for association between categorical variables.

### Application in this study

Tests whether **risk tier classification is associated with actual reporting delay**.

### Interpretation

• p < 0.05 → risk tiers meaningfully differentiate delay risk.

---

# Cross-Validation

*(Used in RQ2 – Model Validation)*

### What it is

Repeatedly splitting data into training and validation sets.

### Why it is used

• Prevents overfitting
• Ensures model stability

### Typical approach

• k-fold cross-validation (k = 5 or 10)

---

# Mental Map (One-Line Summary)

• **RQ1** → Why delays happen → Logistic & Cox models
• **RQ2** → Can we predict delays → Machine Learning
• **RQ3** → Are predictions interpretable → Feature importance
• **RQ4** → How predictions guide action → Risk stratification

---

# Key Takeaway

**Regression explains, machine learning predicts, interpretability validates, and risk stratification translates analytics into governance decisions.**
