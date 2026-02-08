Quick Reference Guide
Analytical Methods Used in the Capstone

Descriptive Statistics
(Used in RQ1 – Diagnostic)
What it is
Summarizes data to understand distribution, central tendency, and spread.
Typical outputs
•	Mean, median, standard deviation
•	Frequency tables
•	Proportions (e.g., % delayed trials)
Why you use it
•	First sanity check of the data
•	Understand baseline delay patterns
•	Context for regression results
What it does NOT do
Does not test causality
Does not control for confounders

Logistic Regression
(Used in RQ1 – Association)
What it is
A probabilistic classification model for binary outcomes.
[
\log\left(\frac{p}{1-p}\right) = \beta_0 + \beta_1 X_1 + \dots + \beta_k X_k
]
Outcome in your study
•	Delayed vs Not Delayed
Key outputs
•	Odds Ratios (OR)
•	p-values
•	Confidence intervals
Why you use it
•	Tests association between TOCI / SGMP and delay
•	Controls for multiple variables simultaneously
Interpretation (important)
•	OR > 1 → higher odds of delay
•	OR < 1 → lower odds of delay
What it does NOT do
Does not predict exact delay days
Does not imply causality

Cox Proportional Hazards Model
(Used in RQ1 – Time-to-event)
What it is
A survival analysis model that estimates the rate of event occurrence over time.
[
h(t|X) = h_0(t)\exp(\beta X)
]
Outcome in your study
•	Time to results reporting
Key outputs
•	Hazard Ratios (HR)
Why you use it
•	Accounts for timing, not just occurrence
•	Handles censored data (not yet reported trials)
Interpretation
•	HR < 1 → slower reporting
•	HR > 1 → faster reporting
Assumption
•	Proportional hazards (effect constant over time)

Random Forest (Classification)
(Used in RQ2 – Prediction)
What it is
An ensemble learning method using many decision trees.
Key idea
Many weak trees → one strong model
Outcome in your study
•	Probability of reporting delay
Why you use it
•	Captures non-linear relationships
•	Handles interactions automatically
•	Strong predictive performance
Key outputs
•	Predicted probabilities
•	Feature importance
Evaluation metrics
•	AUC
•	Accuracy
•	Precision / Recall

Gradient Boosting
(Used in RQ2 – Benchmarking)
What it is
An ensemble method that sequentially improves errors.
Why include it
•	Strong benchmark against Random Forest
•	Tests robustness of prediction
Difference from Random Forest
Random Forest	Gradient Boosting
Parallel trees	Sequential trees
Lower variance	Lower bias

Regression (Expected Delay)
(Used in RQ2 – Magnitude prediction)
What it is
Predicts a continuous outcome.
Outcome in your study
•	Delay duration (days)
Key metrics
•	MAE → average error in days
•	RMSE → penalizes large errors
Why MAE matters more here
•	Managers understand “± days” easily

Feature Importance
(Used in RQ3 – Interpretability)
What it is
Ranks variables by contribution to model performance.
Why you use it
•	Identifies dominant drivers of delay risk
•	Supports hypothesis interpretation
Limitation
Does not show direction or interaction alone

Tree Surrogate Models
(Used in RQ3 – Translation layer)
What it is
A simple decision tree trained to mimic a complex model.
Why it’s important
•	Converts black-box models into rules
•	Enables decision-ready explanations
Example rule
If TOCI > threshold AND SGMP < threshold → High risk
Key point
You do not replace the model — you explain it.

Partial Dependence Plots (PDPs)
(Used in RQ3 – Interpretability)
What it is
Shows how predictions change as one variable changes.
Why you use it
•	Visualize direction of influence
•	Check monotonicity (risk ↑ as complexity ↑)
Limitation
•	Averages effects; hides individual heterogeneity

Risk Stratification
(Used in RQ4 – Governance)
What it is
Mapping predictions into risk tiers.
Inputs
•	Probability of delay
•	Expected delay duration
Output
•	Low / Medium / High risk categories
Why this matters
•	Enables prioritization
•	Supports RBQM-style governance (without measuring RBQM)

Scenario-Based Analysis
(Used in RQ4 – Application)
What it is
Evaluates what-if operational scenarios.
Example
High probability + long delay → Immediate escalation
Why you use it
•	Bridges analytics to decisions
•	Shows business value

Cross-Validation
(Used across RQ2 & RQ5)
What it is
Repeatedly splits data into train/test sets.
Why you use it
•	Prevents overfitting
•	Ensures robustness
Typical choice
•	k-fold cross-validation (k = 5 or 10)

Mental Map (One-Liner)
•	RQ1 → Why delays happen → Regression & Survival
•	RQ2 → Who will be late & by how much → ML
•	RQ3 → Can leaders understand it → Interpretability
•	RQ4 → What should be done → Governance logic
•	RQ5 → Does it hold across contexts → Robustness checks

Final Advice (Important)
If you remember only one thing, remember this:
Regression explains, ML predicts, interpretability translates, governance applies.
