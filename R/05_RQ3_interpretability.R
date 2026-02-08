library(ranger)
library(dplyr)
library(readr)

final <- read_csv("data/processed/final_dataset.csv")

set.seed(123)

rf_class <- ranger(
  as.factor(delayed_reporting) ~ TOCI + SGMP,
  data = final,
  num.trees = 500,
  importance = "impurity",
  probability = TRUE
)

importance_df <- data.frame(
  Feature = names(rf_class$variable.importance),
  Importance = rf_class$variable.importance
) %>%
  arrange(desc(Importance))

write_csv(
  importance_df,
  "outputs/tables/RQ3_Table_R6_Feature_Importance.csv"
)
