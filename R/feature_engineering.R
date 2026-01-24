raw <- read.csv("data/raw/aact_sample.csv")

# Example derived variable
raw$study_type_flag <- ifelse(raw$study_type == "Interventional", 1, 0)

dir.create("data/interim", showWarnings = FALSE)
write.csv(raw, "data/interim/aact_cleaned.csv", row.names = FALSE)
