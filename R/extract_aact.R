source("R/connect_aact.R")

con <- connect_aact()

# Placeholder query (will be expanded)
query <- "SELECT nct_id, study_type FROM studies LIMIT 10;"

raw_data <- DBI::dbGetQuery(con, query)

DBI::dbDisconnect(con)

# Save raw extract
dir.create("data/raw", showWarnings = FALSE)
write.csv(raw_data, "data/raw/aact_sample.csv", row.names = FALSE)
