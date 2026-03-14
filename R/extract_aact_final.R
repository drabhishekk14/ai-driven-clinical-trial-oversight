# ============================================================
# AACT extraction
# ============================================================

source("R/connect_aact.R")

# Connect to database
con <- connect_aact()

# Ensure connection closes even if error occurs
on.exit(DBI::dbDisconnect(con), add = TRUE)

# Load SQL
sql_file <- "sql/aact_extract_final.sql"

sql <- readLines(sql_file, warn = FALSE)
sql <- paste(sql, collapse = "\n")

cat("Running AACT extraction query...\n")

# Run query
aact_raw <- DBI::dbGetQuery(con, sql)

# Create output directory
if (!dir.exists("data/raw")) {
  dir.create("data/raw", recursive = TRUE)
}

# Save dataset
readr::write_csv(
  aact_raw,
  "data/raw/aact_capstone_raw_final.csv"
)

cat("Final AACT extract complete:", nrow(aact_raw), "rows\n")