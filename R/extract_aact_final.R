# ============================================================
# AACT extraction
# ============================================================

# Load connection function
source("R/connect_aact.R")

cat("Starting AACT data extraction...\n")

# ------------------------------------------------------------
# Connect to database
# ------------------------------------------------------------

con <- connect_aact()

# ------------------------------------------------------------
# Load SQL query
# ------------------------------------------------------------

sql_file <- "sql/aact_extract_final.sql"

if (!file.exists(sql_file)) {
  stop("SQL file not found: ", sql_file)
}

sql <- readLines(sql_file, warn = FALSE)
sql <- paste(sql, collapse = "\n")

cat("Running AACT extraction query...\n")

# ------------------------------------------------------------
# Run query
# ------------------------------------------------------------

aact_raw <- DBI::dbGetQuery(con, sql)

# ------------------------------------------------------------
# Close connection safely
# ------------------------------------------------------------

if (DBI::dbIsValid(con)) {
  DBI::dbDisconnect(con)
  cat("Database connection closed.\n")
}

# ------------------------------------------------------------
# Create output directory if needed
# ------------------------------------------------------------

output_dir <- "data/raw"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ------------------------------------------------------------
# Save dataset
# ------------------------------------------------------------

output_file <- file.path(output_dir, "aact_capstone_raw_final.csv")

readr::write_csv(aact_raw, output_file)

# ------------------------------------------------------------
# Validation checks
# ------------------------------------------------------------

cat("\nExtraction completed successfully.\n")

cat("Rows extracted:", nrow(aact_raw), "\n")
cat("Columns extracted:", ncol(aact_raw), "\n")

if ("nct_id" %in% names(aact_raw)) {
  cat("Unique trials:", length(unique(aact_raw$nct_id)), "\n")
}

cat("Output saved to:", output_file, "\n")