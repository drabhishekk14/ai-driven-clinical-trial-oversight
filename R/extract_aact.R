# ============================================================
# AACT extraction
# ============================================================

source("R/connect_aact.R")

con <- connect_aact()

sql <- readLines("sql/aact_extract.sql")
sql <- paste(sql, collapse = "\n")

aact_raw <- DBI::dbGetQuery(con, sql)

DBI::dbDisconnect(con)

if (!dir.exists("data/raw")) {
  dir.create("data/raw", recursive = TRUE)
}

write.csv(
  aact_raw,
  "data/raw/aact_capstone_raw.csv",
  row.names = FALSE
)

cat("AACT extract complete:", nrow(aact_raw), "rows\n")
