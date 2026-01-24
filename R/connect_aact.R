connect_aact <- function() {
  
  if (Sys.getenv("AACT_USER") == "" || Sys.getenv("AACT_PWD") == "") {
    stop("AACT credentials not found. Check .Renviron.", call. = FALSE)
  }
  
  DBI::dbConnect(
    RPostgres::Postgres(),
    dbname   = "aact",
    host     = "aact-db.ctti-clinicaltrials.org",
    port     = 5432,
    user     = Sys.getenv("AACT_USER"),
    password = Sys.getenv("AACT_PWD"),
    sslmode  = "require"
  )
}
