connect_aact <- function() {
  
  requireNamespace("DBI")
  requireNamespace("RPostgres")
  
  user <- Sys.getenv("AACT_USER")
  pwd  <- Sys.getenv("AACT_PWD")
  
  if (user == "" || pwd == "") {
    stop("AACT credentials not found. Check .Renviron.", call. = FALSE)
  }
  
  message("Connecting to AACT database...")
  
  DBI::dbConnect(
    RPostgres::Postgres(),
    dbname   = "aact",
    host     = "aact-db.ctti-clinicaltrials.org",
    port     = 5432,
    user     = user,
    password = pwd,
    sslmode  = "require"
  )
}