# ------------------------------------------
# Capstone Pipeline Runner
# ------------------------------------------

# Ensure working directory is project root
if (!file.exists("data") || !file.exists("R")) {
  stop("Run this script from the project root directory.")
}

# ------------------------------------------
# Logging setup
# ------------------------------------------

log_file <- "pipeline_log.txt"

log_message <- function(msg) {
  line <- paste0("[", Sys.time(), "] ", msg)
  cat(line, "\n")
  write(line, log_file, append = TRUE)
}

# ------------------------------------------
# Script execution wrapper
# ------------------------------------------

run_step <- function(script_path) {
  
  log_message(paste("Running:", script_path))
  
  if (!file.exists(script_path)) {
    stop(paste("Script not found:", script_path))
  }
  
  tryCatch(
    {
      source(script_path, echo = TRUE)
      log_message(paste("Completed:", script_path))
    },
    
    error = function(e) {
      
      log_message(paste("ERROR in:", script_path))
      log_message(paste("Message:", e$message))
      
      stop(paste("Pipeline stopped due to error in", script_path))
    }
  )
}

# ------------------------------------------
# Pipeline Steps
# ------------------------------------------

pipeline <- c(
  
  # 01_Data acquisition
  "R/connect_aact.R",
  "R/extract_aact_final.R",
  
  # 02_Feature engineering
  "final_analysis/scripts/feature_engineering_sponsor_history_final.R",
  
  # 03_EDA
  "final_analysis/scripts/EDA_final_report.R",
  "final_analysis/scripts/sample_size_calculations.R",
  
  # 04_Modeling (includes RQ1–RQ2)
  "final_analysis/scripts/analysis_modeling_sponsor_history_final.R",
  
  # 05_Interpretability
  "final_analysis/scripts/RQ3_interpretability_final.R",
  "final_analysis/scripts/RQ4_risk_tier_dashboard_final.R"
)

# ------------------------------------------
# Run pipeline
# ------------------------------------------

log_message("Starting Capstone Pipeline")

start_time <- Sys.time()

for (step in pipeline) {
  run_step(step)
}

end_time <- Sys.time()

log_message("Pipeline completed successfully")
log_message(paste("Total runtime:", round(end_time - start_time, 2)))