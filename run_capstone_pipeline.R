# -------------------------------------------------
# Capstone Pipeline Runner
# -------------------------------------------------

options(error = function() traceback(2))

if (!file.exists("data") || !file.exists("R")) {
  stop("Run this script from the project root directory.")
}

# -------------------------------------------------
# Logging
# -------------------------------------------------

log_file <- "pipeline_log.txt"
checkpoint_file <- "pipeline_checkpoint.txt"

log_message <- function(msg) {
  line <- paste0("[", Sys.time(), "] ", msg)
  cat(line, "\n")
  write(line, log_file, append = TRUE)
}

# -------------------------------------------------
# Load completed steps
# -------------------------------------------------

completed_steps <- c()

if (file.exists(checkpoint_file)) {
  completed_steps <- readLines(checkpoint_file)
}

log_message("Starting Capstone Pipeline")

# -------------------------------------------------
# Script execution wrapper
# -------------------------------------------------

run_step <- function(script_path) {
  
  # Skip completed steps
  if (script_path %in% completed_steps) {
    log_message(paste("Skipping (already completed):", script_path))
    return()
  }
  
  if (!file.exists(script_path)) {
    stop(paste("Script not found:", script_path))
  }
  
  log_message(paste("Running:", script_path))
  
  step_start <- Sys.time()
  
  tryCatch(
    {
      source(script_path, echo = TRUE, local = .GlobalEnv)
      
      step_end <- Sys.time()
      
      runtime <- round(as.numeric(difftime(step_end, step_start, units = "secs")), 2)
      
      log_message(paste(
        "Completed:", script_path,
        "| Runtime:", runtime, "seconds"
      ))
      
      # Save checkpoint
      write(script_path, checkpoint_file, append = TRUE)
      
    },
    
    error = function(e) {
      
      log_message(paste("ERROR in:", script_path))
      log_message(paste("Message:", e$message))
      
      stop(paste("Pipeline stopped due to error in", script_path))
    }
  )
}

# -------------------------------------------------
# Pipeline Steps
# -------------------------------------------------

pipeline <- c(
  
  "R/connect_aact.R",
  "R/extract_aact_final.R",
  
  "final_analysis/scripts/feature_engineering_sponsor_history_final.R",
  
  "final_analysis/scripts/EDA_final_report.R",
  "final_analysis/scripts/sample_size_calculations.R",
  
  "final_analysis/scripts/analysis_modeling_sponsor_history_final_xgb_lgbm.R",
  
  "final_analysis/scripts/RQ3_interpretability_final.R",
  
  "final_analysis/scripts/RQ4_risk_tier_dashboard_final.R"
)

# -------------------------------------------------
# Run pipeline
# -------------------------------------------------

start_time <- Sys.time()

for (step in pipeline) {
  run_step(step)
}

end_time <- Sys.time()

runtime <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 2)

log_message("Pipeline completed successfully")
log_message(paste("Total runtime:", runtime, "seconds"))