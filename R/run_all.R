# ==============================================================================
# run_all.R
# Thesis project: The Effect of Monetary Policy Announcements on Cryptocurrency Returns
#
# Purpose:
#   - Run the complete thesis-code workflow from Step 0 to Step 9
#   - Stop immediately if any step fails
#   - Save a workflow execution log
#
# Run with:
#   source("RCodin/R/run_all.R")
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. Settings
# ------------------------------------------------------------------------------

STOP_ON_ERROR <- TRUE

PROJECT_ROOT <- file.path(path.expand("~"), "Documents", "Timi", "RThesis")
CODE_DIR <- file.path(PROJECT_ROOT, "RCodin", "R")
TABLE_DIR <- file.path(PROJECT_ROOT, "output", "tables")

if (!dir.exists(CODE_DIR)) {
  stop("Code directory not found: ", CODE_DIR, call. = FALSE)
}

if (!dir.exists(TABLE_DIR)) {
  dir.create(TABLE_DIR, recursive = TRUE, showWarnings = FALSE)
}


# ------------------------------------------------------------------------------
# 1. Define workflow scripts
# ------------------------------------------------------------------------------

workflow_scripts <- tibble::tibble(
  step = c(
    "Step 0",
    "Step 1",
    "Step 2",
    "Step 3",
    "Step 4",
    "Step 5",
    "Step 6",
    "Step 7",
    "Step 8",
    "Step 9"
  ),
  script = c(
    "0_setup.R",
    "1_import_validate.R",
    "2_clean_prepare.R",
    "3_make_model_samples.R",
    "4_lpirf_wrapper.R",
    "5_estimate_irfs.R",
    "6_descriptive_tables.R",
    "7_irf_tables.R",
    "8_figures.R",
    "9_validation_checks.R"
  ),
  purpose = c(
    "Setup paths, packages, global settings, labels, and helpers",
    "Import and validate raw merged dataset",
    "Clean data, standardize variables, create shock and trading-day indicators",
    "Create ECB and Fed model samples",
    "Load LPIRF wrapper functions",
    "Estimate six baseline IRFs",
    "Create descriptive and event-summary tables",
    "Create detailed IRF regression table",
    "Create thesis figures",
    "Run final validation and audit checks"
  )
) %>%
  mutate(
    path = file.path(CODE_DIR, .data$script),
    file_exists = file.exists(.data$path)
  )

missing_scripts <- workflow_scripts %>%
  filter(!.data$file_exists)

if (nrow(missing_scripts) > 0) {
  stop(
    "Missing workflow scripts: ",
    paste(missing_scripts$script, collapse = ", "),
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 2. Helper function to run one script
# ------------------------------------------------------------------------------

run_one_script <- function(step_name, script_name, script_path) {
  message("")
  message("================================================================")
  message("Running ", step_name, ": ", script_name)
  message("================================================================")
  
  start_time <- Sys.time()
  
  result <- tryCatch(
    {
      source(script_path, local = FALSE)
      
      tibble::tibble(
        step = step_name,
        script = script_name,
        path = script_path,
        status = "OK",
        error_message = NA_character_,
        start_time = as.character(start_time),
        end_time = as.character(Sys.time()),
        runtime_seconds = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      )
    },
    error = function(e) {
      tibble::tibble(
        step = step_name,
        script = script_name,
        path = script_path,
        status = "ERROR",
        error_message = e$message,
        start_time = as.character(start_time),
        end_time = as.character(Sys.time()),
        runtime_seconds = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      )
    },
    warning = function(w) {
      message("Warning in ", script_name, ": ", w$message)
      
      invokeRestart("muffleWarning")
    }
  )
  
  result
}


# ------------------------------------------------------------------------------
# 3. Run full workflow
# ------------------------------------------------------------------------------

workflow_log <- list()

for (i in seq_len(nrow(workflow_scripts))) {
  current_result <- run_one_script(
    step_name = workflow_scripts$step[i],
    script_name = workflow_scripts$script[i],
    script_path = workflow_scripts$path[i]
  )
  
  workflow_log[[i]] <- current_result
  
  if (current_result$status == "ERROR" && STOP_ON_ERROR) {
    workflow_log_table <- bind_rows(workflow_log)
    
    readr::write_csv(
      workflow_log_table,
      file.path(TABLE_DIR, "validation_10_run_all_log.csv"),
      na = ""
    )
    
    stop(
      "Workflow stopped at ",
      current_result$step,
      " / ",
      current_result$script,
      ". Error: ",
      current_result$error_message,
      call. = FALSE
    )
  }
}

workflow_log_table <- bind_rows(workflow_log)


# ------------------------------------------------------------------------------
# 4. Save workflow log
# ------------------------------------------------------------------------------

workflow_log_file <- file.path(TABLE_DIR, "validation_10_run_all_log.csv")

readr::write_csv(
  workflow_log_table,
  workflow_log_file,
  na = ""
)

message("")
message("Saved workflow log: ", workflow_log_file)


# ------------------------------------------------------------------------------
# 5. Final workflow summary
# ------------------------------------------------------------------------------

workflow_summary <- tibble::tibble(
  metric = c(
    "workflow_run_time",
    "project_root",
    "scripts_expected",
    "scripts_run",
    "scripts_ok",
    "scripts_error",
    "overall_status"
  ),
  value = c(
    as.character(Sys.time()),
    PROJECT_ROOT,
    as.character(nrow(workflow_scripts)),
    as.character(nrow(workflow_log_table)),
    as.character(sum(workflow_log_table$status == "OK")),
    as.character(sum(workflow_log_table$status == "ERROR")),
    ifelse(all(workflow_log_table$status == "OK"), "PASS", "FAIL")
  )
)

workflow_summary_file <- file.path(TABLE_DIR, "validation_10_run_all_summary.csv")

readr::write_csv(
  workflow_summary,
  workflow_summary_file,
  na = ""
)

message("Saved workflow summary: ", workflow_summary_file)

print(workflow_summary)

if (all(workflow_log_table$status == "OK")) {
  message("Full workflow completed successfully.")
} else {
  warning(
    "Full workflow completed with errors. Check validation_10_run_all_log.csv.",
    call. = FALSE
  )
}


