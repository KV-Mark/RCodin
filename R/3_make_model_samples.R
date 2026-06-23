# ==============================================================================
# 3_make_model_samples.R
# Thesis project: The Effect of Monetary Policy Announcements on Cryptocurrency Returns
#
# Purpose:
#   - Load cleaned daily data from Step 2
#   - Create separate ECB/EU and Fed/US model samples
#   - Keep the relevant trading days for each market
#   - Keep only region-specific controls in each sample
#   - Preserve non-announcement days with shock = 0
#   - Save model samples for LP/IRF estimation
#
# Input:
#   data/processed/02_clean_daily_data.csv
#
# Outputs:
#   data/processed/03_model_sample_ecb.csv
#   data/processed/03_model_sample_fed.csv
#   output/tables/validation_03_model_specs.csv
#   output/tables/validation_03_model_sample_summary.csv
#   output/tables/validation_03_missing_rhs_by_sample.csv
# ==============================================================================


# ------------------------------------------------------------------------------
# 3.0 Load setup
# ------------------------------------------------------------------------------

setup_candidates <- c(
  file.path("RCodin", "R", "0_setup.R"),
  file.path(getwd(), "0_setup.R"),
  file.path(path.expand("~"), "Documents", "Timi", "RThesis", "RCodin", "R", "0_setup.R")
)

setup_file <- setup_candidates[file.exists(setup_candidates)][1]

if (is.na(setup_file)) {
  stop(
    "Could not find 0_setup.R. Check that the project is located at ~/Documents/Timi/RThesis.",
    call. = FALSE
  )
}

source(setup_file)


# ------------------------------------------------------------------------------
# 3.1 Read Step 2 output
# ------------------------------------------------------------------------------

clean_input_file <- file.path(PATHS$data_processed, "02_clean_daily_data.csv")

if (!file.exists(clean_input_file)) {
  stop(
    "Could not find Step 2 output: ",
    clean_input_file,
    "\nRun source('RCodin/R/2_clean_prepare.R') first.",
    call. = FALSE
  )
}

clean_data <- safe_read_csv(clean_input_file)

message("Step 2 cleaned dataset loaded.")
message("Rows: ", nrow(clean_data))
message("Columns: ", ncol(clean_data))


# ------------------------------------------------------------------------------
# 3.2 Validate required Step 2 variables
# ------------------------------------------------------------------------------

step3_required_vars <- unique(c(
  DATE_VAR,
  CRYPTO_RETURN_VARS,
  SHOCK_VARS,
  FED_CONTROL_VARS,
  ECB_CONTROL_VARS,
  "ecb_mp_observed",
  "fed_mp_observed",
  "ecb_meeting",
  "fed_meeting",
  "ecb_mp_nonzero",
  "fed_mp_nonzero",
  "ecb_mp_direction",
  "fed_mp_direction",
  "eu_trading_day",
  "us_trading_day"
))

check_required_columns(
  clean_data,
  step3_required_vars,
  data_name = "clean_data from Step 2"
)

message("Step 3 required-variable validation passed.")


# ------------------------------------------------------------------------------
# 3.3 Model specification
# ------------------------------------------------------------------------------

# Design decision:
#   - ECB model uses EU stock-market and German treasury variables.
#   - Fed model uses US stock-market and US treasury variables.
#   - DXY and COVID dummy are retained in both models.
#   - MiCA dummy is retained only in the ECB/EU model.

model_specs <- tibble::tibble(
  central_bank = c("ECB", "Fed"),
  sample_name = c("ecb", "fed"),
  shock_var = c("ecb_mp", "fed_mp"),
  observed_shock_var = c("ecb_mp_observed", "fed_mp_observed"),
  meeting_var = c("ecb_meeting", "fed_meeting"),
  nonzero_var = c("ecb_mp_nonzero", "fed_mp_nonzero"),
  direction_var = c("ecb_mp_direction", "fed_mp_direction"),
  trading_day_var = c("eu_trading_day", "us_trading_day"),
  stock_return_var = c("stoxx50_log_return", "sp500_log_return"),
  controls = c(
    paste(ECB_CONTROL_VARS, collapse = ", "),
    paste(FED_CONTROL_VARS, collapse = ", ")
  )
)

write_table_csv(
  model_specs,
  "validation_03_model_specs.csv",
  digits = TABLE_DIGITS
)


# ------------------------------------------------------------------------------
# 3.4 Helper functions
# ------------------------------------------------------------------------------

count_direction <- function(data, direction_var, direction_value) {
  sum(data[[direction_var]] == direction_value, na.rm = TRUE)
}

count_missing_by_variable <- function(data, variables, sample_name) {
  purrr::map_dfr(
    variables,
    function(v) {
      tibble::tibble(
        sample = sample_name,
        variable = v,
        n_rows = nrow(data),
        n_missing = sum(is.na(data[[v]])),
        n_non_missing = sum(!is.na(data[[v]])),
        pct_missing = ifelse(
          nrow(data) == 0,
          NA_real_,
          100 * sum(is.na(data[[v]])) / nrow(data)
        )
      )
    }
  )
}

make_model_sample <- function(
    data,
    sample_name,
    central_bank,
    shock_var,
    observed_shock_var,
    meeting_var,
    nonzero_var,
    direction_var,
    trading_day_var,
    stock_return_var,
    control_vars
) {
  rhs_vars <- unique(c(shock_var, control_vars))
  
  keep_vars <- unique(c(
    DATE_VAR,
    CRYPTO_RETURN_VARS,
    stock_return_var,
    control_vars,
    shock_var,
    observed_shock_var,
    meeting_var,
    nonzero_var,
    direction_var,
    trading_day_var,
    "eu_trading_day",
    "us_trading_day",
    "both_stock_markets_trading"
  ))
  
  missing_keep_vars <- setdiff(keep_vars, names(data))
  
  if (length(missing_keep_vars) > 0) {
    stop(
      "Missing variables for ",
      central_bank,
      " sample: ",
      paste(missing_keep_vars, collapse = ", "),
      call. = FALSE
    )
  }
  
  sample_before_rhs_filter <- data %>%
    filter(.data[[trading_day_var]] == 1) %>%
    select(all_of(keep_vars)) %>%
    mutate(
      central_bank = central_bank,
      sample_name = sample_name,
      rhs_complete = stats::complete.cases(across(all_of(rhs_vars)))
    )
  
  dropped_rhs_incomplete <- sample_before_rhs_filter %>%
    filter(!.data$rhs_complete)
  
  sample_final <- sample_before_rhs_filter %>%
    filter(.data$rhs_complete) %>%
    arrange(.data$date) %>%
    mutate(
      trading_day_index = row_number(),
      shock_abs = abs(.data[[shock_var]]),
      announcement_day = .data[[meeting_var]],
      nonzero_announcement_day = .data[[nonzero_var]]
    ) %>%
    select(
      central_bank,
      sample_name,
      trading_day_index,
      date,
      all_of(CRYPTO_RETURN_VARS),
      all_of(stock_return_var),
      all_of(control_vars),
      all_of(shock_var),
      all_of(observed_shock_var),
      all_of(meeting_var),
      all_of(nonzero_var),
      all_of(direction_var),
      shock_abs,
      announcement_day,
      nonzero_announcement_day,
      all_of(trading_day_var),
      eu_trading_day,
      us_trading_day,
      both_stock_markets_trading
    )
  
  list(
    sample = sample_final,
    before_rhs_filter = sample_before_rhs_filter,
    dropped_rhs_incomplete = dropped_rhs_incomplete,
    rhs_vars = rhs_vars
  )
}


# ------------------------------------------------------------------------------
# 3.5 Create ECB/EU sample
# ------------------------------------------------------------------------------

ecb_result <- make_model_sample(
  data = clean_data,
  sample_name = "ecb",
  central_bank = "ECB",
  shock_var = "ecb_mp",
  observed_shock_var = "ecb_mp_observed",
  meeting_var = "ecb_meeting",
  nonzero_var = "ecb_mp_nonzero",
  direction_var = "ecb_mp_direction",
  trading_day_var = "eu_trading_day",
  stock_return_var = "stoxx50_log_return",
  control_vars = ECB_CONTROL_VARS
)

ecb_sample <- ecb_result$sample

message("ECB sample created.")
message("ECB rows: ", nrow(ecb_sample))


# ------------------------------------------------------------------------------
# 3.6 Create Fed/US sample
# ------------------------------------------------------------------------------

fed_result <- make_model_sample(
  data = clean_data,
  sample_name = "fed",
  central_bank = "Fed",
  shock_var = "fed_mp",
  observed_shock_var = "fed_mp_observed",
  meeting_var = "fed_meeting",
  nonzero_var = "fed_mp_nonzero",
  direction_var = "fed_mp_direction",
  trading_day_var = "us_trading_day",
  stock_return_var = "sp500_log_return",
  control_vars = FED_CONTROL_VARS
)

fed_sample <- fed_result$sample

message("Fed sample created.")
message("Fed rows: ", nrow(fed_sample))


# ------------------------------------------------------------------------------
# 3.7 Validation summaries
# ------------------------------------------------------------------------------

sample_summary_one <- function(
    sample,
    sample_before_rhs_filter,
    dropped_rhs_incomplete,
    central_bank,
    meeting_var,
    nonzero_var,
    direction_var,
    trading_day_var
) {
  tibble::tibble(
    central_bank = central_bank,
    rows_after_trading_day_filter = nrow(sample_before_rhs_filter),
    rows_dropped_due_to_missing_rhs = nrow(dropped_rhs_incomplete),
    rows_final_model_sample = nrow(sample),
    first_date = as.character(min(sample$date, na.rm = TRUE)),
    last_date = as.character(max(sample$date, na.rm = TRUE)),
    no_announcement_days_retained = sum(sample[[meeting_var]] == 0, na.rm = TRUE),
    observed_meetings_retained = sum(sample[[meeting_var]] == 1, na.rm = TRUE),
    nonzero_mp_shocks_retained = sum(sample[[nonzero_var]] == 1, na.rm = TRUE),
    negative_shocks_retained = count_direction(sample, direction_var, "Negative (<0)"),
    positive_shocks_retained = count_direction(sample, direction_var, "Positive (>0)"),
    zero_observed_shocks_retained = count_direction(sample, direction_var, "Zero (=0)"),
    trading_day_indicator_used = trading_day_var
  )
}

model_sample_summary <- bind_rows(
  sample_summary_one(
    sample = ecb_sample,
    sample_before_rhs_filter = ecb_result$before_rhs_filter,
    dropped_rhs_incomplete = ecb_result$dropped_rhs_incomplete,
    central_bank = "ECB",
    meeting_var = "ecb_meeting",
    nonzero_var = "ecb_mp_nonzero",
    direction_var = "ecb_mp_direction",
    trading_day_var = "eu_trading_day"
  ),
  sample_summary_one(
    sample = fed_sample,
    sample_before_rhs_filter = fed_result$before_rhs_filter,
    dropped_rhs_incomplete = fed_result$dropped_rhs_incomplete,
    central_bank = "Fed",
    meeting_var = "fed_meeting",
    nonzero_var = "fed_mp_nonzero",
    direction_var = "fed_mp_direction",
    trading_day_var = "us_trading_day"
  )
)

missing_rhs_by_sample <- bind_rows(
  count_missing_by_variable(
    data = ecb_result$before_rhs_filter,
    variables = ecb_result$rhs_vars,
    sample_name = "ECB before RHS complete-case filter"
  ),
  count_missing_by_variable(
    data = fed_result$before_rhs_filter,
    variables = fed_result$rhs_vars,
    sample_name = "Fed before RHS complete-case filter"
  )
)

write_table_csv(
  model_sample_summary,
  "validation_03_model_sample_summary.csv",
  digits = TABLE_DIGITS
)

write_table_csv(
  missing_rhs_by_sample,
  "validation_03_missing_rhs_by_sample.csv",
  digits = TABLE_DIGITS
)


# ------------------------------------------------------------------------------
# 3.8 Save model samples
# ------------------------------------------------------------------------------

ecb_output_file <- file.path(PATHS$data_processed, "03_model_sample_ecb.csv")
fed_output_file <- file.path(PATHS$data_processed, "03_model_sample_fed.csv")

readr::write_csv(ecb_sample, ecb_output_file, na = "")
readr::write_csv(fed_sample, fed_output_file, na = "")

message("Saved ECB model sample: ", ecb_output_file)
message("Saved Fed model sample: ", fed_output_file)


# ------------------------------------------------------------------------------
# 3.9 Final checks
# ------------------------------------------------------------------------------

if (any(is.na(ecb_sample$ecb_mp))) {
  stop("ECB model sample still contains NA in ecb_mp.", call. = FALSE)
}

if (any(is.na(fed_sample$fed_mp))) {
  stop("Fed model sample still contains NA in fed_mp.", call. = FALSE)
}

if (sum(ecb_sample$ecb_meeting == 0, na.rm = TRUE) == 0) {
  warning(
    "ECB sample contains no non-announcement days. This is unexpected.",
    call. = FALSE
  )
}

if (sum(fed_sample$fed_meeting == 0, na.rm = TRUE) == 0) {
  warning(
    "Fed sample contains no non-announcement days. This is unexpected.",
    call. = FALSE
  )
}

message("Step 3 complete: ECB and Fed model samples prepared.")

