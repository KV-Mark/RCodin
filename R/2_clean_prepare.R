# ==============================================================================
# 2_clean_prepare.R
# Thesis project: The Effect of Monetary Policy Announcements on Cryptocurrency Returns
#
# Purpose:
#   - Load the validated raw dataset from Step 1
#   - Standardize column names
#   - Parse and sort dates
#   - Rename variables to canonical names used in the thesis code
#   - Convert required variables to numeric
#   - Preserve original monetary policy shock observations
#   - Create meeting indicators and shock-direction variables
#   - Replace non-announcement shock NA values with 0 for estimation
#   - Save the cleaned daily dataset for model-sample creation
#
# Input:
#   data/processed/01_raw_validated.csv
#
# Outputs:
#   data/processed/02_clean_daily_data.csv
#   output/tables/validation_02_cleaning_summary.csv
#   output/tables/validation_02_canonical_columns.csv
#   output/tables/validation_02_shock_transformation.csv
# ==============================================================================


# ------------------------------------------------------------------------------
# 2.0 Load setup
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
# 2.1 Read Step 1 output
# ------------------------------------------------------------------------------

validated_input_file <- file.path(PATHS$data_processed, "01_raw_validated.csv")

if (!file.exists(validated_input_file)) {
  stop(
    "Could not find Step 1 output: ",
    validated_input_file,
    "\nRun source('RCodin/R/1_import_validate.R') first.",
    call. = FALSE
  )
}

raw_validated <- safe_read_csv(validated_input_file)

message("Step 1 validated dataset loaded.")
message("Rows: ", nrow(raw_validated))
message("Columns: ", ncol(raw_validated))


# ------------------------------------------------------------------------------
# 2.2 Helper functions
# ------------------------------------------------------------------------------

parse_project_date <- function(x) {
  parsed <- suppressWarnings(lubridate::ymd_hms(x, tz = "UTC", quiet = TRUE))
  
  if (all(is.na(parsed))) {
    parsed <- suppressWarnings(lubridate::ymd(x, tz = "UTC", quiet = TRUE))
  }
  
  as.Date(parsed)
}

as_numeric_safely <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }
  
  suppressWarnings(readr::parse_number(as.character(x)))
}

rename_if_present <- function(data, from, to) {
  if (from %in% names(data) && !(to %in% names(data))) {
    data <- data %>%
      dplyr::rename(!!to := all_of(from))
  }
  
  data
}

shock_direction <- function(meeting_indicator, shock_value) {
  dplyr::case_when(
    meeting_indicator == 0 ~ "No announcement",
    meeting_indicator == 1 & is.na(shock_value) ~ "Observed but missing",
    meeting_indicator == 1 & shock_value < 0 ~ "Negative (<0)",
    meeting_indicator == 1 & shock_value > 0 ~ "Positive (>0)",
    meeting_indicator == 1 & shock_value == 0 ~ "Zero (=0)",
    TRUE ~ "Unclassified"
  )
}


# ------------------------------------------------------------------------------
# 2.3 Clean column names and apply canonical naming
# ------------------------------------------------------------------------------

clean_data <- raw_validated %>%
  janitor::clean_names()

# Canonical 3-month yield names used by the setup file
clean_data <- clean_data %>%
  rename_if_present("us_3m_value", "us_3m") %>%
  rename_if_present("de_3m_value", "de_3m")

# Sanity check: no duplicated column names after cleaning/renaming
duplicated_names <- names(clean_data)[duplicated(names(clean_data))]

if (length(duplicated_names) > 0) {
  stop(
    "Duplicated column names after cleaning: ",
    paste(unique(duplicated_names), collapse = ", "),
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 2.4 Parse dates and sort chronologically
# ------------------------------------------------------------------------------

if (!DATE_VAR %in% names(clean_data)) {
  stop("The cleaned dataset does not contain a date column.", call. = FALSE)
}

clean_data <- clean_data %>%
  mutate(
    date = parse_project_date(.data$date)
  )

if (any(is.na(clean_data$date))) {
  failed_dates <- sum(is.na(clean_data$date))
  
  stop(
    "Date parsing failed for ",
    failed_dates,
    " rows. Check the raw date column before continuing.",
    call. = FALSE
  )
}

duplicate_date_count <- sum(duplicated(clean_data$date))

if (duplicate_date_count > 0) {
  warning(
    duplicate_date_count,
    " duplicate dates found. The script will keep them, but this should be checked.",
    call. = FALSE
  )
}

clean_data <- clean_data %>%
  arrange(.data$date)


# ------------------------------------------------------------------------------
# 2.5 Convert variables to numeric
# ------------------------------------------------------------------------------

# Convert all non-date columns to numeric where possible. The raw dataset is numeric
# apart from the date column, so this is intentional.
non_date_cols <- setdiff(names(clean_data), DATE_VAR)

clean_data <- clean_data %>%
  mutate(
    across(
      all_of(non_date_cols),
      as_numeric_safely
    )
  )

# Dummies should be 0/1. Do not silently create values, only convert existing values.
dummy_vars <- intersect(c("covid_dummy", "mica_dummy"), names(clean_data))

clean_data <- clean_data %>%
  mutate(
    across(
      all_of(dummy_vars),
      ~ as.integer(.x)
    )
  )

dummy_validation <- purrr::map_dfr(
  dummy_vars,
  function(v) {
    observed_values_num <- sort(unique(clean_data[[v]][!is.na(clean_data[[v]])]))
    valid_binary <- all(observed_values_num %in% c(0L, 1L))
    
    tibble::tibble(
      variable = v,
      observed_values = paste(observed_values_num, collapse = ", "),
      valid_binary_dummy = valid_binary,
      n_missing = sum(is.na(clean_data[[v]]))
    )
  }
)

if (nrow(dummy_validation) > 0 && any(!dummy_validation$valid_binary_dummy)) {
  warning(
    "At least one dummy variable has values outside 0/1. Check validation_02_canonical_columns.csv.",
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 2.6 Validate canonical model variables after renaming
# ------------------------------------------------------------------------------

check_required_columns(
  clean_data,
  ALL_MODEL_VARS,
  data_name = "clean_data after Step 2 cleaning"
)

message("Canonical model-variable validation passed.")


# ------------------------------------------------------------------------------
# 2.7 Preserve original shocks and create meeting/direction variables
# ------------------------------------------------------------------------------

# Important:
#   - ecb_mp_observed / fed_mp_observed preserve the raw announcement-day values.
#   - ecb_mp / fed_mp become estimation-ready shock series:
#       observed announcement shock on meeting days,
#       0 on non-announcement days.
#   - This avoids dropping all non-announcement trading days during LP estimation.

clean_data <- clean_data %>%
  mutate(
    ecb_mp_observed = .data$ecb_mp,
    fed_mp_observed = .data$fed_mp,
    
    ecb_meeting = as.integer(!is.na(.data$ecb_mp_observed)),
    fed_meeting = as.integer(!is.na(.data$fed_mp_observed)),
    
    ecb_mp_nonzero = as.integer(.data$ecb_meeting == 1 & .data$ecb_mp_observed != 0),
    fed_mp_nonzero = as.integer(.data$fed_meeting == 1 & .data$fed_mp_observed != 0),
    
    ecb_mp_direction = shock_direction(.data$ecb_meeting, .data$ecb_mp_observed),
    fed_mp_direction = shock_direction(.data$fed_meeting, .data$fed_mp_observed),
    
    ecb_mp = standardize_na_shock_to_zero(.data$ecb_mp_observed),
    fed_mp = standardize_na_shock_to_zero(.data$fed_mp_observed)
  )

if (any(is.na(clean_data$ecb_mp))) {
  stop("ecb_mp still contains NA after standardization.", call. = FALSE)
}

if (any(is.na(clean_data$fed_mp))) {
  stop("fed_mp still contains NA after standardization.", call. = FALSE)
}


# ------------------------------------------------------------------------------
# 2.8 Trading-day availability indicators
# ------------------------------------------------------------------------------

# These are only indicators. Actual ECB/Fed trading-day samples are created in Step 3.
clean_data <- clean_data %>%
  mutate(
    eu_trading_day = as.integer(!is.na(.data$stoxx50_log_return)),
    us_trading_day = as.integer(!is.na(.data$sp500_log_return)),
    both_stock_markets_trading = as.integer(
      .data$eu_trading_day == 1 & .data$us_trading_day == 1
    )
  )


# ------------------------------------------------------------------------------
# 2.9 Validation tables
# ------------------------------------------------------------------------------

cleaning_summary <- tibble::tibble(
  metric = c(
    "rows_input_from_step_1",
    "rows_output_step_2",
    "rows_dropped_in_step_2",
    "columns_input_from_step_1",
    "columns_output_step_2",
    "first_date",
    "last_date",
    "duplicate_dates",
    "ecb_meetings_observed",
    "fed_meetings_observed",
    "ecb_nonzero_mp_shocks",
    "fed_nonzero_mp_shocks",
    "eu_trading_days_stoxx50_available",
    "us_trading_days_sp500_available",
    "both_stock_markets_trading_days",
    "ecb_mp_missing_after_standardization",
    "fed_mp_missing_after_standardization"
  ),
  value = c(
    as.character(nrow(raw_validated)),
    as.character(nrow(clean_data)),
    as.character(nrow(raw_validated) - nrow(clean_data)),
    as.character(ncol(raw_validated)),
    as.character(ncol(clean_data)),
    as.character(min(clean_data$date, na.rm = TRUE)),
    as.character(max(clean_data$date, na.rm = TRUE)),
    as.character(duplicate_date_count),
    as.character(sum(clean_data$ecb_meeting == 1, na.rm = TRUE)),
    as.character(sum(clean_data$fed_meeting == 1, na.rm = TRUE)),
    as.character(sum(clean_data$ecb_mp_nonzero == 1, na.rm = TRUE)),
    as.character(sum(clean_data$fed_mp_nonzero == 1, na.rm = TRUE)),
    as.character(sum(clean_data$eu_trading_day == 1, na.rm = TRUE)),
    as.character(sum(clean_data$us_trading_day == 1, na.rm = TRUE)),
    as.character(sum(clean_data$both_stock_markets_trading == 1, na.rm = TRUE)),
    as.character(sum(is.na(clean_data$ecb_mp))),
    as.character(sum(is.na(clean_data$fed_mp)))
  )
)

canonical_columns <- tibble::tibble(
  variable = ALL_MODEL_VARS,
  present = ALL_MODEL_VARS %in% names(clean_data),
  class = purrr::map_chr(
    ALL_MODEL_VARS,
    function(v) {
      if (v %in% names(clean_data)) {
        paste(class(clean_data[[v]]), collapse = " / ")
      } else {
        NA_character_
      }
    }
  ),
  n_missing = purrr::map_int(
    ALL_MODEL_VARS,
    function(v) {
      if (v %in% names(clean_data)) {
        sum(is.na(clean_data[[v]]))
      } else {
        NA_integer_
      }
    }
  ),
  n_non_missing = purrr::map_int(
    ALL_MODEL_VARS,
    function(v) {
      if (v %in% names(clean_data)) {
        sum(!is.na(clean_data[[v]]))
      } else {
        NA_integer_
      }
    }
  )
)

shock_transformation <- tibble::tibble(
  central_bank = c("ECB", "Fed"),
  observed_shock_variable = c("ecb_mp_observed", "fed_mp_observed"),
  estimation_shock_variable = c("ecb_mp", "fed_mp"),
  observed_meetings = c(
    sum(clean_data$ecb_meeting == 1, na.rm = TRUE),
    sum(clean_data$fed_meeting == 1, na.rm = TRUE)
  ),
  non_announcement_days_set_to_zero = c(
    sum(clean_data$ecb_meeting == 0 & clean_data$ecb_mp == 0, na.rm = TRUE),
    sum(clean_data$fed_meeting == 0 & clean_data$fed_mp == 0, na.rm = TRUE)
  ),
  negative_shocks = c(
    sum(clean_data$ecb_mp_direction == "Negative (<0)", na.rm = TRUE),
    sum(clean_data$fed_mp_direction == "Negative (<0)", na.rm = TRUE)
  ),
  positive_shocks = c(
    sum(clean_data$ecb_mp_direction == "Positive (>0)", na.rm = TRUE),
    sum(clean_data$fed_mp_direction == "Positive (>0)", na.rm = TRUE)
  ),
  zero_observed_shocks = c(
    sum(clean_data$ecb_mp_direction == "Zero (=0)", na.rm = TRUE),
    sum(clean_data$fed_mp_direction == "Zero (=0)", na.rm = TRUE)
  )
)

write_table_csv(
  cleaning_summary,
  "validation_02_cleaning_summary.csv",
  digits = TABLE_DIGITS
)

write_table_csv(
  canonical_columns,
  "validation_02_canonical_columns.csv",
  digits = TABLE_DIGITS
)

write_table_csv(
  shock_transformation,
  "validation_02_shock_transformation.csv",
  digits = TABLE_DIGITS
)

if (nrow(dummy_validation) > 0) {
  write_table_csv(
    dummy_validation,
    "validation_02_dummy_validation.csv",
    digits = TABLE_DIGITS
  )
}


# ------------------------------------------------------------------------------
# 2.10 Save cleaned data
# ------------------------------------------------------------------------------

clean_output_file <- file.path(PATHS$data_processed, "02_clean_daily_data.csv")

readr::write_csv(
  clean_data,
  clean_output_file,
  na = ""
)

message("Saved cleaned daily data: ", clean_output_file)


# ------------------------------------------------------------------------------
# 2.11 Final message
# ------------------------------------------------------------------------------

message("Step 2 complete: cleaned daily dataset prepared.")


