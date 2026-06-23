# ==============================================================================
# 1_import_validate.R
# Thesis project: The Effect of Monetary Policy Announcements on Cryptocurrency Returns
#
# Purpose:
#   - Load the raw merged dataset
#   - Validate required columns
#   - Validate date parsing
#   - Validate key numeric variables
#   - Produce initial dataset diagnostics
#   - Save raw validated data for the next step
#
# Input:
#   data/raw/merged.csv
#
# Outputs:
#   data/processed/01_raw_validated.csv
#   output/tables/validation_01_column_name_map.csv
#   output/tables/validation_01_required_columns.csv
#   output/tables/validation_01_raw_dataset_summary.csv
# ==============================================================================


# ------------------------------------------------------------------------------
# 1.0 Load setup
# ------------------------------------------------------------------------------

setup_candidates <- c(
  file.path("RCodin", "R", "0_setup.R"),
  file.path(getwd(), "0_setup.R"),
  file.path(path.expand("~"), "Documents", "Timi", "RThesis", "RCodin", "R", "0_setup.R")
)

setup_file <- setup_candidates[file.exists(setup_candidates)][1]

if (is.na(setup_file)) {
  stop(
    "Could not find 0_setup.R. Check that the project is located at.",
    call. = FALSE
  )
}

source(setup_file)


# ------------------------------------------------------------------------------
# 1.1 Read raw data
# ------------------------------------------------------------------------------

raw_data <- safe_read_csv(RAW_DATA_FILE)

message("Raw dataset loaded.")
message("Rows: ", nrow(raw_data))
message("Columns: ", ncol(raw_data))


# ------------------------------------------------------------------------------
# 1.2 Column-name map
# ------------------------------------------------------------------------------

column_name_map <- tibble::tibble(
  raw_name = names(raw_data),
  clean_name_preview = janitor::make_clean_names(names(raw_data))
)

write_table_csv(
  column_name_map,
  "validation_01_column_name_map.csv",
  digits = TABLE_DIGITS
)

clean_names_available <- column_name_map$clean_name_preview


# ------------------------------------------------------------------------------
# 1.3 Required-column validation
# ------------------------------------------------------------------------------

required_column_specs <- tibble::tribble(
  ~canonical_name,         ~role,                    ~acceptable_clean_names,
  "date",                  "Date column",             list(c("date")),
  
  "btc_log_return",         "Dependent variable",      list(c("btc_log_return")),
  "eth_log_return",         "Dependent variable",      list(c("eth_log_return")),
  "usdt_log_return",        "Dependent variable",      list(c("usdt_log_return")),
  
  "sp500_log_return",       "US stock control",        list(c("sp500_log_return")),
  "stoxx50_log_return",     "EU stock control",        list(c("stoxx50_log_return")),
  "dxy_log_return",         "Global market control",   list(c("dxy_log_return")),
  
  "ecb_mp",                 "ECB MP shock",            list(c("ecb_mp")),
  "fed_mp",                 "Fed MP shock",            list(c("fed_mp")),
  
  "us_short",               "US treasury control",     list(c("us_short")),
  "us_long",                "US treasury control",     list(c("us_long")),
  "us_3m",                  "US treasury control",     list(c("us_3m", "us_3m_value")),
  
  "de_short",               "German treasury control", list(c("de_short")),
  "de_long",                "German treasury control", list(c("de_long")),
  "de_3m",                  "German treasury control", list(c("de_3m", "de_3m_value")),
  
  "covid_dummy",            "Dummy variable",          list(c("covid_dummy")),
  "mica_dummy",             "Dummy variable",          list(c("mica_dummy"))
)

find_matching_clean_name <- function(acceptable_names, available_names) {
  matched <- intersect(acceptable_names, available_names)
  
  if (length(matched) == 0) {
    return(NA_character_)
  }
  
  matched[1]
}

column_validation <- required_column_specs %>%
  rowwise() %>%
  mutate(
    matched_clean_name = find_matching_clean_name(
      acceptable_clean_names[[1]],
      clean_names_available
    ),
    found = !is.na(matched_clean_name),
    matched_raw_name = ifelse(
      found,
      column_name_map$raw_name[
        match(matched_clean_name, column_name_map$clean_name_preview)
      ],
      NA_character_
    ),
    accepted_names = paste(acceptable_clean_names[[1]], collapse = " | ")
  ) %>%
  ungroup() %>%
  select(
    canonical_name,
    role,
    found,
    matched_raw_name,
    matched_clean_name,
    accepted_names
  )

write_table_csv(
  column_validation,
  "validation_01_required_columns.csv",
  digits = TABLE_DIGITS
)

missing_required <- column_validation %>%
  filter(!found)

if (nrow(missing_required) > 0) {
  stop(
    "The raw dataset is missing required columns: ",
    paste(missing_required$canonical_name, collapse = ", "),
    call. = FALSE
  )
}

message("Required-column validation passed.")


# ------------------------------------------------------------------------------
# 1.4 Helper functions for validation
# ------------------------------------------------------------------------------

get_raw_col_from_canonical <- function(target_canonical_name) {
  matched <- column_validation %>%
    filter(.data$canonical_name == .env$target_canonical_name) %>%
    pull(.data$matched_raw_name)
  
  if (length(matched) == 0 || all(is.na(matched))) {
    stop(
      "No raw column found for canonical variable: ",
      target_canonical_name,
      call. = FALSE
    )
  }
  
  matched[1]
}

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

count_nonzero <- function(x) {
  x_num <- as_numeric_safely(x)
  sum(!is.na(x_num) & x_num != 0)
}

count_positive <- function(x) {
  x_num <- as_numeric_safely(x)
  sum(!is.na(x_num) & x_num > 0)
}

count_negative <- function(x) {
  x_num <- as_numeric_safely(x)
  sum(!is.na(x_num) & x_num < 0)
}

count_zero_observed <- function(x) {
  x_num <- as_numeric_safely(x)
  sum(!is.na(x_num) & x_num == 0)
}


# ------------------------------------------------------------------------------
# 1.5 Date validation
# ------------------------------------------------------------------------------

date_raw_col <- get_raw_col_from_canonical("date")
date_parsed <- parse_project_date(raw_data[[date_raw_col]])

date_validation <- tibble::tibble(
  metric = c(
    "raw_date_column",
    "rows_total",
    "dates_parsed",
    "dates_failed_to_parse",
    "first_date",
    "last_date",
    "duplicate_dates",
    "dates_sorted_non_decreasing"
  ),
  value = c(
    date_raw_col,
    as.character(nrow(raw_data)),
    as.character(sum(!is.na(date_parsed))),
    as.character(sum(is.na(date_parsed))),
    as.character(min(date_parsed, na.rm = TRUE)),
    as.character(max(date_parsed, na.rm = TRUE)),
    as.character(sum(duplicated(date_parsed[!is.na(date_parsed)]))),
    as.character(all(diff(date_parsed[!is.na(date_parsed)]) >= 0))
  )
)

if (sum(is.na(date_parsed)) > 0) {
  stop(
    "Some date values could not be parsed. Check validation_01_raw_dataset_summary.csv.",
    call. = FALSE
  )
}

if (sum(duplicated(date_parsed[!is.na(date_parsed)])) > 0) {
  warning(
    "Duplicate dates were found in the raw dataset. This may or may not be intentional.",
    call. = FALSE
  )
}

if (!all(diff(date_parsed[!is.na(date_parsed)]) >= 0)) {
  warning(
    "Dates are not sorted in non-decreasing order. Step 2 will sort by date.",
    call. = FALSE
  )
}

message("Date validation passed.")


# ------------------------------------------------------------------------------
# 1.6 Variable-level diagnostics
# ------------------------------------------------------------------------------

diagnostic_vars <- column_validation$canonical_name

variable_diagnostics <- purrr::map_dfr(
  diagnostic_vars,
  function(var_name) {
    raw_col <- get_raw_col_from_canonical(var_name)
    x <- raw_data[[raw_col]]
    
    if (var_name == "date") {
      return(
        tibble::tibble(
          canonical_name = var_name,
          raw_column = raw_col,
          class = paste(class(x), collapse = " / "),
          n_total = length(x),
          n_missing = sum(is.na(x)),
          n_non_missing = sum(!is.na(x)),
          n_distinct_non_missing = dplyr::n_distinct(x, na.rm = TRUE),
          min_value = as.character(min(date_parsed, na.rm = TRUE)),
          max_value = as.character(max(date_parsed, na.rm = TRUE))
        )
      )
    }
    
    x_num <- as_numeric_safely(x)
    
    tibble::tibble(
      canonical_name = var_name,
      raw_column = raw_col,
      class = paste(class(x), collapse = " / "),
      n_total = length(x),
      n_missing = sum(is.na(x_num)),
      n_non_missing = sum(!is.na(x_num)),
      n_distinct_non_missing = dplyr::n_distinct(x_num, na.rm = TRUE),
      min_value = as.character(suppressWarnings(min(x_num, na.rm = TRUE))),
      max_value = as.character(suppressWarnings(max(x_num, na.rm = TRUE)))
    )
  }
)


# ------------------------------------------------------------------------------
# 1.7 Shock and trading-day diagnostics
# ------------------------------------------------------------------------------

ecb_mp_raw_col <- get_raw_col_from_canonical("ecb_mp")
fed_mp_raw_col <- get_raw_col_from_canonical("fed_mp")

stoxx50_raw_col <- get_raw_col_from_canonical("stoxx50_log_return")
sp500_raw_col <- get_raw_col_from_canonical("sp500_log_return")

ecb_mp <- as_numeric_safely(raw_data[[ecb_mp_raw_col]])
fed_mp <- as_numeric_safely(raw_data[[fed_mp_raw_col]])

stoxx50_lr <- as_numeric_safely(raw_data[[stoxx50_raw_col]])
sp500_lr <- as_numeric_safely(raw_data[[sp500_raw_col]])

shock_and_sample_diagnostics <- tibble::tibble(
  metric = c(
    "ecb_mp_observed_meetings",
    "ecb_mp_nonzero_shocks",
    "ecb_mp_positive_shocks",
    "ecb_mp_negative_shocks",
    "ecb_mp_zero_observed_shocks",
    
    "fed_mp_observed_meetings",
    "fed_mp_nonzero_shocks",
    "fed_mp_positive_shocks",
    "fed_mp_negative_shocks",
    "fed_mp_zero_observed_shocks",
    
    "eu_stock_trading_days_stoxx50_available",
    "us_stock_trading_days_sp500_available",
    
    "rows_with_both_stoxx50_and_sp500_returns",
    "rows_with_neither_stoxx50_nor_sp500_returns"
  ),
  value = c(
    as.character(sum(!is.na(ecb_mp))),
    as.character(count_nonzero(ecb_mp)),
    as.character(count_positive(ecb_mp)),
    as.character(count_negative(ecb_mp)),
    as.character(count_zero_observed(ecb_mp)),
    
    as.character(sum(!is.na(fed_mp))),
    as.character(count_nonzero(fed_mp)),
    as.character(count_positive(fed_mp)),
    as.character(count_negative(fed_mp)),
    as.character(count_zero_observed(fed_mp)),
    
    as.character(sum(!is.na(stoxx50_lr))),
    as.character(sum(!is.na(sp500_lr))),
    
    as.character(sum(!is.na(stoxx50_lr) & !is.na(sp500_lr))),
    as.character(sum(is.na(stoxx50_lr) & is.na(sp500_lr)))
  )
)


# ------------------------------------------------------------------------------
# 1.8 Combined validation summary
# ------------------------------------------------------------------------------

raw_dataset_summary <- dplyr::bind_rows(
  tibble::tibble(section = "date_validation", date_validation),
  tibble::tibble(section = "shock_and_sample_diagnostics", shock_and_sample_diagnostics)
)

write_table_csv(
  raw_dataset_summary,
  "validation_01_raw_dataset_summary.csv",
  digits = TABLE_DIGITS
)

write_table_csv(
  variable_diagnostics,
  "validation_01_variable_diagnostics.csv",
  digits = TABLE_DIGITS
)


# ------------------------------------------------------------------------------
# 1.9 Save validated raw data
# ------------------------------------------------------------------------------

validated_output_file <- file.path(PATHS$data_processed, "01_raw_validated.csv")

readr::write_csv(
  raw_data,
  validated_output_file,
  na = ""
)

message("Saved validated raw data: ", validated_output_file)


# ------------------------------------------------------------------------------
# 1.10 Final message
# ------------------------------------------------------------------------------

message("Step 1 complete: raw import and validation finished.")