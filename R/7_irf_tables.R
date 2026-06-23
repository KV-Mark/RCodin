# ==============================================================================
# 7_irf_tables.R
# Thesis project: The Effect of Monetary Policy Announcements on Cryptocurrency Returns
#
# Purpose:
#   - Generate the detailed IRF regression table required for the thesis
#   - Use the master long-format IRF result file from Step 5
#   - Save table as CSV for MS Word
#   - Keep uniform notation:
#       * 5% significance level
#       * 95% confidence intervals
#       * Newey-West / HAC standard errors
#
# Input:
#   data/processed/05_irf_results_long.csv
#   data/processed/05_irf_model_configs.csv
#
# Outputs:
#   output/tables/table_10_irf_detailed_results.csv
#   output/tables/validation_07_irf_table_summary.csv
#   output/tables/table_notes_07_irf_tables.csv
# ==============================================================================


# ------------------------------------------------------------------------------
# 7.0 Load setup
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
# 7.1 Read Step 5 outputs
# ------------------------------------------------------------------------------

irf_results_file <- file.path(PATHS$data_processed, "05_irf_results_long.csv")
irf_configs_file <- file.path(PATHS$data_processed, "05_irf_model_configs.csv")

if (!file.exists(irf_results_file)) {
  stop(
    "Could not find IRF results file: ",
    irf_results_file,
    "\nRun source('RCodin/R/5_estimate_irfs.R') first.",
    call. = FALSE
  )
}

if (!file.exists(irf_configs_file)) {
  stop(
    "Could not find IRF model config file: ",
    irf_configs_file,
    "\nRun source('RCodin/R/5_estimate_irfs.R') first.",
    call. = FALSE
  )
}

irf_results <- safe_read_csv(irf_results_file)
irf_configs <- safe_read_csv(irf_configs_file)

message("IRF results loaded. Rows: ", nrow(irf_results))
message("IRF model configs loaded. Rows: ", nrow(irf_configs))


# ------------------------------------------------------------------------------
# 7.2 Validate required IRF columns
# ------------------------------------------------------------------------------

required_irf_cols <- c(
  "central_bank",
  "asset",
  "asset_var",
  "shock_var",
  "horizon",
  "coefficient",
  "std_error",
  "t_statistic",
  "p_value",
  "conf_low",
  "conf_high",
  "significant_5pct",
  "significance_5pct",
  "n_obs",
  "n_nonzero_shocks",
  "r_squared",
  "adj_r_squared",
  "f_p_value",
  "nw_lag",
  "regression_status",
  "equation_lhs",
  "equation_rhs"
)

check_required_columns(
  irf_results,
  required_irf_cols,
  data_name = "IRF results from Step 5"
)

message("Step 7 IRF-column validation passed.")


# ------------------------------------------------------------------------------
# 7.3 Helper functions
# ------------------------------------------------------------------------------

format_number_for_table <- function(x, digits = TABLE_DIGITS) {
  ifelse(
    is.na(x),
    "",
    formatC(
      x,
      format = "f",
      digits = digits
    )
  )
}

format_p_value <- function(x, digits = TABLE_DIGITS) {
  dplyr::case_when(
    is.na(x) ~ "",
    x < 0.0001 ~ "<0.0001",
    TRUE ~ formatC(x, format = "f", digits = digits)
  )
}

format_coefficient_with_star <- function(coefficient, significance_marker, digits = TABLE_DIGITS) {
  coef_text <- format_number_for_table(coefficient, digits = digits)
  
  ifelse(
    coef_text == "",
    "",
    paste0(coef_text, significance_marker)
  )
}

format_parentheses <- function(x, digits = TABLE_DIGITS) {
  x_text <- format_number_for_table(x, digits = digits)
  
  ifelse(
    x_text == "",
    "",
    paste0("(", x_text, ")")
  )
}

format_ci <- function(low, high, digits = TABLE_DIGITS) {
  low_text <- format_number_for_table(low, digits = digits)
  high_text <- format_number_for_table(high, digits = digits)
  
  ifelse(
    low_text == "" | high_text == "",
    "",
    paste0("[", low_text, ", ", high_text, "]")
  )
}

standardize_central_bank_order <- function(x) {
  factor(x, levels = c("ECB", "Fed"))
}

standardize_asset_order <- function(x) {
  factor(x, levels = c("BTC", "ETH", "USDT"))
}


# ------------------------------------------------------------------------------
# 7.4 Create detailed IRF table
# ------------------------------------------------------------------------------

table_10_irf_detailed <- irf_results %>%
  mutate(
    central_bank = as.character(.data$central_bank),
    asset = as.character(.data$asset),
    central_bank_order = standardize_central_bank_order(.data$central_bank),
    asset_order = standardize_asset_order(.data$asset),
    
    coefficient_star = format_coefficient_with_star(
      .data$coefficient,
      .data$significance_5pct,
      digits = TABLE_DIGITS
    ),
    standard_error_parentheses = format_parentheses(
      .data$std_error,
      digits = TABLE_DIGITS
    ),
    confidence_interval_95 = format_ci(
      .data$conf_low,
      .data$conf_high,
      digits = TABLE_DIGITS
    ),
    p_value_formatted = format_p_value(
      .data$p_value,
      digits = TABLE_DIGITS
    ),
    significance_rule = "Significant at 5% if p < 0.05",
    confidence_interval_rule = "95% confidence interval"
  ) %>%
  arrange(
    .data$central_bank_order,
    .data$asset_order,
    .data$horizon
  ) %>%
  select(
    central_bank,
    asset,
    horizon,
    coefficient,
    std_error,
    t_statistic,
    p_value,
    conf_low,
    conf_high,
    significant_5pct,
    coefficient_star,
    standard_error_parentheses,
    confidence_interval_95,
    p_value_formatted,
    n_obs,
    n_nonzero_shocks,
    r_squared,
    adj_r_squared,
    f_p_value,
    nw_lag,
    regression_status,
    shock_var,
    asset_var,
    equation_lhs,
    equation_rhs,
    significance_rule,
    confidence_interval_rule
  )


# ------------------------------------------------------------------------------
# 7.5 Check for failed regressions before saving
# ------------------------------------------------------------------------------

failed_irf_rows <- table_10_irf_detailed %>%
  filter(.data$regression_status != "OK")

if (nrow(failed_irf_rows) > 0) {
  warning(
    "Some IRF rows have regression_status not equal to OK. They will still be saved, ",
    "but check validation_07_irf_table_summary.csv.",
    call. = FALSE
  )
}

if (all(is.na(table_10_irf_detailed$coefficient))) {
  stop(
    "All IRF coefficients are NA. Table 10 would be unusable.",
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 7.6 Validation summary
# ------------------------------------------------------------------------------

validation_07_summary <- table_10_irf_detailed %>%
  group_by(.data$central_bank, .data$asset, .data$shock_var, .data$asset_var) %>%
  summarise(
    horizons_in_table = n(),
    min_horizon = min(.data$horizon, na.rm = TRUE),
    max_horizon = max(.data$horizon, na.rm = TRUE),
    rows_ok = sum(.data$regression_status == "OK", na.rm = TRUE),
    rows_not_ok = sum(.data$regression_status != "OK", na.rm = TRUE),
    min_n_obs = min(.data$n_obs, na.rm = TRUE),
    max_n_obs = max(.data$n_obs, na.rm = TRUE),
    nonzero_shocks_min = min(.data$n_nonzero_shocks, na.rm = TRUE),
    nonzero_shocks_max = max(.data$n_nonzero_shocks, na.rm = TRUE),
    significant_horizons_5pct = sum(.data$significant_5pct, na.rm = TRUE),
    first_significant_horizon = ifelse(
      any(.data$significant_5pct, na.rm = TRUE),
      min(.data$horizon[.data$significant_5pct], na.rm = TRUE),
      NA_integer_
    ),
    last_significant_horizon = ifelse(
      any(.data$significant_5pct, na.rm = TRUE),
      max(.data$horizon[.data$significant_5pct], na.rm = TRUE),
      NA_integer_
    ),
    min_coefficient = min(.data$coefficient, na.rm = TRUE),
    max_coefficient = max(.data$coefficient, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    standardize_central_bank_order(.data$central_bank),
    standardize_asset_order(.data$asset)
  )


# ------------------------------------------------------------------------------
# 7.7 Table notes
# ------------------------------------------------------------------------------

table_notes_07 <- tibble::tibble(
  table_file = c(
    "table_10_irf_detailed_results.csv"
  ),
  notes = c(
    paste0(
      "Notes: This table reports detailed local projection impulse response results ",
      "for BTC, ETH, and USDT daily log returns to ECB and Fed monetary policy surprises. ",
      "The horizon column reports trading-day horizons h = 0 to h = 25. ",
      "The coefficient is the estimated beta_h from the local projection regression of ",
      "the return at t+h on the monetary policy surprise at t and the relevant controls. ",
      "Standard errors are Newey-West/HAC standard errors. Confidence intervals are 95%. ",
      "An asterisk (*) denotes statistical significance at the 5% level. ",
      "ECB specifications use STOXX50, DXY, German treasury variables, COVID dummy, and MiCA dummy as controls. ",
      "Fed specifications use S&P 500, DXY, US treasury variables, and COVID dummy as controls."
    )
  )
)


# ------------------------------------------------------------------------------
# 7.8 Save outputs
# ------------------------------------------------------------------------------

write_table_csv(
  table_10_irf_detailed,
  "table_10_irf_detailed_results.csv",
  digits = TABLE_DIGITS
)

write_table_csv(
  validation_07_summary,
  "validation_07_irf_table_summary.csv",
  digits = TABLE_DIGITS
)

readr::write_csv(
  table_notes_07,
  file.path(PATHS$tables, "table_notes_07_irf_tables.csv"),
  na = ""
)

message("Saved table notes: ", file.path(PATHS$tables, "table_notes_07_irf_tables.csv"))


# ------------------------------------------------------------------------------
# 7.9 Console preview
# ------------------------------------------------------------------------------

message("Step 7 IRF detailed table preview:")

print(
  table_10_irf_detailed %>%
    filter(.data$horizon %in% c(0, 1, 5, 10, 25)) %>%
    select(
      central_bank,
      asset,
      horizon,
      coefficient_star,
      standard_error_parentheses,
      p_value_formatted,
      confidence_interval_95,
      n_obs,
      regression_status
    ) %>%
    arrange(
      standardize_central_bank_order(.data$central_bank),
      standardize_asset_order(.data$asset),
      .data$horizon
    )
)

message("Step 7 validation summary preview:")

print(validation_07_summary)


# ------------------------------------------------------------------------------
# 7.10 Final message
# ------------------------------------------------------------------------------

message("Step 7 complete: detailed IRF table saved.")

