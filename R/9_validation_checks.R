# ==============================================================================
# 9_validation_checks.R
# Thesis project: The Effect of Monetary Policy Announcements on Cryptocurrency Returns
#
# Purpose:
#   - Run final consistency checks across the full workflow
#   - Verify model samples, shock counts, IRF outputs, tables, and figures
#   - Save a final audit log for reproducibility and supervisor review
#
# Inputs:
#   data/processed/02_clean_daily_data.csv
#   data/processed/03_model_sample_ecb.csv
#   data/processed/03_model_sample_fed.csv
#   data/processed/05_irf_results_long.csv
#   output/tables/*.csv
#   output/figures/*.png
#
# Outputs:
#   output/tables/validation_09_final_audit_log.csv
#   output/tables/validation_09_required_output_files.csv
#   output/tables/validation_09_model_sample_irf_consistency.csv
#   output/tables/validation_09_irf_horizon_status.csv
#   output/tables/validation_09_final_summary.csv
# ==============================================================================


# ------------------------------------------------------------------------------
# 9.0 Load setup
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
# 9.1 Define expected workflow files
# ------------------------------------------------------------------------------

expected_processed_files <- tibble::tibble(
  category = "processed_data",
  step = c(
    "Step 1",
    "Step 2",
    "Step 3",
    "Step 3",
    "Step 5",
    "Step 5",
    "Step 5"
  ),
  file_name = c(
    "01_raw_validated.csv",
    "02_clean_daily_data.csv",
    "03_model_sample_ecb.csv",
    "03_model_sample_fed.csv",
    "05_irf_results_long.csv",
    "05_irf_model_configs.csv",
    "05_irf_estimation_objects.rds"
  ),
  path = file.path(
    PATHS$data_processed,
    c(
      "01_raw_validated.csv",
      "02_clean_daily_data.csv",
      "03_model_sample_ecb.csv",
      "03_model_sample_fed.csv",
      "05_irf_results_long.csv",
      "05_irf_model_configs.csv",
      "05_irf_estimation_objects.rds"
    )
  )
)

expected_table_files <- tibble::tibble(
  category = "tables",
  step = c(
    "Step 6",
    "Step 6",
    "Step 6",
    "Step 6",
    "Step 6",
    "Step 6",
    "Step 7",
    "Step 8"
  ),
  file_name = c(
    "table_01_descriptive_statistics_all_variables.csv",
    "table_03_crypto_stock_return_summary.csv",
    "table_04_mp_surprise_summary_statistics.csv",
    "table_05_control_variable_correlation_matrix.csv",
    "table_11_event_counts_shock_direction_breakdown.csv",
    "table_12_announcement_timing_summary.csv",
    "table_10_irf_detailed_results.csv",
    "validation_08_figure_generation_summary.csv"
  ),
  path = file.path(
    PATHS$tables,
    c(
      "table_01_descriptive_statistics_all_variables.csv",
      "table_03_crypto_stock_return_summary.csv",
      "table_04_mp_surprise_summary_statistics.csv",
      "table_05_control_variable_correlation_matrix.csv",
      "table_11_event_counts_shock_direction_breakdown.csv",
      "table_12_announcement_timing_summary.csv",
      "table_10_irf_detailed_results.csv",
      "validation_08_figure_generation_summary.csv"
    )
  )
)

expected_figure_files <- tibble::tibble(
  category = "figures",
  step = "Step 8",
  file_name = c(
    "figure_02_returns_time_series.png",
    "figure_06_irf_6_grid_baseline.png",
    "figure_07_central_bank_asymmetry.png",
    "figure_08_cross_currency_heterogeneity.png",
    "figure_09_return_distributions.png",
    "figure_13_scatter_crypto_returns_mp_shocks.png"
  ),
  path = file.path(
    PATHS$figures,
    c(
      "figure_02_returns_time_series.png",
      "figure_06_irf_6_grid_baseline.png",
      "figure_07_central_bank_asymmetry.png",
      "figure_08_cross_currency_heterogeneity.png",
      "figure_09_return_distributions.png",
      "figure_13_scatter_crypto_returns_mp_shocks.png"
    )
  )
)

required_output_files <- bind_rows(
  expected_processed_files,
  expected_table_files,
  expected_figure_files
) %>%
  mutate(
    file_exists = file.exists(.data$path),
    file_size_bytes = ifelse(
      .data$file_exists,
      file.info(.data$path)$size,
      NA_real_
    ),
    file_nonempty = .data$file_exists & !is.na(.data$file_size_bytes) & .data$file_size_bytes > 0,
    status = case_when(
      .data$file_nonempty ~ "OK",
      .data$file_exists & !.data$file_nonempty ~ "Exists but empty",
      TRUE ~ "Missing"
    )
  )

write_table_csv(
  required_output_files,
  "validation_09_required_output_files.csv",
  digits = TABLE_DIGITS
)

missing_or_empty_files <- required_output_files %>%
  filter(.data$status != "OK")

if (nrow(missing_or_empty_files) > 0) {
  warning(
    "Some expected workflow files are missing or empty. Check validation_09_required_output_files.csv.",
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 9.2 Read core processed data
# ------------------------------------------------------------------------------

core_files <- c(
  clean_data = file.path(PATHS$data_processed, "02_clean_daily_data.csv"),
  ecb_sample = file.path(PATHS$data_processed, "03_model_sample_ecb.csv"),
  fed_sample = file.path(PATHS$data_processed, "03_model_sample_fed.csv"),
  irf_results = file.path(PATHS$data_processed, "05_irf_results_long.csv")
)

missing_core_files <- core_files[!file.exists(core_files)]

if (length(missing_core_files) > 0) {
  stop(
    "Missing core files required for final validation: ",
    paste(missing_core_files, collapse = ", "),
    call. = FALSE
  )
}

clean_data <- safe_read_csv(core_files["clean_data"]) %>%
  mutate(date = as.Date(.data$date))

ecb_sample <- safe_read_csv(core_files["ecb_sample"]) %>%
  mutate(date = as.Date(.data$date))

fed_sample <- safe_read_csv(core_files["fed_sample"]) %>%
  mutate(date = as.Date(.data$date))

irf_results <- safe_read_csv(core_files["irf_results"])


# ------------------------------------------------------------------------------
# 9.3 Validate required columns in core data
# ------------------------------------------------------------------------------

check_required_columns(
  clean_data,
  unique(c(
    DATE_VAR,
    CRYPTO_RETURN_VARS,
    STOCK_RETURN_VARS,
    SHOCK_VARS,
    FED_BASELINE_CONTROL_VARS,
    ECB_BASELINE_CONTROL_VARS,
    "ecb_mp_observed",
    "fed_mp_observed",
    "ecb_meeting",
    "fed_meeting",
    "eu_trading_day",
    "us_trading_day"
  )),
  data_name = "clean_data"
)

check_required_columns(
  ecb_sample,
  unique(c(
    DATE_VAR,
    "trading_day_index",
    CRYPTO_RETURN_VARS,
    "ecb_mp",
    "ecb_mp_observed",
    "ecb_meeting",
    "ecb_mp_nonzero",
    ECB_CONTROL_VARS
  )),
  data_name = "ecb_sample"
)

check_required_columns(
  fed_sample,
  unique(c(
    DATE_VAR,
    "trading_day_index",
    CRYPTO_RETURN_VARS,
    "fed_mp",
    "fed_mp_observed",
    "fed_meeting",
    "fed_mp_nonzero",
    FED_CONTROL_VARS
  )),
  data_name = "fed_sample"
)

check_required_columns(
  irf_results,
  c(
    "central_bank",
    "asset",
    "asset_var",
    "shock_var",
    "horizon",
    "coefficient",
    "std_error",
    "p_value",
    "conf_low",
    "conf_high",
    "significant_5pct",
    "n_obs",
    "n_nonzero_shocks",
    "regression_status"
  ),
  data_name = "irf_results"
)


# ------------------------------------------------------------------------------
# 9.4 Model sample and shock consistency checks
# ------------------------------------------------------------------------------

model_sample_summary <- tibble::tibble(
  central_bank = c("ECB", "Fed"),
  sample_rows = c(nrow(ecb_sample), nrow(fed_sample)),
  first_date = c(
    as.character(min(ecb_sample$date, na.rm = TRUE)),
    as.character(min(fed_sample$date, na.rm = TRUE))
  ),
  last_date = c(
    as.character(max(ecb_sample$date, na.rm = TRUE)),
    as.character(max(fed_sample$date, na.rm = TRUE))
  ),
  announcement_days = c(
    sum(ecb_sample$ecb_meeting == 1, na.rm = TRUE),
    sum(fed_sample$fed_meeting == 1, na.rm = TRUE)
  ),
  non_announcement_days = c(
    sum(ecb_sample$ecb_meeting == 0, na.rm = TRUE),
    sum(fed_sample$fed_meeting == 0, na.rm = TRUE)
  ),
  nonzero_mp_shocks = c(
    sum(ecb_sample$ecb_mp_nonzero == 1, na.rm = TRUE),
    sum(fed_sample$fed_mp_nonzero == 1, na.rm = TRUE)
  ),
  shock_na_count = c(
    sum(is.na(ecb_sample$ecb_mp)),
    sum(is.na(fed_sample$fed_mp))
  ),
  crypto_na_rows_any = c(
    sum(!stats::complete.cases(ecb_sample[, CRYPTO_RETURN_VARS])),
    sum(!stats::complete.cases(fed_sample[, CRYPTO_RETURN_VARS]))
  ),
  rhs_na_rows_any = c(
    sum(!stats::complete.cases(ecb_sample[, unique(c("ecb_mp", ECB_CONTROL_VARS))])),
    sum(!stats::complete.cases(fed_sample[, unique(c("fed_mp", FED_CONTROL_VARS))]))
  ),
  lag5_control_na_rows_any = c(
    sum(!stats::complete.cases(ecb_sample[, CRYPTO_5D_LAG_RETURN_VARS])),
    sum(!stats::complete.cases(fed_sample[, CRYPTO_5D_LAG_RETURN_VARS]))
  )
) %>%
  mutate(
    sample_status = case_when(
      .data$sample_rows == 0 ~ "FAIL: sample has zero rows",
      .data$nonzero_mp_shocks == 0 ~ "FAIL: no nonzero MP shocks",
      .data$shock_na_count > 0 ~ "FAIL: shock variable has NA",
      .data$rhs_na_rows_any > 0 ~ "FAIL: RHS variables have missing values",
      TRUE ~ "OK"
    )
  )


# ------------------------------------------------------------------------------
# 9.5 IRF horizon and regression-status checks
# ------------------------------------------------------------------------------

expected_irf_rows <- length(CRYPTO_RETURN_VARS) * 2L * length(HORIZONS)

irf_horizon_status <- irf_results %>%
  group_by(.data$central_bank, .data$asset, .data$asset_var, .data$shock_var) %>%
  summarise(
    rows = n(),
    min_horizon = min(.data$horizon, na.rm = TRUE),
    max_horizon = max(.data$horizon, na.rm = TRUE),
    unique_horizons = dplyr::n_distinct(.data$horizon),
    missing_expected_horizons = paste(
      setdiff(HORIZONS, sort(unique(.data$horizon))),
      collapse = ", "
    ),
    rows_ok = sum(.data$regression_status == "OK", na.rm = TRUE),
    rows_not_ok = sum(.data$regression_status != "OK", na.rm = TRUE),
    coefficients_missing = sum(is.na(.data$coefficient)),
    standard_errors_missing = sum(is.na(.data$std_error)),
    p_values_missing = sum(is.na(.data$p_value)),
    min_n_obs = min(.data$n_obs, na.rm = TRUE),
    max_n_obs = max(.data$n_obs, na.rm = TRUE),
    significant_horizons_5pct = sum(.data$significant_5pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    missing_expected_horizons = ifelse(
      .data$missing_expected_horizons == "",
      "None",
      .data$missing_expected_horizons
    ),
    horizon_status = case_when(
      .data$unique_horizons != length(HORIZONS) ~ "FAIL: missing horizons",
      .data$min_horizon != min(HORIZONS) ~ "FAIL: min horizon incorrect",
      .data$max_horizon != max(HORIZONS) ~ "FAIL: max horizon incorrect",
      .data$rows_not_ok > 0 ~ "FAIL: at least one regression not OK",
      .data$coefficients_missing > 0 ~ "FAIL: missing coefficients",
      TRUE ~ "OK"
    )
  ) %>%
  arrange(
    factor(.data$central_bank, levels = c("ECB", "Fed")),
    factor(.data$asset, levels = c("BTC", "ETH", "USDT"))
  )

write_table_csv(
  irf_horizon_status,
  "validation_09_irf_horizon_status.csv",
  digits = TABLE_DIGITS
)


# ------------------------------------------------------------------------------
# 9.6 Model sample versus IRF consistency
# ------------------------------------------------------------------------------

sample_nonzero_counts <- tibble::tibble(
  central_bank = c("ECB", "Fed"),
  expected_shock_var = c("ecb_mp", "fed_mp"),
  sample_nonzero_shocks = c(
    sum(ecb_sample$ecb_mp != 0, na.rm = TRUE),
    sum(fed_sample$fed_mp != 0, na.rm = TRUE)
  ),
  sample_rows = c(
    nrow(ecb_sample),
    nrow(fed_sample)
  )
)

irf_nonzero_counts <- irf_results %>%
  group_by(.data$central_bank, .data$shock_var) %>%
  summarise(
    min_irf_nonzero_shocks = min(.data$n_nonzero_shocks, na.rm = TRUE),
    max_irf_nonzero_shocks = max(.data$n_nonzero_shocks, na.rm = TRUE),
    min_irf_n_obs = min(.data$n_obs, na.rm = TRUE),
    max_irf_n_obs = max(.data$n_obs, na.rm = TRUE),
    .groups = "drop"
  )

model_sample_irf_consistency <- sample_nonzero_counts %>%
  left_join(
    irf_nonzero_counts,
    by = c("central_bank", "expected_shock_var" = "shock_var")
  ) %>%
  mutate(
    nonzero_shock_count_consistent = .data$sample_nonzero_shocks == .data$max_irf_nonzero_shocks,
    irf_obs_not_above_sample_rows = .data$max_irf_n_obs <= .data$sample_rows,
    consistency_status = case_when(
      is.na(.data$max_irf_nonzero_shocks) ~ "FAIL: no matching IRF result",
      !.data$nonzero_shock_count_consistent ~ "CHECK: nonzero shock counts differ across horizons",
      !.data$irf_obs_not_above_sample_rows ~ "FAIL: IRF observations exceed sample rows",
      TRUE ~ "OK"
    )
  )

write_table_csv(
  model_sample_irf_consistency,
  "validation_09_model_sample_irf_consistency.csv",
  digits = TABLE_DIGITS
)


# ------------------------------------------------------------------------------
# 9.7 Final audit log
# ------------------------------------------------------------------------------

audit_items <- tibble::tibble(
  check_id = c(
    "A01",
    "A02",
    "A03",
    "A04",
    "A05",
    "A06",
    "A07",
    "A08",
    "A09",
    "A10",
    "A11",
    "A12",
    "A13",
    "A14"
  ),
  check_name = c(
    "All required output files exist and are nonempty",
    "Clean daily dataset has rows",
    "ECB model sample has rows",
    "Fed model sample has rows",
    "ECB shock has no NA in model sample",
    "Fed shock has no NA in model sample",
    "ECB sample has nonzero monetary policy shocks",
    "Fed sample has nonzero monetary policy shocks",
    "IRF result row count equals expected count",
    "All six IRF models have horizons 0 to 25",
    "All IRF horizon regressions returned OK",
    "No IRF coefficients are missing",
    "Sample and IRF nonzero shock counts are consistent",
    "Required figures exist"
  ),
  passed = c(
    all(required_output_files$status == "OK"),
    nrow(clean_data) > 0,
    nrow(ecb_sample) > 0,
    nrow(fed_sample) > 0,
    sum(is.na(ecb_sample$ecb_mp)) == 0,
    sum(is.na(fed_sample$fed_mp)) == 0,
    sum(ecb_sample$ecb_mp != 0, na.rm = TRUE) > 0,
    sum(fed_sample$fed_mp != 0, na.rm = TRUE) > 0,
    nrow(irf_results) == expected_irf_rows,
    all(irf_horizon_status$horizon_status == "OK"),
    sum(irf_results$regression_status != "OK", na.rm = TRUE) == 0,
    sum(is.na(irf_results$coefficient)) == 0,
    all(model_sample_irf_consistency$consistency_status == "OK"),
    all(expected_figure_files %>%
          mutate(file_exists = file.exists(.data$path),
                 file_nonempty = .data$file_exists & file.info(.data$path)$size > 0) %>%
          pull(.data$file_nonempty))
  )
) %>%
  mutate(
    status = ifelse(.data$passed, "PASS", "FAIL")
  )

write_table_csv(
  audit_items,
  "validation_09_final_audit_log.csv",
  digits = TABLE_DIGITS
)


# ------------------------------------------------------------------------------
# 9.8 Final summary
# ------------------------------------------------------------------------------

final_summary <- tibble::tibble(
  metric = c(
    "audit_run_time",
    "project_root",
    "clean_daily_rows",
    "ecb_sample_rows",
    "fed_sample_rows",
    "ecb_announcement_days",
    "fed_announcement_days",
    "ecb_nonzero_mp_shocks",
    "fed_nonzero_mp_shocks",
    "irf_models_expected",
    "irf_horizons_per_model",
    "irf_rows_expected",
    "irf_rows_actual",
    "irf_rows_status_ok",
    "irf_rows_status_not_ok",
    "required_files_ok",
    "required_files_missing_or_empty",
    "audit_checks_passed",
    "audit_checks_failed",
    "overall_status"
  ),
  value = c(
    as.character(Sys.time()),
    PROJECT_ROOT,
    as.character(nrow(clean_data)),
    as.character(nrow(ecb_sample)),
    as.character(nrow(fed_sample)),
    as.character(sum(ecb_sample$ecb_meeting == 1, na.rm = TRUE)),
    as.character(sum(fed_sample$fed_meeting == 1, na.rm = TRUE)),
    as.character(sum(ecb_sample$ecb_mp != 0, na.rm = TRUE)),
    as.character(sum(fed_sample$fed_mp != 0, na.rm = TRUE)),
    as.character(6),
    as.character(length(HORIZONS)),
    as.character(expected_irf_rows),
    as.character(nrow(irf_results)),
    as.character(sum(irf_results$regression_status == "OK", na.rm = TRUE)),
    as.character(sum(irf_results$regression_status != "OK", na.rm = TRUE)),
    as.character(sum(required_output_files$status == "OK")),
    as.character(sum(required_output_files$status != "OK")),
    as.character(sum(audit_items$passed)),
    as.character(sum(!audit_items$passed)),
    ifelse(all(audit_items$passed), "PASS", "FAIL")
  )
)

write_table_csv(
  final_summary,
  "validation_09_final_summary.csv",
  digits = TABLE_DIGITS
)


# ------------------------------------------------------------------------------
# 9.9 Console output
# ------------------------------------------------------------------------------

message("Step 9 final audit summary:")

print(final_summary)

failed_checks <- audit_items %>%
  filter(.data$status == "FAIL")

if (nrow(failed_checks) > 0) {
  message("Failed checks:")
  print(failed_checks)
  warning(
    "Step 9 completed, but at least one audit check failed. Review validation_09_final_audit_log.csv.",
    call. = FALSE
  )
} else {
  message("All Step 9 audit checks passed.")
}


# ------------------------------------------------------------------------------
# 9.10 Final message
# ------------------------------------------------------------------------------

message("Step 9 complete: final validation and audit logs saved.")

