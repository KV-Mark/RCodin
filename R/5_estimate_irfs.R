# ==============================================================================
# 5_estimate_irfs.R
# Thesis project: The Effect of Monetary Policy Announcements on Cryptocurrency Returns
#
# Purpose:
#   - Load ECB and Fed model samples from Step 3
#   - Source the LPIRF wrapper from Step 4
#   - Estimate the six baseline IRFs:
#       ECB -> BTC, ETH, USDT
#       Fed -> BTC, ETH, USDT
#   - Save one master long-format IRF result file
#   - Save model configuration and estimation validation tables
#
# Inputs:
#   data/processed/03_model_sample_ecb.csv
#   data/processed/03_model_sample_fed.csv
#   RCodin/R/4_lpirf_wrapper.R
#
# Outputs:
#   data/processed/05_irf_results_long.csv
#   data/processed/05_irf_model_configs.csv
#   data/processed/05_irf_estimation_objects.rds
#   data/processed/lpirfs_objects/*.rds
#
#   output/tables/validation_05_irf_estimation_summary.csv
#   output/tables/validation_05_irf_status_by_model.csv
#   output/tables/validation_05_irf_observation_counts.csv
#   output/tables/validation_05_lpirfs_package_status.csv
# ==============================================================================


# ------------------------------------------------------------------------------
# 5.0 Load setup and wrapper
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

wrapper_candidates <- c(
  file.path(PATHS$code, "4_lpirf_wrapper.R"),
  file.path("RCodin", "R", "4_lpirf_wrapper.R"),
  file.path(getwd(), "4_lpirf_wrapper.R")
)

wrapper_file <- wrapper_candidates[file.exists(wrapper_candidates)][1]

if (is.na(wrapper_file)) {
  stop(
    "Could not find 4_lpirf_wrapper.R. Run/save Step 4 first.",
    call. = FALSE
  )
}

source(wrapper_file)


# ------------------------------------------------------------------------------
# 5.1 Read Step 3 model samples
# ------------------------------------------------------------------------------

ecb_sample_file <- file.path(PATHS$data_processed, "03_model_sample_ecb.csv")
fed_sample_file <- file.path(PATHS$data_processed, "03_model_sample_fed.csv")

if (!file.exists(ecb_sample_file)) {
  stop(
    "Could not find ECB model sample: ",
    ecb_sample_file,
    "\nRun source('RCodin/R/3_make_model_samples.R') first.",
    call. = FALSE
  )
}

if (!file.exists(fed_sample_file)) {
  stop(
    "Could not find Fed model sample: ",
    fed_sample_file,
    "\nRun source('RCodin/R/3_make_model_samples.R') first.",
    call. = FALSE
  )
}

ecb_sample <- safe_read_csv(ecb_sample_file)
fed_sample <- safe_read_csv(fed_sample_file)

message("ECB sample loaded. Rows: ", nrow(ecb_sample))
message("Fed sample loaded. Rows: ", nrow(fed_sample))


# ------------------------------------------------------------------------------
# 5.2 Validate model-sample variables
# ------------------------------------------------------------------------------

check_required_columns(
  ecb_sample,
  unique(c(
    DATE_VAR,
    CRYPTO_RETURN_VARS,
    "ecb_mp",
    ECB_BASELINE_CONTROL_VARS,
    "ecb_meeting",
    "ecb_mp_nonzero"
  )),
  data_name = "ECB model sample"
)

check_required_columns(
  fed_sample,
  unique(c(
    DATE_VAR,
    CRYPTO_RETURN_VARS,
    "fed_mp",
    FED_BASELINE_CONTROL_VARS,
    "fed_meeting",
    "fed_mp_nonzero"
  )),
  data_name = "Fed model sample"
)

if (any(is.na(ecb_sample$ecb_mp))) {
  stop("ECB sample contains NA in ecb_mp. Step 2/3 should have fixed this.", call. = FALSE)
}

if (any(is.na(fed_sample$fed_mp))) {
  stop("Fed sample contains NA in fed_mp. Step 2/3 should have fixed this.", call. = FALSE)
}

if (sum(ecb_sample$ecb_mp != 0, na.rm = TRUE) == 0) {
  stop("ECB sample has no nonzero ecb_mp shocks. Cannot estimate ECB IRFs.", call. = FALSE)
}

if (sum(fed_sample$fed_mp != 0, na.rm = TRUE) == 0) {
  stop("Fed sample has no nonzero fed_mp shocks. Cannot estimate Fed IRFs.", call. = FALSE)
}

message("Step 5 model-sample validation passed.")


# ------------------------------------------------------------------------------
# 5.3 Define baseline IRF model plan
# ------------------------------------------------------------------------------

# The baseline intentionally estimates only pure monetary policy shocks:
#   - ECB monetary policy shock: ecb_mp
#   - Fed monetary policy shock: fed_mp
#
# CBI shocks are not estimated here because the current thesis baseline focuses
# only on monetary policy announcement surprises.

irf_model_plan <- tibble::tibble(
  central_bank = c("ECB", "Fed"),
  sample_name = c("ecb", "fed"),
  shock_var = c("ecb_mp", "fed_mp"),
  controls = c(
    paste(ECB_BASELINE_CONTROL_VARS, collapse = ", "),
    paste(FED_BASELINE_CONTROL_VARS, collapse = ", ")
  ),
  asset_vars = c(
    paste(CRYPTO_RETURN_VARS, collapse = ", "),
    paste(CRYPTO_RETURN_VARS, collapse = ", ")
  ),
  horizons = paste0(min(HORIZONS), " to ", max(HORIZONS)),
  lags_endog = LP_LAGS_ENDOG,
  lags_exog = LP_LAGS_EXOG,
  trend = LP_TREND,
  nw_lag_rule = NW_LAG_RULE,
  confidence_level = CONF_LEVEL,
  significance_level = SIG_LEVEL
)

write_table_csv(
  irf_model_plan,
  "validation_05_irf_model_plan.csv",
  digits = TABLE_DIGITS
)


# ------------------------------------------------------------------------------
# 5.4 Create folder for optional lpirfs package objects
# ------------------------------------------------------------------------------

lpirfs_rds_dir <- file.path(PATHS$data_processed, "lpirfs_objects")
create_dir_if_missing(lpirfs_rds_dir)


# ------------------------------------------------------------------------------
# 5.5 Estimate ECB baseline IRFs
# ------------------------------------------------------------------------------

message("Estimating ECB baseline IRFs...")

ecb_irf <- estimate_multiple_asset_irfs(
  data = ecb_sample,
  asset_vars = CRYPTO_RETURN_VARS,
  shock_var = "ecb_mp",
  control_vars = ECB_BASELINE_CONTROL_VARS,
  central_bank = "ECB",
  horizons = HORIZONS,
  lags_endog = LP_LAGS_ENDOG,
  lags_exog = LP_LAGS_EXOG,
  trend = LP_TREND,
  nw_lag_rule = NW_LAG_RULE,
  nw_fixed_lag = NULL,
  conf_z = CONF_Z,
  sig_level = SIG_LEVEL,
  run_lpirfs = TRUE,
  save_lpirfs_objects = TRUE,
  rds_directory = lpirfs_rds_dir
)

message("ECB baseline IRFs estimated.")


# ------------------------------------------------------------------------------
# 5.6 Estimate Fed baseline IRFs
# ------------------------------------------------------------------------------

message("Estimating Fed baseline IRFs...")

fed_irf <- estimate_multiple_asset_irfs(
  data = fed_sample,
  asset_vars = CRYPTO_RETURN_VARS,
  shock_var = "fed_mp",
  control_vars = FED_BASELINE_CONTROL_VARS,
  central_bank = "Fed",
  horizons = HORIZONS,
  lags_endog = LP_LAGS_ENDOG,
  lags_exog = LP_LAGS_EXOG,
  trend = LP_TREND,
  nw_lag_rule = NW_LAG_RULE,
  nw_fixed_lag = NULL,
  conf_z = CONF_Z,
  sig_level = SIG_LEVEL,
  run_lpirfs = TRUE,
  save_lpirfs_objects = TRUE,
  rds_directory = lpirfs_rds_dir
)

message("Fed baseline IRFs estimated.")


# ------------------------------------------------------------------------------
# 5.7 Combine results
# ------------------------------------------------------------------------------

irf_results_long <- bind_rows(
  ecb_irf$tidy_results,
  fed_irf$tidy_results
) %>%
  arrange(
    .data$central_bank,
    .data$asset,
    .data$horizon
  )

irf_model_configs <- bind_rows(
  ecb_irf$configs,
  fed_irf$configs
) %>%
  arrange(
    .data$central_bank,
    .data$asset
  )

full_irf_objects <- list(
  ecb = ecb_irf,
  fed = fed_irf,
  estimated_at = Sys.time(),
  horizon_max = HORIZON_MAX,
  horizons = HORIZONS,
  confidence_level = CONF_LEVEL,
  significance_level = SIG_LEVEL
)


# ------------------------------------------------------------------------------
# 5.8 Validate estimation output
# ------------------------------------------------------------------------------

expected_rows <- length(CRYPTO_RETURN_VARS) * length(unique(irf_model_plan$central_bank)) * length(HORIZONS)

if (nrow(irf_results_long) != expected_rows) {
  warning(
    "Unexpected number of IRF result rows. Expected ",
    expected_rows,
    ", got ",
    nrow(irf_results_long),
    ". Check validation_05_irf_status_by_model.csv.",
    call. = FALSE
  )
}

bad_status <- irf_results_long %>%
  filter(.data$regression_status != "OK")

if (nrow(bad_status) > 0) {
  warning(
    "Some horizon regressions did not return status OK. Check validation_05_irf_status_by_model.csv.",
    call. = FALSE
  )
}

if (all(is.na(irf_results_long$coefficient))) {
  stop(
    "All IRF coefficients are NA. Something is wrong with Step 5 estimation.",
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 5.9 Validation tables
# ------------------------------------------------------------------------------

irf_estimation_summary <- tibble::tibble(
  metric = c(
    "models_estimated",
    "assets_per_central_bank",
    "horizons_per_model",
    "expected_result_rows",
    "actual_result_rows",
    "ecb_sample_rows",
    "fed_sample_rows",
    "ecb_nonzero_mp_shocks",
    "fed_nonzero_mp_shocks",
    "ecb_non_announcement_days",
    "fed_non_announcement_days",
    "confidence_level",
    "significance_level",
    "lags_endog",
    "lags_exog",
    "trend",
    "newey_west_lag_rule",
    "manual_regressions_with_status_ok",
    "manual_regressions_not_ok",
    "significant_coefficients_5pct"
  ),
  value = c(
    as.character(6),
    as.character(length(CRYPTO_RETURN_VARS)),
    as.character(length(HORIZONS)),
    as.character(expected_rows),
    as.character(nrow(irf_results_long)),
    as.character(nrow(ecb_sample)),
    as.character(nrow(fed_sample)),
    as.character(sum(ecb_sample$ecb_mp != 0, na.rm = TRUE)),
    as.character(sum(fed_sample$fed_mp != 0, na.rm = TRUE)),
    as.character(sum(ecb_sample$ecb_meeting == 0, na.rm = TRUE)),
    as.character(sum(fed_sample$fed_meeting == 0, na.rm = TRUE)),
    as.character(CONF_LEVEL),
    as.character(SIG_LEVEL),
    as.character(LP_LAGS_ENDOG),
    as.character(LP_LAGS_EXOG),
    as.character(LP_TREND),
    as.character(NW_LAG_RULE),
    as.character(sum(irf_results_long$regression_status == "OK", na.rm = TRUE)),
    as.character(sum(irf_results_long$regression_status != "OK", na.rm = TRUE)),
    as.character(sum(irf_results_long$significant_5pct, na.rm = TRUE))
  )
)

irf_status_by_model <- irf_results_long %>%
  group_by(.data$central_bank, .data$asset, .data$asset_var, .data$shock_var) %>%
  summarise(
    horizons_estimated = n(),
    horizons_ok = sum(.data$regression_status == "OK", na.rm = TRUE),
    horizons_not_ok = sum(.data$regression_status != "OK", na.rm = TRUE),
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
    .groups = "drop"
  )

irf_observation_counts <- irf_results_long %>%
  select(
    central_bank,
    asset,
    asset_var,
    shock_var,
    horizon,
    n_obs,
    n_nonzero_shocks,
    nw_lag,
    regression_status
  ) %>%
  arrange(
    .data$central_bank,
    .data$asset,
    .data$horizon
  )

lpirfs_package_status <- irf_model_configs %>%
  select(
    central_bank,
    asset,
    asset_var,
    shock_var,
    controls,
    horizons,
    lags_endog,
    lags_exog,
    trend,
    lpirfs_status
  )

write_table_csv(
  irf_estimation_summary,
  "validation_05_irf_estimation_summary.csv",
  digits = TABLE_DIGITS
)

write_table_csv(
  irf_status_by_model,
  "validation_05_irf_status_by_model.csv",
  digits = TABLE_DIGITS
)

write_table_csv(
  irf_observation_counts,
  "validation_05_irf_observation_counts.csv",
  digits = TABLE_DIGITS
)

write_table_csv(
  lpirfs_package_status,
  "validation_05_lpirfs_package_status.csv",
  digits = TABLE_DIGITS
)


# ------------------------------------------------------------------------------
# 5.10 Save processed IRF results
# ------------------------------------------------------------------------------

irf_results_file <- file.path(PATHS$data_processed, "05_irf_results_long.csv")
irf_configs_file <- file.path(PATHS$data_processed, "05_irf_model_configs.csv")
irf_objects_file <- file.path(PATHS$data_processed, "05_irf_estimation_objects.rds")

readr::write_csv(
  irf_results_long,
  irf_results_file,
  na = ""
)

readr::write_csv(
  irf_model_configs,
  irf_configs_file,
  na = ""
)

saveRDS(
  full_irf_objects,
  irf_objects_file
)

message("Saved IRF results: ", irf_results_file)
message("Saved IRF model configs: ", irf_configs_file)
message("Saved full IRF estimation objects: ", irf_objects_file)


# ------------------------------------------------------------------------------
# 5.10B Additional check: USDT IRFs with 1st/99th percentile winsorised returns
# ------------------------------------------------------------------------------

# Purpose:
#   - Create a separate robustness/check table for Tether (USDT)
#   - Winsorise USDT daily log returns at the 1st and 99th percentile
#   - Re-estimate USDT IRFs for ECB and Fed monetary policy surprises
#   - Keep the baseline IRF results unchanged

USDT_WINSORISED_RETURN_VAR <- "usdt_log_return_winsor_1_99"
USDT_WINSORISED_5D_LAG_VAR <- "usdt_5d_lag_log_return_winsor_1_99"

winsorise_1_99 <- function(x) {
  bounds <- stats::quantile(
    x,
    probs = c(0.01, 0.99),
    na.rm = TRUE,
    type = 7,
    names = FALSE
  )
  
  pmin(
    pmax(x, bounds[1]),
    bounds[2]
  )
}

make_usdt_winsorised_sample <- function(data, central_bank_label) {
  data_ordered <- data %>%
    arrange(.data[[DATE_VAR]])
  
  winsor_bounds <- stats::quantile(
    data_ordered$usdt_log_return,
    probs = c(0.01, 0.99),
    na.rm = TRUE,
    type = 7,
    names = FALSE
  )
  
  data_ordered[[USDT_WINSORISED_RETURN_VAR]] <- winsorise_1_99(
    data_ordered$usdt_log_return
  )
  
  lagged_winsorised_usdt <- do.call(
    cbind,
    lapply(
      seq_len(CRYPTO_TREND_LAG),
      function(k) dplyr::lag(data_ordered[[USDT_WINSORISED_RETURN_VAR]], n = k)
    )
  )
  
  data_ordered[[USDT_WINSORISED_5D_LAG_VAR]] <- rowSums(
    lagged_winsorised_usdt,
    na.rm = FALSE
  )
  
  bounds_table <- tibble::tibble(
    central_bank = central_bank_label,
    original_variable = "usdt_log_return",
    winsorised_variable = USDT_WINSORISED_RETURN_VAR,
    lower_percentile = 0.01,
    upper_percentile = 0.99,
    lower_bound = winsor_bounds[1],
    upper_bound = winsor_bounds[2],
    n_rows = nrow(data_ordered),
    n_non_missing_original = sum(!is.na(data_ordered$usdt_log_return)),
    n_lower_clipped = sum(data_ordered$usdt_log_return < winsor_bounds[1], na.rm = TRUE),
    n_upper_clipped = sum(data_ordered$usdt_log_return > winsor_bounds[2], na.rm = TRUE)
  )
  
  list(
    sample = data_ordered,
    bounds = bounds_table
  )
}

replace_usdt_5d_control_with_winsorised <- function(control_vars) {
  out <- control_vars
  out[out == "usdt_5d_lag_log_return"] <- USDT_WINSORISED_5D_LAG_VAR
  unique(out)
}

message("Creating USDT winsorised samples...")

ecb_usdt_winsorised <- make_usdt_winsorised_sample(
  data = ecb_sample,
  central_bank_label = "ECB"
)

fed_usdt_winsorised <- make_usdt_winsorised_sample(
  data = fed_sample,
  central_bank_label = "Fed"
)

ecb_usdt_winsorised_sample <- ecb_usdt_winsorised$sample
fed_usdt_winsorised_sample <- fed_usdt_winsorised$sample

usdt_winsorisation_bounds <- bind_rows(
  ecb_usdt_winsorised$bounds,
  fed_usdt_winsorised$bounds
)

write_table_csv(
  usdt_winsorisation_bounds,
  "validation_05_usdt_winsorised_bounds.csv",
  digits = TABLE_DIGITS
)

ecb_usdt_winsorised_controls <- replace_usdt_5d_control_with_winsorised(
  ECB_BASELINE_CONTROL_VARS
)

fed_usdt_winsorised_controls <- replace_usdt_5d_control_with_winsorised(
  FED_BASELINE_CONTROL_VARS
)

message("Estimating ECB USDT winsorised IRF...")

ecb_usdt_winsorised_irf <- estimate_single_asset_irf(
  data = ecb_usdt_winsorised_sample,
  asset_var = USDT_WINSORISED_RETURN_VAR,
  shock_var = "ecb_mp",
  control_vars = ecb_usdt_winsorised_controls,
  central_bank = "ECB",
  horizons = HORIZONS,
  lags_endog = LP_LAGS_ENDOG,
  lags_exog = LP_LAGS_EXOG,
  trend = LP_TREND,
  nw_lag_rule = NW_LAG_RULE,
  nw_fixed_lag = NULL,
  conf_z = CONF_Z,
  sig_level = SIG_LEVEL,
  run_lpirfs = TRUE,
  lpirfs_rds_file = file.path(lpirfs_rds_dir, "lpirfs_ecb_usdt_winsor_1_99.rds")
)

message("Estimating Fed USDT winsorised IRF...")

fed_usdt_winsorised_irf <- estimate_single_asset_irf(
  data = fed_usdt_winsorised_sample,
  asset_var = USDT_WINSORISED_RETURN_VAR,
  shock_var = "fed_mp",
  control_vars = fed_usdt_winsorised_controls,
  central_bank = "Fed",
  horizons = HORIZONS,
  lags_endog = LP_LAGS_ENDOG,
  lags_exog = LP_LAGS_EXOG,
  trend = LP_TREND,
  nw_lag_rule = NW_LAG_RULE,
  nw_fixed_lag = NULL,
  conf_z = CONF_Z,
  sig_level = SIG_LEVEL,
  run_lpirfs = TRUE,
  lpirfs_rds_file = file.path(lpirfs_rds_dir, "lpirfs_fed_usdt_winsor_1_99.rds")
)

usdt_winsorised_irf_results_long <- bind_rows(
  ecb_usdt_winsorised_irf$tidy_results,
  fed_usdt_winsorised_irf$tidy_results
) %>%
  mutate(
    asset = "USDT",
    asset_variant = "USDT log return winsorised at 1st and 99th percentiles",
    original_asset_var = "usdt_log_return",
    winsorised_asset_var = USDT_WINSORISED_RETURN_VAR,
    winsorised_5d_lag_control = USDT_WINSORISED_5D_LAG_VAR
  ) %>%
  left_join(
    usdt_winsorisation_bounds %>%
      select(
        central_bank,
        winsor_lower_bound = lower_bound,
        winsor_upper_bound = upper_bound,
        winsor_n_lower_clipped = n_lower_clipped,
        winsor_n_upper_clipped = n_upper_clipped
      ),
    by = "central_bank"
  ) %>%
  arrange(
    .data$central_bank,
    .data$horizon
  )

usdt_winsorised_irf_model_configs <- bind_rows(
  ecb_usdt_winsorised_irf$config,
  fed_usdt_winsorised_irf$config
) %>%
  mutate(
    asset = "USDT",
    asset_variant = "USDT log return winsorised at 1st and 99th percentiles",
    original_asset_var = "usdt_log_return",
    winsorised_asset_var = USDT_WINSORISED_RETURN_VAR,
    winsorised_5d_lag_control = USDT_WINSORISED_5D_LAG_VAR
  ) %>%
  arrange(
    .data$central_bank
  )

usdt_winsorised_status <- usdt_winsorised_irf_results_long %>%
  group_by(.data$central_bank, .data$asset, .data$shock_var, .data$asset_var) %>%
  summarise(
    horizons_estimated = n(),
    horizons_ok = sum(.data$regression_status == "OK", na.rm = TRUE),
    horizons_not_ok = sum(.data$regression_status != "OK", na.rm = TRUE),
    min_n_obs = min(.data$n_obs, na.rm = TRUE),
    max_n_obs = max(.data$n_obs, na.rm = TRUE),
    nonzero_shocks_min = min(.data$n_nonzero_shocks, na.rm = TRUE),
    nonzero_shocks_max = max(.data$n_nonzero_shocks, na.rm = TRUE),
    significant_horizons_5pct = sum(.data$significant_5pct, na.rm = TRUE),
    winsor_lower_bound = dplyr::first(.data$winsor_lower_bound),
    winsor_upper_bound = dplyr::first(.data$winsor_upper_bound),
    winsor_n_lower_clipped = dplyr::first(.data$winsor_n_lower_clipped),
    winsor_n_upper_clipped = dplyr::first(.data$winsor_n_upper_clipped),
    .groups = "drop"
  )

write_table_csv(
  usdt_winsorised_status,
  "validation_05_usdt_winsorised_irf_status.csv",
  digits = TABLE_DIGITS
)

usdt_winsorised_irf_results_file <- file.path(
  PATHS$data_processed,
  "05_usdt_winsorised_irf_results_long.csv"
)

usdt_winsorised_irf_configs_file <- file.path(
  PATHS$data_processed,
  "05_usdt_winsorised_irf_model_configs.csv"
)

readr::write_csv(
  usdt_winsorised_irf_results_long,
  usdt_winsorised_irf_results_file,
  na = ""
)

readr::write_csv(
  usdt_winsorised_irf_model_configs,
  usdt_winsorised_irf_configs_file,
  na = ""
)

message("Saved USDT winsorised IRF results: ", usdt_winsorised_irf_results_file)
message("Saved USDT winsorised IRF configs: ", usdt_winsorised_irf_configs_file)


# ------------------------------------------------------------------------------
# 5.11 Console preview
# ------------------------------------------------------------------------------

message("IRF estimation preview:")

print(
  irf_results_long %>%
    select(
      central_bank,
      asset,
      horizon,
      coefficient,
      std_error,
      p_value,
      conf_low,
      conf_high,
      significant_5pct,
      n_obs,
      regression_status
    ) %>%
    filter(.data$horizon %in% c(0, 1, 5, 10, 25)) %>%
    arrange(.data$central_bank, .data$asset, .data$horizon)
)


# ------------------------------------------------------------------------------
# 5.12 Final message
# ------------------------------------------------------------------------------

message("Step 5 complete: six baseline IRFs estimated and saved.")