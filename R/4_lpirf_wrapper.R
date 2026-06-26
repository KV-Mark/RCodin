# ==============================================================================
# 4_lpirf_wrapper.R
# Thesis project: The Effect of Monetary Policy Announcements on Cryptocurrency Returns
#
# Purpose:
#   - Define reusable wrapper functions for local projection IRFs
#   - Estimate horizon-by-horizon LP regressions manually for thesis-ready output
#   - Run lpirfs::lp_lin_iv() as the package-based LPIRF object
#   - Return tidy IRF tables with coefficients, HAC/Newey-West SEs, p-values,
#     95% confidence intervals, and 5% significance indicators
#
# Inputs:
#   This file defines functions only.
#   Step 5 will use:
#     data/processed/03_model_sample_ecb.csv
#     data/processed/03_model_sample_fed.csv
#
# Outputs:
#   output/tables/validation_04_lpirf_wrapper_specification.csv
# ==============================================================================


# ------------------------------------------------------------------------------
# 4.0 Load setup
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
# 4.1 General helpers
# ------------------------------------------------------------------------------

validate_lp_inputs <- function(
    data,
    asset_var,
    shock_var,
    control_vars,
    central_bank,
    horizons
) {
  required_vars <- unique(c(DATE_VAR, asset_var, shock_var, control_vars))
  
  check_required_columns(
    data,
    required_vars,
    data_name = paste0("LP input data for ", central_bank, " - ", asset_var)
  )
  
  if (!is.numeric(data[[asset_var]])) {
    stop(asset_var, " must be numeric.", call. = FALSE)
  }
  
  if (!is.numeric(data[[shock_var]])) {
    stop(shock_var, " must be numeric.", call. = FALSE)
  }
  
  for (v in control_vars) {
    if (!is.numeric(data[[v]])) {
      stop(v, " must be numeric.", call. = FALSE)
    }
  }
  
  if (any(is.na(data[[shock_var]]))) {
    stop(
      shock_var,
      " contains NA values. Step 2 should have converted non-announcement shock NAs to zero.",
      call. = FALSE
    )
  }
  
  if (length(horizons) == 0) {
    stop("At least one horizon is required.", call. = FALSE)
  }
  
  if (any(horizons < 0)) {
    stop("Horizons must be non-negative.", call. = FALSE)
  }
  
  invisible(TRUE)
}


generate_lag_names <- function(variables, lags) {
  if (length(variables) == 0 || lags <= 0) {
    return(character(0))
  }
  
  unlist(
    purrr::map(
      variables,
      function(v) paste0(v, "_lag", seq_len(lags))
    )
  )
}


add_lags <- function(data, variables, lags) {
  if (length(variables) == 0 || lags <= 0) {
    return(data)
  }
  
  out <- data
  
  for (v in variables) {
    for (lag_i in seq_len(lags)) {
      lag_name <- paste0(v, "_lag", lag_i)
      out[[lag_name]] <- dplyr::lag(out[[v]], n = lag_i)
    }
  }
  
  out
}


add_horizon_response <- function(data, asset_var, horizon) {
  out <- data
  response_var <- paste0(asset_var, "_h", horizon)
  
  if (horizon == 0) {
    out[[response_var]] <- out[[asset_var]]
  } else {
    out[[response_var]] <- dplyr::lead(out[[asset_var]], n = horizon)
  }
  
  out
}


add_trend_terms <- function(data, trend) {
  out <- data
  
  if (trend >= 1) {
    out$trend_index <- seq_len(nrow(out))
  }
  
  if (trend >= 2) {
    out$trend_index_sq <- out$trend_index^2
  }
  
  out
}


get_trend_terms <- function(trend) {
  if (trend <= 0) {
    return(character(0))
  }
  
  if (trend == 1) {
    return("trend_index")
  }
  
  c("trend_index", "trend_index_sq")
}


get_nw_lag <- function(horizon, n_obs, nw_lag_rule = NW_LAG_RULE, fixed_lag = NULL) {
  if (!is.null(fixed_lag)) {
    lag_value <- fixed_lag
  } else if (nw_lag_rule == "horizon") {
    lag_value <- horizon
  } else if (nw_lag_rule == "zero") {
    lag_value <- 0
  } else {
    stop("Unknown NW lag rule: ", nw_lag_rule, call. = FALSE)
  }
  
  lag_value <- as.integer(max(0, lag_value))
  
  # Avoid impossible lag values in very small horizon samples.
  lag_value <- min(lag_value, max(0, n_obs - 1))
  
  lag_value
}


get_model_f_pvalue <- function(model) {
  fstat <- summary(model)$fstatistic
  
  if (is.null(fstat)) {
    return(NA_real_)
  }
  
  stats::pf(
    q = unname(fstat[1]),
    df1 = unname(fstat[2]),
    df2 = unname(fstat[3]),
    lower.tail = FALSE
  )
}


make_significance_5pct <- function(p_value) {
  dplyr::case_when(
    is.na(p_value) ~ "",
    p_value < SIG_LEVEL ~ "*",
    TRUE ~ ""
  )
}


# ------------------------------------------------------------------------------
# 4.2 Manual local projection estimator
# ------------------------------------------------------------------------------

estimate_lp_horizon_manual <- function(
    data,
    asset_var,
    shock_var,
    control_vars,
    central_bank,
    horizon,
    lags_endog = LP_LAGS_ENDOG,
    lags_exog = LP_LAGS_EXOG,
    trend = LP_TREND,
    nw_lag_rule = NW_LAG_RULE,
    nw_fixed_lag = NULL,
    conf_z = CONF_Z,
    sig_level = SIG_LEVEL
) {
  validate_lp_inputs(
    data = data,
    asset_var = asset_var,
    shock_var = shock_var,
    control_vars = control_vars,
    central_bank = central_bank,
    horizons = horizon
  )
  
  data_ordered <- data %>%
    arrange(.data[[DATE_VAR]])
  
  response_var <- paste0(asset_var, "_h", horizon)
  
  # Do not create an additional 1-day lag of controls that are already
  # constructed as lagged multi-day controls.
  controls_to_lag <- setdiff(control_vars, CRYPTO_5D_LAG_RETURN_VARS)
  
  design_data <- data_ordered %>%
    add_horizon_response(asset_var = asset_var, horizon = horizon) %>%
    add_lags(variables = asset_var, lags = lags_endog) %>%
    add_lags(variables = controls_to_lag, lags = lags_exog) %>%
    add_trend_terms(trend = trend)
  
  lagged_endog_vars <- generate_lag_names(asset_var, lags_endog)
  lagged_control_vars <- generate_lag_names(controls_to_lag, lags_exog)
  trend_vars <- get_trend_terms(trend)
  
  regressors <- unique(c(
    shock_var,
    control_vars,
    lagged_endog_vars,
    lagged_control_vars,
    trend_vars
  ))
  
  model_vars <- unique(c(DATE_VAR, response_var, regressors))
  
  model_data <- design_data %>%
    select(all_of(model_vars))
  
  complete_rows <- stats::complete.cases(model_data[, c(response_var, regressors)])
  model_data <- model_data[complete_rows, , drop = FALSE]
  
  n_obs <- nrow(model_data)
  n_regressors <- length(regressors)
  
  if (n_obs <= n_regressors + 2) {
    return(
      tibble::tibble(
        central_bank = central_bank,
        asset = label_asset(asset_var),
        asset_var = asset_var,
        shock_var = shock_var,
        horizon = horizon,
        coefficient = NA_real_,
        std_error = NA_real_,
        t_statistic = NA_real_,
        p_value = NA_real_,
        conf_low = NA_real_,
        conf_high = NA_real_,
        significant_5pct = FALSE,
        significance_5pct = "",
        n_obs = n_obs,
        n_nonzero_shocks = sum(model_data[[shock_var]] != 0, na.rm = TRUE),
        r_squared = NA_real_,
        adj_r_squared = NA_real_,
        f_p_value = NA_real_,
        nw_lag = NA_integer_,
        regression_status = "Insufficient observations",
        equation_lhs = response_var,
        equation_rhs = paste(regressors, collapse = " + ")
      )
    )
  }
  
  if (dplyr::n_distinct(model_data[[shock_var]]) < 2) {
    return(
      tibble::tibble(
        central_bank = central_bank,
        asset = label_asset(asset_var),
        asset_var = asset_var,
        shock_var = shock_var,
        horizon = horizon,
        coefficient = NA_real_,
        std_error = NA_real_,
        t_statistic = NA_real_,
        p_value = NA_real_,
        conf_low = NA_real_,
        conf_high = NA_real_,
        significant_5pct = FALSE,
        significance_5pct = "",
        n_obs = n_obs,
        n_nonzero_shocks = sum(model_data[[shock_var]] != 0, na.rm = TRUE),
        r_squared = NA_real_,
        adj_r_squared = NA_real_,
        f_p_value = NA_real_,
        nw_lag = NA_integer_,
        regression_status = "No shock variation",
        equation_lhs = response_var,
        equation_rhs = paste(regressors, collapse = " + ")
      )
    )
  }
  
  lp_formula <- stats::reformulate(
    termlabels = regressors,
    response = response_var
  )
  
  model <- stats::lm(lp_formula, data = model_data)
  
  nw_lag <- get_nw_lag(
    horizon = horizon,
    n_obs = n_obs,
    nw_lag_rule = nw_lag_rule,
    fixed_lag = nw_fixed_lag
  )
  
  hac_vcov <- tryCatch(
    {
      sandwich::NeweyWest(
        model,
        lag = nw_lag,
        prewhite = FALSE,
        adjust = TRUE
      )
    },
    error = function(e) {
      warning(
        "NeweyWest failed for ",
        central_bank,
        " - ",
        asset_var,
        " at h = ",
        horizon,
        ". Falling back to vcovHC(type = 'HC1'). Error: ",
        e$message,
        call. = FALSE
      )
      
      sandwich::vcovHC(model, type = "HC1")
    }
  )
  
  coef_test <- tryCatch(
    {
      lmtest::coeftest(model, vcov. = hac_vcov)
    },
    error = function(e) {
      warning(
        "coeftest failed for ",
        central_bank,
        " - ",
        asset_var,
        " at h = ",
        horizon,
        ". Error: ",
        e$message,
        call. = FALSE
      )
      
      NULL
    }
  )
  
  if (is.null(coef_test) || !(shock_var %in% rownames(coef_test))) {
    return(
      tibble::tibble(
        central_bank = central_bank,
        asset = label_asset(asset_var),
        asset_var = asset_var,
        shock_var = shock_var,
        horizon = horizon,
        coefficient = NA_real_,
        std_error = NA_real_,
        t_statistic = NA_real_,
        p_value = NA_real_,
        conf_low = NA_real_,
        conf_high = NA_real_,
        significant_5pct = FALSE,
        significance_5pct = "",
        n_obs = n_obs,
        n_nonzero_shocks = sum(model_data[[shock_var]] != 0, na.rm = TRUE),
        r_squared = summary(model)$r.squared,
        adj_r_squared = summary(model)$adj.r.squared,
        f_p_value = get_model_f_pvalue(model),
        nw_lag = nw_lag,
        regression_status = "Shock coefficient not estimated",
        equation_lhs = response_var,
        equation_rhs = paste(regressors, collapse = " + ")
      )
    )
  }
  
  coefficient <- unname(coef_test[shock_var, "Estimate"])
  std_error <- unname(coef_test[shock_var, "Std. Error"])
  t_statistic <- unname(coef_test[shock_var, "t value"])
  
  # Use the finite-sample t distribution with residual degrees of freedom.
  p_value <- ifelse(
    is.na(t_statistic),
    NA_real_,
    2 * stats::pt(abs(t_statistic), df = stats::df.residual(model), lower.tail = FALSE)
  )
  
  conf_low <- coefficient - conf_z * std_error
  conf_high <- coefficient + conf_z * std_error
  
  tibble::tibble(
    central_bank = central_bank,
    asset = label_asset(asset_var),
    asset_var = asset_var,
    shock_var = shock_var,
    horizon = horizon,
    coefficient = coefficient,
    std_error = std_error,
    t_statistic = t_statistic,
    p_value = p_value,
    conf_low = conf_low,
    conf_high = conf_high,
    significant_5pct = !is.na(p_value) & p_value < sig_level,
    significance_5pct = make_significance_5pct(p_value),
    n_obs = n_obs,
    n_nonzero_shocks = sum(model_data[[shock_var]] != 0, na.rm = TRUE),
    r_squared = summary(model)$r.squared,
    adj_r_squared = summary(model)$adj.r.squared,
    f_p_value = get_model_f_pvalue(model),
    nw_lag = nw_lag,
    regression_status = "OK",
    equation_lhs = response_var,
    equation_rhs = paste(regressors, collapse = " + ")
  )
}


estimate_lp_irf_manual <- function(
    data,
    asset_var,
    shock_var,
    control_vars,
    central_bank,
    horizons = HORIZONS,
    lags_endog = LP_LAGS_ENDOG,
    lags_exog = LP_LAGS_EXOG,
    trend = LP_TREND,
    nw_lag_rule = NW_LAG_RULE,
    nw_fixed_lag = NULL,
    conf_z = CONF_Z,
    sig_level = SIG_LEVEL
) {
  validate_lp_inputs(
    data = data,
    asset_var = asset_var,
    shock_var = shock_var,
    control_vars = control_vars,
    central_bank = central_bank,
    horizons = horizons
  )
  
  purrr::map_dfr(
    horizons,
    function(h) {
      estimate_lp_horizon_manual(
        data = data,
        asset_var = asset_var,
        shock_var = shock_var,
        control_vars = control_vars,
        central_bank = central_bank,
        horizon = h,
        lags_endog = lags_endog,
        lags_exog = lags_exog,
        trend = trend,
        nw_lag_rule = nw_lag_rule,
        nw_fixed_lag = nw_fixed_lag,
        conf_z = conf_z,
        sig_level = sig_level
      )
    }
  )
}


# ------------------------------------------------------------------------------
# 4.3 lpirfs package wrapper
# ------------------------------------------------------------------------------

run_lpirfs_package_model <- function(
    data,
    asset_var,
    shock_var,
    control_vars,
    central_bank,
    horizons = HORIZONS,
    lags_endog = LP_LAGS_ENDOG,
    lags_exog = LP_LAGS_EXOG,
    trend = LP_TREND,
    conf_z = CONF_Z,
    save_rds_file = NULL
) {
  validate_lp_inputs(
    data = data,
    asset_var = asset_var,
    shock_var = shock_var,
    control_vars = control_vars,
    central_bank = central_bank,
    horizons = horizons
  )
  
  model_vars <- unique(c(asset_var, shock_var, control_vars))
  
  lpirfs_data <- data %>%
    arrange(.data[[DATE_VAR]]) %>%
    select(all_of(model_vars))
  
  lpirfs_data <- lpirfs_data[
    stats::complete.cases(lpirfs_data[, model_vars]),
    ,
    drop = FALSE
  ]
  
  endog_data <- lpirfs_data %>%
    select(all_of(asset_var)) %>%
    as.data.frame()
  
  shock_data <- lpirfs_data %>%
    select(all_of(shock_var)) %>%
    as.data.frame()
  
  exog_data <- lpirfs_data %>%
    select(all_of(control_vars)) %>%
    as.data.frame()
  
  # lpirfs convention: hor is the number of horizons. Since HORIZONS = 0:25,
  # length(HORIZONS) = 26, corresponding to h = 0, ..., 25.
  lpirfs_args <- list(
    endog_data = endog_data,
    lags_endog_lin = lags_endog,
    shock = shock_data,
    use_twosls = FALSE,
    trend = trend,
    confint = conf_z,
    hor = length(horizons),
    use_nw = TRUE,
    exog_data = exog_data,
    lags_exog = lags_exog
  )
  
  lpirfs_result <- tryCatch(
    {
      do.call(lpirfs::lp_lin_iv, lpirfs_args)
    },
    error = function(e) {
      structure(
        list(
          error_message = e$message,
          central_bank = central_bank,
          asset_var = asset_var,
          shock_var = shock_var
        ),
        class = "lpirfs_error"
      )
    }
  )
  
  status <- if (inherits(lpirfs_result, "lpirfs_error")) {
    paste0("lpirfs_error: ", lpirfs_result$error_message)
  } else {
    "OK"
  }
  
  if (!is.null(save_rds_file) && !inherits(lpirfs_result, "lpirfs_error")) {
    saveRDS(lpirfs_result, save_rds_file)
    message("Saved lpirfs object: ", save_rds_file)
  }
  
  list(
    object = lpirfs_result,
    status = status,
    n_input_rows = nrow(lpirfs_data),
    asset_var = asset_var,
    shock_var = shock_var,
    central_bank = central_bank
  )
}


# ------------------------------------------------------------------------------
# 4.4 Main wrapper used by Step 5
# ------------------------------------------------------------------------------

estimate_single_asset_irf <- function(
    data,
    asset_var,
    shock_var,
    control_vars,
    central_bank,
    horizons = HORIZONS,
    lags_endog = LP_LAGS_ENDOG,
    lags_exog = LP_LAGS_EXOG,
    trend = LP_TREND,
    nw_lag_rule = NW_LAG_RULE,
    nw_fixed_lag = NULL,
    conf_z = CONF_Z,
    sig_level = SIG_LEVEL,
    run_lpirfs = TRUE,
    lpirfs_rds_file = NULL
) {
  manual_results <- estimate_lp_irf_manual(
    data = data,
    asset_var = asset_var,
    shock_var = shock_var,
    control_vars = control_vars,
    central_bank = central_bank,
    horizons = horizons,
    lags_endog = lags_endog,
    lags_exog = lags_exog,
    trend = trend,
    nw_lag_rule = nw_lag_rule,
    nw_fixed_lag = nw_fixed_lag,
    conf_z = conf_z,
    sig_level = sig_level
  )
  
  lpirfs_package_result <- NULL
  
  if (run_lpirfs) {
    lpirfs_package_result <- run_lpirfs_package_model(
      data = data,
      asset_var = asset_var,
      shock_var = shock_var,
      control_vars = control_vars,
      central_bank = central_bank,
      horizons = horizons,
      lags_endog = lags_endog,
      lags_exog = lags_exog,
      trend = trend,
      conf_z = conf_z,
      save_rds_file = lpirfs_rds_file
    )
  }
  
  config <- tibble::tibble(
    central_bank = central_bank,
    asset = label_asset(asset_var),
    asset_var = asset_var,
    shock_var = shock_var,
    controls = paste(control_vars, collapse = ", "),
    horizons = paste(range(horizons), collapse = " to "),
    lags_endog = lags_endog,
    lags_exog = lags_exog,
    trend = trend,
    nw_lag_rule = nw_lag_rule,
    nw_fixed_lag = ifelse(is.null(nw_fixed_lag), NA_integer_, nw_fixed_lag),
    confidence_level = CONF_LEVEL,
    significance_level = sig_level,
    lpirfs_status = ifelse(
      is.null(lpirfs_package_result),
      "Not run",
      lpirfs_package_result$status
    )
  )
  
  list(
    tidy_results = manual_results,
    lpirfs_package = lpirfs_package_result,
    config = config
  )
}


# ------------------------------------------------------------------------------
# 4.5 Convenience function for multiple assets
# ------------------------------------------------------------------------------

estimate_multiple_asset_irfs <- function(
    data,
    asset_vars,
    shock_var,
    control_vars,
    central_bank,
    horizons = HORIZONS,
    lags_endog = LP_LAGS_ENDOG,
    lags_exog = LP_LAGS_EXOG,
    trend = LP_TREND,
    nw_lag_rule = NW_LAG_RULE,
    nw_fixed_lag = NULL,
    conf_z = CONF_Z,
    sig_level = SIG_LEVEL,
    run_lpirfs = TRUE,
    save_lpirfs_objects = FALSE,
    rds_directory = NULL
) {
  if (save_lpirfs_objects && is.null(rds_directory)) {
    rds_directory <- file.path(PATHS$data_processed, "lpirfs_objects")
  }
  
  if (save_lpirfs_objects) {
    create_dir_if_missing(rds_directory)
  }
  
  results_list <- purrr::map(
    asset_vars,
    function(asset_i) {
      rds_file <- NULL
      
      if (save_lpirfs_objects) {
        rds_file <- file.path(
          rds_directory,
          paste0(
            "lpirfs_",
            tolower(central_bank),
            "_",
            gsub("_log_return", "", asset_i),
            ".rds"
          )
        )
      }
      
      estimate_single_asset_irf(
        data = data,
        asset_var = asset_i,
        shock_var = shock_var,
        control_vars = control_vars,
        central_bank = central_bank,
        horizons = horizons,
        lags_endog = lags_endog,
        lags_exog = lags_exog,
        trend = trend,
        nw_lag_rule = nw_lag_rule,
        nw_fixed_lag = nw_fixed_lag,
        conf_z = conf_z,
        sig_level = sig_level,
        run_lpirfs = run_lpirfs,
        lpirfs_rds_file = rds_file
      )
    }
  )
  
  tidy_results <- purrr::map_dfr(results_list, "tidy_results")
  configs <- purrr::map_dfr(results_list, "config")
  
  list(
    tidy_results = tidy_results,
    configs = configs,
    full_results = results_list
  )
}


# ------------------------------------------------------------------------------
# 4.6 Wrapper specification table
# ------------------------------------------------------------------------------

wrapper_specification <- tibble::tibble(
  setting = c(
    "manual_lp_equation",
    "package_function",
    "response_type",
    "horizons",
    "confidence_interval",
    "significance_level",
    "hac_standard_errors",
    "nw_lag_rule",
    "lags_endog",
    "lags_exog",
    "trend",
    "non_announcement_shock_handling",
    "primary_table_source",
    "package_object_source"
  ),
  value = c(
    "y_{t+h} = alpha_h + beta_h shock_t + phi_h controls_t + lag terms + error_{t+h}",
    "lpirfs::lp_lin_iv()",
    "Daily log return at horizon h, not cumulative return",
    paste0(min(HORIZONS), " to ", max(HORIZONS)),
    paste0(CONF_LEVEL * 100, "%"),
    paste0(SIG_LEVEL * 100, "%"),
    "Newey-West / HAC",
    NW_LAG_RULE,
    as.character(LP_LAGS_ENDOG),
    as.character(LP_LAGS_EXOG),
    as.character(LP_TREND),
    "Step 2 preserves observed shocks and sets non-announcement shock NA values to zero",
    "Manual LP wrapper output, because it gives thesis-ready coefficients, SEs, p-values, and CIs",
    "lpirfs::lp_lin_iv() object, saved optionally for package-consistency checks"
  )
)

write_table_csv(
  wrapper_specification,
  "validation_04_lpirf_wrapper_specification.csv",
  digits = TABLE_DIGITS
)


# ------------------------------------------------------------------------------
# 4.7 Final message
# ------------------------------------------------------------------------------

message("Step 4 complete: LPIRF wrapper functions loaded.")