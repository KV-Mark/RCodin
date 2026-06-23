# ============================================================
# Step 7: Robustness checks
# Purpose:
# 1. Test alternative Newey-West bandwidth choices
# 2. Test alternative lag lengths
# 3. Test reduced-control specifications
# 4. Save robustness tables and plots
# ============================================================

source(file.path(path.expand("~"), "Documents", "Timi", "RThesis", "RCodin", "R", "0_setup.R"))
# source(file.path(path.expand("~"), "Downloads", "RThesis", "R", "0_setup.R"))

# ------------------------------------------------------------
# 7.1 Load data
# ------------------------------------------------------------

us_dt <- readRDS(file.path(PROCESSED_DIR, "us_trading_step2.rds"))
eu_dt <- readRDS(file.path(PROCESSED_DIR, "eu_trading_step2.rds"))

main_mp_irfs <- readRDS(file.path(PROCESSED_DIR, "main_mp_irfs_step5.rds"))
all_irfs <- readRDS(file.path(PROCESSED_DIR, "all_baseline_irfs_step4.rds"))

# Basic check
nrow(main_mp_irfs)
main_mp_irfs %>% count(asset, central_bank)


# ------------------------------------------------------------
# 7.2 Helper functions
# ------------------------------------------------------------

add_lags <- function(data, vars, lags = 1) {
  for (v in vars) {
    for (l in lags) {
      new_name <- paste0(v, "_l", l)
      data[[new_name]] <- dplyr::lag(data[[v]], l)
    }
  }
  
  data
}

add_cumulative_return_horizons <- function(data, log_price_var, y_var, max_h = 25) {
  for (h in 0:max_h) {
    new_name <- paste0(y_var, "_cum_h", h)
    
    data[[new_name]] <- 100 * (
      dplyr::lead(data[[log_price_var]], h) -
        dplyr::lag(data[[log_price_var]], 1)
    )
  }
  
  data
}

get_nw_lag <- function(h, bandwidth_type) {
  case_when(
    bandwidth_type == "h_plus_1" ~ h + 1,
    bandwidth_type == "fixed_5" ~ 5,
    bandwidth_type == "fixed_10" ~ 10,
    TRUE ~ h + 1
  )
}

estimate_lp_irf_robust <- function(
    data,
    y_var,
    shock_var,
    controls,
    max_h = 25,
    bandwidth_type = "h_plus_1"
) {
  
  results <- purrr::map_dfr(0:max_h, function(h) {
    
    dep_var <- paste0(y_var, "_cum_h", h)
    rhs_vars <- c(shock_var, controls)
    
    formula_text <- paste(
      dep_var,
      "~",
      paste(rhs_vars, collapse = " + ")
    )
    
    model <- lm(as.formula(formula_text), data = data)
    
    nw_lag <- get_nw_lag(h, bandwidth_type)
    
    nw_vcov <- sandwich::NeweyWest(
      model,
      lag = nw_lag,
      prewhite = FALSE,
      adjust = TRUE
    )
    
    coef_table <- lmtest::coeftest(model, vcov. = nw_vcov)
    
    tibble(
      horizon = h,
      term = rownames(coef_table),
      estimate = coef_table[, 1],
      std_error = coef_table[, 2],
      statistic = coef_table[, 3],
      p_value = coef_table[, 4],
      n_obs = nobs(model),
      nw_lag = nw_lag,
      r_squared = summary(model)$r.squared
    ) %>%
      filter(term == shock_var)
  })
  
  results %>%
    mutate(
      ci_95_low = estimate - 1.96 * std_error,
      ci_95_high = estimate + 1.96 * std_error
    )
}


# ------------------------------------------------------------
# 7.3 Prepare LP datasets
# ------------------------------------------------------------

us_lag_vars_1lag <- c(
  "btc_ret",
  "eth_ret",
  "usdt_ret",
  "sp500_ret",
  "vix_log",
  "dxy_ret",
  "us_short_spread",
  "us_long_spread"
)

eu_lag_vars_1lag <- c(
  "btc_ret",
  "eth_ret",
  "usdt_ret",
  "stoxx50_ret",
  "vix_log",
  "dxy_ret",
  "de_short_spread",
  "de_long_spread"
)

us_lag_vars_2lag <- us_lag_vars_1lag
eu_lag_vars_2lag <- eu_lag_vars_1lag

us_dt_lp_1lag <- us_dt %>%
  add_lags(vars = us_lag_vars_1lag, lags = 1) %>%
  add_cumulative_return_horizons("btc_log_price", "btc_ret", 25) %>%
  add_cumulative_return_horizons("eth_log_price", "eth_ret", 25) %>%
  add_cumulative_return_horizons("usdt_log_price", "usdt_ret", 25)

eu_dt_lp_1lag <- eu_dt %>%
  add_lags(vars = eu_lag_vars_1lag, lags = 1) %>%
  add_cumulative_return_horizons("btc_log_price", "btc_ret", 25) %>%
  add_cumulative_return_horizons("eth_log_price", "eth_ret", 25) %>%
  add_cumulative_return_horizons("usdt_log_price", "usdt_ret", 25)

us_dt_lp_2lag <- us_dt %>%
  add_lags(vars = us_lag_vars_2lag, lags = 1:2) %>%
  add_cumulative_return_horizons("btc_log_price", "btc_ret", 25) %>%
  add_cumulative_return_horizons("eth_log_price", "eth_ret", 25) %>%
  add_cumulative_return_horizons("usdt_log_price", "usdt_ret", 25)

eu_dt_lp_2lag <- eu_dt %>%
  add_lags(vars = eu_lag_vars_2lag, lags = 1:2) %>%
  add_cumulative_return_horizons("btc_log_price", "btc_ret", 25) %>%
  add_cumulative_return_horizons("eth_log_price", "eth_ret", 25) %>%
  add_cumulative_return_horizons("usdt_log_price", "usdt_ret", 25)


# ------------------------------------------------------------
# 7.4 Control sets
# ------------------------------------------------------------

make_us_controls_baseline <- function(asset_ret) {
  c(
    paste0(asset_ret, "_l1"),
    "sp500_ret_l1",
    "vix_log_l1",
    "dxy_ret_l1",
    "us_short_spread_l1",
    "us_long_spread_l1",
    "covid_dummy",
    "mica_dummy",
    "hack_dummy",
    "monday_dummy"
  )
}

make_eu_controls_baseline <- function(asset_ret) {
  c(
    paste0(asset_ret, "_l1"),
    "stoxx50_ret_l1",
    "vix_log_l1",
    "dxy_ret_l1",
    "de_short_spread_l1",
    "de_long_spread_l1",
    "covid_dummy",
    "mica_dummy",
    "hack_dummy",
    "monday_dummy"
  )
}

make_us_controls_2lag <- function(asset_ret) {
  c(
    paste0(asset_ret, "_l1"),
    paste0(asset_ret, "_l2"),
    "sp500_ret_l1",
    "sp500_ret_l2",
    "vix_log_l1",
    "vix_log_l2",
    "dxy_ret_l1",
    "dxy_ret_l2",
    "us_short_spread_l1",
    "us_short_spread_l2",
    "us_long_spread_l1",
    "us_long_spread_l2",
    "covid_dummy",
    "mica_dummy",
    "hack_dummy",
    "monday_dummy"
  )
}

make_eu_controls_2lag <- function(asset_ret) {
  c(
    paste0(asset_ret, "_l1"),
    paste0(asset_ret, "_l2"),
    "stoxx50_ret_l1",
    "stoxx50_ret_l2",
    "vix_log_l1",
    "vix_log_l2",
    "dxy_ret_l1",
    "dxy_ret_l2",
    "de_short_spread_l1",
    "de_short_spread_l2",
    "de_long_spread_l1",
    "de_long_spread_l2",
    "covid_dummy",
    "mica_dummy",
    "hack_dummy",
    "monday_dummy"
  )
}

make_us_controls_no_dummies <- function(asset_ret) {
  c(
    paste0(asset_ret, "_l1"),
    "sp500_ret_l1",
    "vix_log_l1",
    "dxy_ret_l1",
    "us_short_spread_l1",
    "us_long_spread_l1"
  )
}

make_eu_controls_no_dummies <- function(asset_ret) {
  c(
    paste0(asset_ret, "_l1"),
    "stoxx50_ret_l1",
    "vix_log_l1",
    "dxy_ret_l1",
    "de_short_spread_l1",
    "de_long_spread_l1"
  )
}


# ------------------------------------------------------------
# 7.5 Model specifications
# ------------------------------------------------------------

asset_specs <- tibble(
  asset = c("BTC", "ETH", "USDT"),
  y_var = c("btc_ret", "eth_ret", "usdt_ret")
)

robust_model_specs <- asset_specs %>%
  crossing(
    central_bank = c("Fed", "ECB")
  ) %>%
  mutate(
    shock_type = "MP",
    shock_var = case_when(
      central_bank == "Fed" ~ "fed_mp_std",
      central_bank == "ECB" ~ "ecb_mp_std"
    ),
    calendar = case_when(
      central_bank == "Fed" ~ "US",
      central_bank == "ECB" ~ "EU"
    )
  ) %>%
  arrange(asset, central_bank)

robust_model_specs


# ------------------------------------------------------------
# 7.6 Function to run robustness specification
# ------------------------------------------------------------

run_one_robust_irf <- function(
    asset,
    y_var,
    central_bank,
    shock_type,
    shock_var,
    calendar,
    specification,
    bandwidth_type
) {
  
  if (specification == "baseline") {
    
    if (calendar == "US") {
      data_used <- us_dt_lp_1lag
      controls_used <- make_us_controls_baseline(y_var)
    } else {
      data_used <- eu_dt_lp_1lag
      controls_used <- make_eu_controls_baseline(y_var)
    }
    
  } else if (specification == "two_lags") {
    
    if (calendar == "US") {
      data_used <- us_dt_lp_2lag
      controls_used <- make_us_controls_2lag(y_var)
    } else {
      data_used <- eu_dt_lp_2lag
      controls_used <- make_eu_controls_2lag(y_var)
    }
    
  } else if (specification == "no_dummies") {
    
    if (calendar == "US") {
      data_used <- us_dt_lp_1lag
      controls_used <- make_us_controls_no_dummies(y_var)
    } else {
      data_used <- eu_dt_lp_1lag
      controls_used <- make_eu_controls_no_dummies(y_var)
    }
    
  } else {
    stop("Unknown robustness specification: ", specification)
  }
  
  estimate_lp_irf_robust(
    data = data_used,
    y_var = y_var,
    shock_var = shock_var,
    controls = controls_used,
    max_h = 25,
    bandwidth_type = bandwidth_type
  ) %>%
    mutate(
      asset = asset,
      y_var = y_var,
      central_bank = central_bank,
      shock_type = shock_type,
      shock_var = shock_var,
      calendar = calendar,
      specification = specification,
      bandwidth_type = bandwidth_type,
      .before = horizon
    )
}

# ------------------------------------------------------------
# 7.7 Run all robustness checks
# ------------------------------------------------------------

robustness_grid <- robust_model_specs %>%
  crossing(
    specification = c("baseline", "two_lags", "no_dummies"),
    bandwidth_type = c("h_plus_1", "fixed_5", "fixed_10")
  )

robustness_grid

robust_irfs <- pmap_dfr(
  robustness_grid,
  run_one_robust_irf
)

robust_irfs

# Expected:
# 6 main MP models × 3 specifications × 3 bandwidths × 26 horizons
nrow(robust_irfs)

robust_irfs %>%
  count(asset, central_bank, specification, bandwidth_type)



# ------------------------------------------------------------
# 7.8 Save robustness results
# ------------------------------------------------------------

write_csv(
  robust_irfs,
  file.path(TABLE_DIR, "robust_irfs_step7.csv")
)

saveRDS(
  robust_irfs,
  file.path(PROCESSED_DIR, "robust_irfs_step7.rds")
)


# ------------------------------------------------------------
# 7.9 Baseline reproduction check
# ------------------------------------------------------------

robust_baseline <- robust_irfs %>%
  filter(
    specification == "baseline",
    bandwidth_type == "h_plus_1"
  ) %>%
  select(
    asset,
    central_bank,
    shock_type,
    horizon,
    estimate_robust = estimate,
    std_error_robust = std_error
  )

baseline_comparison <- main_mp_irfs %>%
  select(
    asset,
    central_bank,
    shock_type,
    horizon,
    estimate_main = estimate,
    std_error_main = std_error
  ) %>%
  left_join(
    robust_baseline,
    by = c("asset", "central_bank", "shock_type", "horizon")
  ) %>%
  mutate(
    estimate_difference = estimate_main - estimate_robust,
    std_error_difference = std_error_main - std_error_robust
  )

baseline_comparison_summary <- baseline_comparison %>%
  summarise(
    max_abs_estimate_difference = max(abs(estimate_difference), na.rm = TRUE),
    max_abs_std_error_difference = max(abs(std_error_difference), na.rm = TRUE)
  )

baseline_comparison_summary

write_csv(
  baseline_comparison,
  file.path(TABLE_DIR, "baseline_reproduction_comparison_step7.csv")
)

write_csv(
  baseline_comparison_summary,
  file.path(TABLE_DIR, "baseline_reproduction_summary_step7.csv")
)



# ------------------------------------------------------------
# 7.10 Peak-response robustness summary
# ------------------------------------------------------------

robust_peak_summary <- robust_irfs %>%
  group_by(asset, central_bank, specification, bandwidth_type) %>%
  summarise(
    max_positive_response = max(estimate, na.rm = TRUE),
    horizon_max_positive = horizon[which.max(estimate)],
    max_negative_response = min(estimate, na.rm = TRUE),
    horizon_max_negative = horizon[which.min(estimate)],
    max_absolute_response = estimate[which.max(abs(estimate))],
    horizon_max_absolute = horizon[which.max(abs(estimate))],
    n_sig_95 = sum(p_value < 0.05, na.rm = TRUE),
    horizons_sig_95 = paste(horizon[p_value < 0.05], collapse = ", "),
    min_n_obs = min(n_obs, na.rm = TRUE),
    max_n_obs = max(n_obs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(asset, central_bank, specification, bandwidth_type)

robust_peak_summary

write_csv(
  robust_peak_summary,
  file.path(TABLE_DIR, "robust_peak_summary_step7.csv")
)



# ------------------------------------------------------------
# 7.11 Selected horizon robustness table
# ------------------------------------------------------------

selected_horizons <- c(0, 1, 2, 5, 10, 15, 20, 25)

robust_selected_horizon_table <- robust_irfs %>%
  filter(horizon %in% selected_horizons) %>%
  select(
    asset,
    central_bank,
    specification,
    bandwidth_type,
    horizon,
    estimate,
    std_error,
    ci_95_low,
    ci_95_high,
    p_value,
    n_obs,
    r_squared
  ) %>%
  arrange(asset, central_bank, specification, bandwidth_type, horizon)

robust_selected_horizon_table

write_csv(
  robust_selected_horizon_table,
  file.path(TABLE_DIR, "robust_selected_horizon_table_step7.csv")
)



# ------------------------------------------------------------
# 7.12 Robustness comparison plots
# ------------------------------------------------------------

robust_plot_data <- robust_irfs %>%
  filter(bandwidth_type == "h_plus_1")

plot_robustness <- function(selected_asset, selected_central_bank) {
  
  plot_data <- robust_plot_data %>%
    filter(
      asset == selected_asset,
      central_bank == selected_central_bank
    )
  
  ggplot(
    plot_data,
    aes(
      x = horizon,
      y = estimate,
      linetype = specification,
      shape = specification
    )
  ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.5) +
    labs(
      title = paste0(
        selected_asset,
        " ",
        selected_central_bank,
        " MP shock robustness"
      ),
      subtitle = "Cumulative log return response to a 1-SD MP shock",
      x = "Horizon in trading days",
      y = "Cumulative log return response, percentage points",
      linetype = "Specification",
      shape = "Specification"
    ) +
    theme_minimal()
}

robust_plot_specs <- robust_model_specs %>%
  distinct(asset, central_bank)

for (i in seq_len(nrow(robust_plot_specs))) {
  
  selected_asset <- robust_plot_specs$asset[i]
  selected_central_bank <- robust_plot_specs$central_bank[i]
  
  p <- plot_robustness(
    selected_asset = selected_asset,
    selected_central_bank = selected_central_bank
  )
  
  file_name <- paste0(
    tolower(selected_asset), "_",
    tolower(selected_central_bank), "_mp_robustness_step7.png"
  )
  
  ggsave(
    filename = file.path(FIGURE_DIR, file_name),
    plot = p,
    width = 8,
    height = 5,
    dpi = 300
  )
}



# ------------------------------------------------------------
# 7.13 Bandwidth robustness plots
# ------------------------------------------------------------

bandwidth_plot_data <- robust_irfs %>%
  filter(specification == "baseline")

plot_bandwidth_robustness <- function(selected_asset, selected_central_bank) {
  
  plot_data <- bandwidth_plot_data %>%
    filter(
      asset == selected_asset,
      central_bank == selected_central_bank
    )
  
  ggplot(
    plot_data,
    aes(
      x = horizon,
      y = estimate,
      linetype = bandwidth_type,
      shape = bandwidth_type
    )
  ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.5) +
    labs(
      title = paste0(
        selected_asset,
        " ",
        selected_central_bank,
        " Newey-West bandwidth robustness"
      ),
      subtitle = "Baseline controls; cumulative log return response to a 1-SD MP shock",
      x = "Horizon in trading days",
      y = "Cumulative log return response, percentage points",
      linetype = "Bandwidth",
      shape = "Bandwidth"
    ) +
    theme_minimal()
}

for (i in seq_len(nrow(robust_plot_specs))) {
  
  selected_asset <- robust_plot_specs$asset[i]
  selected_central_bank <- robust_plot_specs$central_bank[i]
  
  p <- plot_bandwidth_robustness(
    selected_asset = selected_asset,
    selected_central_bank = selected_central_bank
  )
  
  file_name <- paste0(
    tolower(selected_asset), "_",
    tolower(selected_central_bank), "_mp_bandwidth_robustness_step7.png"
  )
  
  ggsave(
    filename = file.path(FIGURE_DIR, file_name),
    plot = p,
    width = 8,
    height = 5,
    dpi = 300
  )
}


# ------------------------------------------------------------
# 7.14 Save robustness note
# ------------------------------------------------------------

robustness_note <- paste(
  "Robustness note:",
  "The main MP impulse responses are re-estimated under alternative specifications.",
  "First, Newey-West bandwidths are varied between h+1, fixed 5, and fixed 10.",
  "Second, the lag structure is expanded from one lag to two lags.",
  "Third, a reduced-control model excludes the COVID, MiCA, hack, and Monday dummies.",
  "The purpose is to assess whether the sign, timing, and magnitude of the main impulse responses are stable across reasonable alternatives.",
  sep = "\n"
)

writeLines(
  robustness_note,
  con = file.path(TABLE_DIR, "robustness_note_step7.txt")
)

robustness_note


