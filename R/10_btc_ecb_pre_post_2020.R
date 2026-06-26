# ==============================================================================
# 10_btc_ecb_pre_post_2020.R
# Thesis project: The Effect of Monetary Policy Announcements on Cryptocurrency Returns
#
# Purpose:
#   - Run the BTC-ECB local projection model separately for:
#       1) the sample up to the end of 2019
#       2) the sample from the start of 2020 onward
#   - Save detailed IRF tables for both subsamples
#   - Save one 2-cell PNG figure with:
#       left panel  = pre-2020
#       right panel = 2020 onward
#
# Inputs:
#   data/processed/03_model_sample_ecb.csv
#
# Outputs:
#   output/tables/table_btc_ecb_irf_pre_2020.csv
#   output/tables/table_btc_ecb_irf_post_2020.csv
#   output/tables/table_btc_ecb_irf_pre_post_2020_combined.csv
#   output/tables/table_btc_ecb_pre_post_2020_sample_summary.csv
#   output/tables/validation_10_btc_ecb_pre_post_2020_config.csv
#   output/figures/figure_btc_ecb_irf_pre_post_2020_2grid.png
# ==============================================================================


# ------------------------------------------------------------------------------
# 10.0 Load setup
# ------------------------------------------------------------------------------

setup_candidates <- c(
  file.path("RCodin", "R", "0_setup.R"),
  file.path(getwd(), "0_setup.R"),
  file.path(path.expand("~"), "Documents", "Timi", "RThesis", "RCodin", "R", "0_setup.R")
)

setup_file <- setup_candidates[file.exists(setup_candidates)][1]

if (is.na(setup_file)) {
  stop(
    "Could not find 0_setup.R. Check the project location.",
    call. = FALSE
  )
}

source(setup_file)


# ------------------------------------------------------------------------------
# 10.1 Load LP wrapper
# ------------------------------------------------------------------------------

wrapper_candidates <- c(
  file.path("RCodin", "R", "4_lpirf_wrapper.R"),
  file.path(getwd(), "4_lpirf_wrapper.R"),
  file.path(path.expand("~"), "Documents", "Timi", "RThesis", "RCodin", "R", "4_lpirf_wrapper.R")
)

wrapper_file <- wrapper_candidates[file.exists(wrapper_candidates)][1]

if (is.na(wrapper_file)) {
  stop(
    "Could not find 4_lpirf_wrapper.R.",
    call. = FALSE
  )
}

source(wrapper_file)


# ------------------------------------------------------------------------------
# 10.2 Read ECB model sample
# ------------------------------------------------------------------------------

ecb_input_file <- file.path(PATHS$data_processed, "03_model_sample_ecb.csv")

if (!file.exists(ecb_input_file)) {
  stop(
    "Could not find ECB model sample: ",
    ecb_input_file,
    "\nRun source('RCodin/R/3_make_model_samples.R') first.",
    call. = FALSE
  )
}

ecb_sample <- safe_read_csv(ecb_input_file)

if (!DATE_VAR %in% names(ecb_sample)) {
  stop("ECB model sample does not contain the date column.", call. = FALSE)
}

ecb_sample <- ecb_sample %>%
  mutate(date = as.Date(.data[[DATE_VAR]])) %>%
  arrange(.data$date)

check_required_columns(
  ecb_sample,
  unique(c(
    DATE_VAR,
    "btc_log_return",
    "ecb_mp",
    "ecb_meeting",
    "ecb_mp_nonzero",
    ECB_CONTROL_VARS
  )),
  data_name = "ECB model sample"
)

message("ECB model sample loaded.")
message("Rows: ", nrow(ecb_sample))


# ------------------------------------------------------------------------------
# 10.3 Split into pre-2020 and post-2020 subsamples
# ------------------------------------------------------------------------------

split_date <- as.Date("2020-01-01")

ecb_pre_2020 <- ecb_sample %>%
  filter(.data$date <= as.Date("2019-12-31"))

ecb_post_2020 <- ecb_sample %>%
  filter(.data$date >= split_date)

if (nrow(ecb_pre_2020) == 0) {
  stop("Pre-2020 ECB sample is empty.", call. = FALSE)
}

if (nrow(ecb_post_2020) == 0) {
  stop("Post-2020 ECB sample is empty.", call. = FALSE)
}

if (sum(ecb_pre_2020$ecb_mp_nonzero == 1, na.rm = TRUE) == 0) {
  stop("Pre-2020 ECB sample contains no nonzero ECB shocks.", call. = FALSE)
}

if (sum(ecb_post_2020$ecb_mp_nonzero == 1, na.rm = TRUE) == 0) {
  stop("Post-2020 ECB sample contains no nonzero ECB shocks.", call. = FALSE)
}

message("Pre-2020 sample rows: ", nrow(ecb_pre_2020))
message("Post-2020 sample rows: ", nrow(ecb_post_2020))


# ------------------------------------------------------------------------------
# 10.4 Sample summary table
# ------------------------------------------------------------------------------

make_subsample_summary <- function(data, period_label) {
  tibble::tibble(
    period = period_label,
    rows = nrow(data),
    first_date = as.character(min(data$date, na.rm = TRUE)),
    last_date = as.character(max(data$date, na.rm = TRUE)),
    observed_meetings = sum(data$ecb_meeting == 1, na.rm = TRUE),
    nonzero_shocks = sum(data$ecb_mp_nonzero == 1, na.rm = TRUE),
    zero_observed_shocks = sum(data$ecb_meeting == 1 & data$ecb_mp == 0, na.rm = TRUE),
    no_announcement_days = sum(data$ecb_meeting == 0, na.rm = TRUE),
    positive_shocks = sum(data$ecb_meeting == 1 & data$ecb_mp > 0, na.rm = TRUE),
    negative_shocks = sum(data$ecb_meeting == 1 & data$ecb_mp < 0, na.rm = TRUE),
    mean_shock = mean(data$ecb_mp[data$ecb_meeting == 1], na.rm = TRUE),
    sd_shock = stats::sd(data$ecb_mp[data$ecb_meeting == 1], na.rm = TRUE),
    mean_abs_shock = mean(abs(data$ecb_mp[data$ecb_meeting == 1]), na.rm = TRUE)
  )
}

sample_summary <- bind_rows(
  make_subsample_summary(ecb_pre_2020, "Pre-2020"),
  make_subsample_summary(ecb_post_2020, "2020 onward")
)

write_table_csv(
  sample_summary,
  "table_btc_ecb_pre_post_2020_sample_summary.csv",
  digits = TABLE_DIGITS
)


# ------------------------------------------------------------------------------
# 10.5 Estimate BTC-ECB IRFs for both subsamples
# ------------------------------------------------------------------------------

pre_result <- estimate_single_asset_irf(
  data = ecb_pre_2020,
  asset_var = "btc_log_return",
  shock_var = "ecb_mp",
  control_vars = ECB_CONTROL_VARS,
  central_bank = "ECB pre-2020",
  horizons = HORIZONS,
  lags_endog = LP_LAGS_ENDOG,
  lags_exog = LP_LAGS_EXOG,
  trend = LP_TREND,
  nw_lag_rule = NW_LAG_RULE,
  nw_fixed_lag = NULL,
  conf_z = CONF_Z,
  sig_level = SIG_LEVEL,
  run_lpirfs = TRUE,
  lpirfs_rds_file = file.path(
    PATHS$data_processed,
    "10_lpirfs_btc_ecb_pre_2020.rds"
  )
)

post_result <- estimate_single_asset_irf(
  data = ecb_post_2020,
  asset_var = "btc_log_return",
  shock_var = "ecb_mp",
  control_vars = ECB_CONTROL_VARS,
  central_bank = "ECB 2020 onward",
  horizons = HORIZONS,
  lags_endog = LP_LAGS_ENDOG,
  lags_exog = LP_LAGS_EXOG,
  trend = LP_TREND,
  nw_lag_rule = NW_LAG_RULE,
  nw_fixed_lag = NULL,
  conf_z = CONF_Z,
  sig_level = SIG_LEVEL,
  run_lpirfs = TRUE,
  lpirfs_rds_file = file.path(
    PATHS$data_processed,
    "10_lpirfs_btc_ecb_post_2020.rds"
  )
)


# ------------------------------------------------------------------------------
# 10.6 Extract and save detailed IRF tables
# ------------------------------------------------------------------------------

pre_irf_table <- pre_result$tidy_results %>%
  mutate(period = "Pre-2020") %>%
  select(
    period,
    central_bank,
    asset,
    asset_var,
    shock_var,
    horizon,
    coefficient,
    std_error,
    t_statistic,
    p_value,
    conf_low,
    conf_high,
    significant_5pct,
    significance_5pct,
    n_obs,
    n_nonzero_shocks,
    r_squared,
    adj_r_squared,
    f_p_value,
    nw_lag,
    regression_status,
    equation_lhs,
    equation_rhs
  )

post_irf_table <- post_result$tidy_results %>%
  mutate(period = "2020 onward") %>%
  select(
    period,
    central_bank,
    asset,
    asset_var,
    shock_var,
    horizon,
    coefficient,
    std_error,
    t_statistic,
    p_value,
    conf_low,
    conf_high,
    significant_5pct,
    significance_5pct,
    n_obs,
    n_nonzero_shocks,
    r_squared,
    adj_r_squared,
    f_p_value,
    nw_lag,
    regression_status,
    equation_lhs,
    equation_rhs
  )

combined_irf_table <- bind_rows(pre_irf_table, post_irf_table)

write_table_csv(
  pre_irf_table,
  "table_btc_ecb_irf_pre_2020.csv",
  digits = TABLE_DIGITS
)

write_table_csv(
  post_irf_table,
  "table_btc_ecb_irf_post_2020.csv",
  digits = TABLE_DIGITS
)

write_table_csv(
  combined_irf_table,
  "table_btc_ecb_irf_pre_post_2020_combined.csv",
  digits = TABLE_DIGITS
)


# ------------------------------------------------------------------------------
# 10.7 Save model configuration table
# ------------------------------------------------------------------------------

config_table <- bind_rows(
  pre_result$config %>% mutate(period = "Pre-2020"),
  post_result$config %>% mutate(period = "2020 onward")
)

write_table_csv(
  config_table,
  "validation_10_btc_ecb_pre_post_2020_config.csv",
  digits = TABLE_DIGITS
)


# ------------------------------------------------------------------------------
# 10.8 Create 2-cell IRF figure
# ------------------------------------------------------------------------------

plot_data <- combined_irf_table %>%
  filter(.data$regression_status == "OK") %>%
  mutate(
    period = factor(period, levels = c("Pre-2020", "2020 onward"))
  )

if (nrow(plot_data) == 0) {
  stop("No valid IRF rows available for plotting.", call. = FALSE)
}

max_abs_y <- max(
  abs(c(plot_data$conf_low, plot_data$conf_high)),
  na.rm = TRUE
)

if (!is.finite(max_abs_y)) {
  max_abs_y <- max(
    abs(c(plot_data$coefficient)),
    na.rm = TRUE
  )
}

if (!is.finite(max_abs_y) || max_abs_y == 0) {
  max_abs_y <- 0.01
}

irf_plot <- ggplot(
  plot_data,
  aes(x = horizon, y = coefficient)
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.40,
    color = "grey35"
  ) +
  geom_ribbon(
    aes(ymin = conf_low, ymax = conf_high),
    fill = "grey80",
    color = NA
  ) +
  geom_line(
    linewidth = 0.70,
    color = "black"
  ) +
  geom_point(
    size = 1.4,
    color = "black"
  ) +
  facet_wrap(
    ~ period,
    ncol = 2,
    scales = "fixed"
  ) +
  scale_x_continuous(
    breaks = seq(0, HORIZON_MAX, by = 5)
  ) +
  scale_y_continuous(
    limits = c(-max_abs_y, max_abs_y)
  ) +
  labs(
    title = "BTC response to ECB monetary policy surprises",
    subtitle = "Local projections, split sample comparison",
    x = "Horizon (trading days)",
    y = "IRF coefficient",
    caption = paste(
      "Notes: Each panel reports the estimated response of BTC daily log returns",
      "to an ECB monetary policy surprise over horizons h = 0,...,25.",
      "Shaded areas are 95% confidence intervals based on Newey-West HAC standard errors.",
      "The specification uses the baseline ECB control set and the ECB trading-day sample.",
      sep = " "
    )
  ) +
  theme_thesis(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )

save_figure_png(
  plot = irf_plot,
  filename = "figure_btc_ecb_irf_pre_post_2020_2grid.png",
  width = 12,
  height = 5.8,
  dpi = FIG_DPI
)


# ------------------------------------------------------------------------------
# 10.9 Final message
# ------------------------------------------------------------------------------

message("BTC-ECB pre/post-2020 subsample analysis completed successfully.")
message("Tables saved to: ", PATHS$tables)
message("Figure saved to: ", PATHS$figures)


message("Step 10 complete: thesis figures saved.")
