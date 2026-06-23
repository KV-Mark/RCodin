# ==============================================================================
# 8_figures.R
# Thesis project: The Effect of Monetary Policy Announcements on Cryptocurrency Returns
#
# Purpose:
#   - Generate all required thesis figures as PNG images for MS Word
#   - Use black-and-white formatting
#   - Use 95% confidence intervals for IRF figures
#
# Inputs:
#   data/processed/02_clean_daily_data.csv
#   data/processed/03_model_sample_ecb.csv
#   data/processed/03_model_sample_fed.csv
#   data/processed/05_irf_results_long.csv
#
# Outputs:
#   output/figures/figure_02_returns_time_series.png
#   output/figures/figure_06_irf_6_grid_baseline.png
#   output/figures/figure_07_central_bank_asymmetry.png
#   output/figures/figure_08_cross_currency_heterogeneity.png
#   output/figures/figure_09_return_distributions.png
#   output/figures/figure_13_scatter_crypto_returns_mp_shocks.png
#
#   output/tables/validation_08_figure_generation_summary.csv
# ==============================================================================


# ------------------------------------------------------------------------------
# 8.0 Load setup
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
# 8.1 Read processed data
# ------------------------------------------------------------------------------

clean_data_file <- file.path(PATHS$data_processed, "02_clean_daily_data.csv")
ecb_sample_file <- file.path(PATHS$data_processed, "03_model_sample_ecb.csv")
fed_sample_file <- file.path(PATHS$data_processed, "03_model_sample_fed.csv")
irf_results_file <- file.path(PATHS$data_processed, "05_irf_results_long.csv")

required_files <- c(
  clean_data_file,
  ecb_sample_file,
  fed_sample_file,
  irf_results_file
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    "Missing required input files for Step 8: ",
    paste(missing_files, collapse = ", "),
    call. = FALSE
  )
}

clean_data <- safe_read_csv(clean_data_file) %>%
  mutate(date = as.Date(.data$date))

ecb_sample <- safe_read_csv(ecb_sample_file) %>%
  mutate(date = as.Date(.data$date))

fed_sample <- safe_read_csv(fed_sample_file) %>%
  mutate(date = as.Date(.data$date))

irf_results <- safe_read_csv(irf_results_file)

message("Clean daily data loaded. Rows: ", nrow(clean_data))
message("ECB model sample loaded. Rows: ", nrow(ecb_sample))
message("Fed model sample loaded. Rows: ", nrow(fed_sample))
message("IRF results loaded. Rows: ", nrow(irf_results))


# ------------------------------------------------------------------------------
# 8.2 Validate columns
# ------------------------------------------------------------------------------

check_required_columns(
  clean_data,
  unique(c(DATE_VAR, CRYPTO_RETURN_VARS, STOCK_RETURN_VARS)),
  data_name = "clean_data"
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
    "conf_low",
    "conf_high",
    "significant_5pct",
    "regression_status"
  ),
  data_name = "irf_results"
)

check_required_columns(
  ecb_sample,
  c("date", "trading_day_index", CRYPTO_RETURN_VARS, "ecb_mp", "ecb_mp_nonzero"),
  data_name = "ecb_sample"
)

check_required_columns(
  fed_sample,
  c("date", "trading_day_index", CRYPTO_RETURN_VARS, "fed_mp", "fed_mp_nonzero"),
  data_name = "fed_sample"
)


# ------------------------------------------------------------------------------
# 8.3 Figure helper functions
# ------------------------------------------------------------------------------

standardize_central_bank_order <- function(x) {
  factor(x, levels = c("ECB", "Fed"))
}

standardize_asset_order <- function(x) {
  factor(x, levels = c("BTC", "ETH", "USDT"))
}

return_series_labels <- c(
  btc_log_return = "BTC",
  eth_log_return = "ETH",
  usdt_log_return = "USDT",
  sp500_log_return = "S&P 500",
  stoxx50_log_return = "STOXX50"
)

make_return_label <- function(x) {
  labels <- return_series_labels[x]
  ifelse(is.na(labels), x, labels)
}

asset_linetypes <- c(
  BTC = "solid",
  ETH = "dashed",
  USDT = "dotted",
  `S&P 500` = "dotdash",
  STOXX50 = "longdash"
)

asset_shapes <- c(
  BTC = 16,
  ETH = 1,
  USDT = 2,
  `S&P 500` = 0,
  STOXX50 = 4
)

make_zero_line <- function() {
  ggplot2::geom_hline(
    yintercept = 0,
    linewidth = 0.25,
    linetype = "dotted"
  )
}


# ------------------------------------------------------------------------------
# 8.4 Prepare IRF plotting data
# ------------------------------------------------------------------------------

irf_plot_data <- irf_results %>%
  filter(.data$regression_status == "OK") %>%
  mutate(
    central_bank = as.character(.data$central_bank),
    asset = as.character(.data$asset),
    central_bank = standardize_central_bank_order(.data$central_bank),
    asset = standardize_asset_order(.data$asset),
    model_label = paste(.data$central_bank, .data$asset, sep = " - ")
  )

if (nrow(irf_plot_data) == 0) {
  stop("No valid IRF rows with regression_status == 'OK'. Cannot generate IRF figures.", call. = FALSE)
}


# ------------------------------------------------------------------------------
# 8.5 Figure 2: Return time series
# ------------------------------------------------------------------------------

returns_ts_data <- clean_data %>%
  select(
    date,
    btc_log_return,
    eth_log_return,
    usdt_log_return,
    sp500_log_return,
    stoxx50_log_return
  ) %>%
  pivot_longer(
    cols = -date,
    names_to = "series",
    values_to = "log_return"
  ) %>%
  filter(!is.na(.data$log_return)) %>%
  mutate(
    series_label = make_return_label(.data$series),
    series_label = factor(
      .data$series_label,
      levels = c("BTC", "ETH", "USDT", "S&P 500", "STOXX50")
    )
  )

figure_02 <- ggplot(
  returns_ts_data,
  aes(
    x = .data$date,
    y = .data$log_return,
    linetype = .data$series_label,
    group = .data$series_label
  )
) +
  geom_line(linewidth = 0.25) +
  scale_linetype_manual(values = asset_linetypes) +
  make_zero_line() +
  labs(
    title = "Daily Log Returns of Cryptocurrencies and Stock Indices",
    x = NULL,
    y = "Daily log return",
    caption = "Notes: The figure plots daily log returns for BTC, ETH, USDT, S&P 500, and STOXX50."
  ) +
  theme_thesis()

save_figure_png(
  figure_02,
  "figure_02_returns_time_series.png",
  width = 11,
  height = 6.5
)


# ------------------------------------------------------------------------------
# 8.6 Figure 6: Six-grid baseline IRFs
# ------------------------------------------------------------------------------

figure_06 <- ggplot(
  irf_plot_data,
  aes(
    x = .data$horizon,
    y = .data$coefficient
  )
) +
  geom_ribbon(
    aes(
      ymin = .data$conf_low,
      ymax = .data$conf_high
    ),
    fill = "grey80",
    color = NA
  ) +
  geom_line(linewidth = 0.45) +
  geom_point(
    aes(shape = .data$significant_5pct),
    size = 1.2,
    stroke = 0.45
  ) +
  scale_shape_manual(
    values = c(`FALSE` = 1, `TRUE` = 16),
    labels = c(`FALSE` = "Not significant at 5%", `TRUE` = "Significant at 5%")
  ) +
  make_zero_line() +
  facet_grid(
    rows = vars(.data$central_bank),
    cols = vars(.data$asset),
    scales = "free_y"
  ) +
  scale_x_continuous(
    breaks = seq(0, HORIZON_MAX, by = 5)
  ) +
  labs(
    title = "Baseline Impulse Responses to Monetary Policy Surprises",
    x = "Trading-day horizon",
    y = "IRF coefficient",
    caption = "Notes: Shaded areas are 95% confidence intervals. Filled markers indicate 5% statistical significance."
  ) +
  theme_thesis()

save_figure_png(
  figure_06,
  "figure_06_irf_6_grid_baseline.png",
  width = 12,
  height = 7.5
)


# ------------------------------------------------------------------------------
# 8.7 Figure 7: Central bank asymmetry
# ------------------------------------------------------------------------------

figure_07 <- ggplot(
  irf_plot_data,
  aes(
    x = .data$horizon,
    y = .data$coefficient,
    linetype = .data$central_bank,
    group = .data$central_bank
  )
) +
  geom_ribbon(
    aes(
      ymin = .data$conf_low,
      ymax = .data$conf_high,
      group = .data$central_bank
    ),
    fill = "grey85",
    alpha = 0.45,
    color = NA
  ) +
  geom_line(linewidth = 0.5) +
  scale_linetype_manual(
    values = c(ECB = "solid", Fed = "dashed")
  ) +
  make_zero_line() +
  facet_wrap(
    vars(.data$asset),
    nrow = 1,
    scales = "free_y"
  ) +
  scale_x_continuous(
    breaks = seq(0, HORIZON_MAX, by = 5)
  ) +
  labs(
    title = "Central Bank Asymmetry in Cryptocurrency Responses",
    x = "Trading-day horizon",
    y = "IRF coefficient",
    caption = "Notes: Each panel compares ECB and Fed monetary policy surprise responses for one cryptocurrency. Shaded areas are 95% confidence intervals."
  ) +
  theme_thesis()

save_figure_png(
  figure_07,
  "figure_07_central_bank_asymmetry.png",
  width = 12,
  height = 4.8
)


# ------------------------------------------------------------------------------
# 8.8 Figure 8: Cross-currency heterogeneity
# ------------------------------------------------------------------------------

figure_08 <- ggplot(
  irf_plot_data,
  aes(
    x = .data$horizon,
    y = .data$coefficient,
    linetype = .data$asset,
    group = .data$asset
  )
) +
  geom_line(linewidth = 0.55) +
  scale_linetype_manual(
    values = c(
      BTC = "solid",
      ETH = "dashed",
      USDT = "dotted"
    )
  ) +
  make_zero_line() +
  facet_wrap(
    vars(.data$central_bank),
    nrow = 1,
    scales = "free_y"
  ) +
  scale_x_continuous(
    breaks = seq(0, HORIZON_MAX, by = 5)
  ) +
  labs(
    title = "Cross-Currency Heterogeneity in Monetary Policy Responses",
    x = "Trading-day horizon",
    y = "IRF coefficient",
    caption = "Notes: The figure compares BTC, ETH, and USDT responses within each central-bank shock model. Confidence bands are omitted to keep cross-currency comparisons readable."
  ) +
  theme_thesis()

save_figure_png(
  figure_08,
  "figure_08_cross_currency_heterogeneity.png",
  width = 11,
  height = 5
)


# ------------------------------------------------------------------------------
# 8.9 Figure 9: Frequency distributions
# ------------------------------------------------------------------------------

distribution_data <- clean_data %>%
  select(
    btc_log_return,
    eth_log_return,
    usdt_log_return,
    sp500_log_return,
    stoxx50_log_return
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "series",
    values_to = "log_return"
  ) %>%
  filter(!is.na(.data$log_return)) %>%
  mutate(
    series_label = make_return_label(.data$series),
    series_label = factor(
      .data$series_label,
      levels = c("BTC", "ETH", "USDT", "S&P 500", "STOXX50")
    )
  )

figure_09 <- ggplot(
  distribution_data,
  aes(x = .data$log_return)
) +
  geom_histogram(
    bins = 60,
    fill = "grey80",
    color = "black",
    linewidth = 0.2
  ) +
  make_zero_line() +
  facet_wrap(
    vars(.data$series_label),
    nrow = 3,
    ncol = 2,
    scales = "free"
  ) +
  labs(
    title = "Frequency Distributions of Daily Log Returns",
    x = "Daily log return",
    y = "Frequency",
    caption = "Notes: The figure reports histograms for BTC, ETH, USDT, S&P 500, and STOXX50 daily log returns."
  ) +
  theme_thesis() +
  theme(
    legend.position = "none"
  )

save_figure_png(
  figure_09,
  "figure_09_return_distributions.png",
  width = 10.5,
  height = 8
)


# ------------------------------------------------------------------------------
# 8.10 Figure 13: Scatter plots of crypto returns against MP shocks
# ------------------------------------------------------------------------------

make_event_scatter_data <- function(
    model_sample,
    central_bank,
    shock_var,
    nonzero_var,
    horizons_to_plot = c(0, 10)
) {
  model_sample_ordered <- model_sample %>%
    arrange(.data$trading_day_index)
  
  output <- list()
  
  for (asset_i in CRYPTO_RETURN_VARS) {
    for (h_i in horizons_to_plot) {
      response_name <- paste0(asset_i, "_h", h_i)
      
      temp <- model_sample_ordered %>%
        mutate(
          response = if (h_i == 0) {
            .data[[asset_i]]
          } else {
            dplyr::lead(.data[[asset_i]], n = h_i)
          }
        ) %>%
        filter(.data[[nonzero_var]] == 1) %>%
        transmute(
          central_bank = central_bank,
          asset = label_asset(asset_i),
          asset_var = asset_i,
          shock_var = shock_var,
          horizon = h_i,
          date = .data$date,
          trading_day_index = .data$trading_day_index,
          shock_value = .data[[shock_var]],
          crypto_return = .data$response
        ) %>%
        filter(
          !is.na(.data$shock_value),
          !is.na(.data$crypto_return)
        )
      
      output[[paste(asset_i, h_i, sep = "_")]] <- temp
    }
  }
  
  bind_rows(output)
}

scatter_data <- bind_rows(
  make_event_scatter_data(
    model_sample = ecb_sample,
    central_bank = "ECB",
    shock_var = "ecb_mp",
    nonzero_var = "ecb_mp_nonzero",
    horizons_to_plot = c(0, 10)
  ),
  make_event_scatter_data(
    model_sample = fed_sample,
    central_bank = "Fed",
    shock_var = "fed_mp",
    nonzero_var = "fed_mp_nonzero",
    horizons_to_plot = c(0, 10)
  )
) %>%
  mutate(
    central_bank = standardize_central_bank_order(.data$central_bank),
    asset = standardize_asset_order(.data$asset),
    horizon_label = paste0("h = ", .data$horizon)
  )

if (nrow(scatter_data) == 0) {
  warning(
    "No scatter data available for nonzero MP shocks. Figure 13 will not be created.",
    call. = FALSE
  )
} else {
  figure_13 <- ggplot(
    scatter_data,
    aes(
      x = .data$shock_value,
      y = .data$crypto_return,
      shape = .data$horizon_label
    )
  ) +
    geom_point(
      alpha = 0.75,
      size = 1.5,
      stroke = 0.35
    ) +
    geom_smooth(
      aes(linetype = .data$horizon_label),
      method = "lm",
      se = FALSE,
      linewidth = 0.45,
      color = "black"
    ) +
    scale_shape_manual(
      values = c(`h = 0` = 16, `h = 10` = 1)
    ) +
    scale_linetype_manual(
      values = c(`h = 0` = "solid", `h = 10` = "dashed")
    ) +
    make_zero_line() +
    geom_vline(
      xintercept = 0,
      linewidth = 0.25,
      linetype = "dotted"
    ) +
    facet_grid(
      rows = vars(.data$asset),
      cols = vars(.data$central_bank),
      scales = "free"
    ) +
    labs(
      title = "Cryptocurrency Returns and Monetary Policy Surprises",
      x = "Monetary policy surprise",
      y = "Cryptocurrency log return",
      caption = "Notes: The scatter plot uses nonzero monetary policy announcement surprises only. Lines are fitted linear relationships for h = 0 and h = 10."
    ) +
    theme_thesis()
  
  save_figure_png(
    figure_13,
    "figure_13_scatter_crypto_returns_mp_shocks.png",
    width = 11.5,
    height = 8
  )
}


# ------------------------------------------------------------------------------
# 8.11 Figure generation validation summary
# ------------------------------------------------------------------------------

figure_files <- tibble::tibble(
  figure_number = c(
    "Figure 2",
    "Figure 6",
    "Figure 7",
    "Figure 8",
    "Figure 9",
    "Figure 13"
  ),
  figure_file = c(
    "figure_02_returns_time_series.png",
    "figure_06_irf_6_grid_baseline.png",
    "figure_07_central_bank_asymmetry.png",
    "figure_08_cross_currency_heterogeneity.png",
    "figure_09_return_distributions.png",
    "figure_13_scatter_crypto_returns_mp_shocks.png"
  ),
  description = c(
    "Daily log returns of BTC, ETH, USDT, S&P 500, and STOXX50",
    "Six-grid baseline IRFs for ECB/Fed and BTC/ETH/USDT",
    "Three-grid central bank asymmetry figure comparing ECB and Fed for each cryptocurrency",
    "Two-grid cross-currency heterogeneity figure comparing BTC, ETH, and USDT within each central-bank model",
    "Frequency distributions of BTC, ETH, USDT, S&P 500, and STOXX50 returns",
    "Scatter plot of crypto returns against monetary policy shocks for h = 0 and h = 10"
  )
) %>%
  mutate(
    path = file.path(PATHS$figures, .data$figure_file),
    file_exists = file.exists(.data$path),
    file_size_bytes = ifelse(
      .data$file_exists,
      file.info(.data$path)$size,
      NA_real_
    )
  )

write_table_csv(
  figure_files,
  "validation_08_figure_generation_summary.csv",
  digits = TABLE_DIGITS
)


# ------------------------------------------------------------------------------
# 8.12 Console preview
# ------------------------------------------------------------------------------

message("Step 8 figure generation summary:")

print(figure_files)


# ------------------------------------------------------------------------------
# 8.13 Final message
# ------------------------------------------------------------------------------

message("Step 8 complete: thesis figures saved.")