# ============================================================
# Step 5: IRF diagnostics and thesis-ready outputs
# Purpose:
# 1. Check that Step 4 produced the expected IRF objects
# 2. Separate main MP results from appendix CBI results
# 3. Create diagnostic tables for sample sizes and significance
# 4. Create peak-response tables
# 5. Save thesis-ready figures and tables
# ============================================================

source(file.path(path.expand("~"), "Documents", "Timi", "RThesis", "RCodin", "R", "0_setup.R"))
# source(file.path(path.expand("~"), "Downloads", "RThesis", "R", "0_setup.R"))

# ------------------------------------------------------------
# 5.1 Load Step 4 outputs
# ------------------------------------------------------------

all_irfs <- readRDS(file.path(PROCESSED_DIR, "all_baseline_irfs_step4.rds"))
stock_irfs <- readRDS(file.path(PROCESSED_DIR, "stock_baseline_irfs_step4.rds"))

us_dt <- readRDS(file.path(PROCESSED_DIR, "us_trading_step2.rds"))
eu_dt <- readRDS(file.path(PROCESSED_DIR, "eu_trading_step2.rds"))

us_dt_lp <- readRDS(file.path(PROCESSED_DIR, "us_trading_step4_lp_ready.rds"))
eu_dt_lp <- readRDS(file.path(PROCESSED_DIR, "eu_trading_step4_lp_ready.rds"))

# ------------------------------------------------------------
# 5.2 Basic structure checks
# ------------------------------------------------------------

required_irf_cols <- c(
  "asset",
  "y_var",
  "central_bank",
  "shock_type",
  "shock_var",
  "calendar",
  "horizon",
  "term",
  "estimate",
  "std_error",
  "statistic",
  "p_value",
  "n_obs",
  "nw_lag",
  "r_squared",
  "ci_95_low",
  "ci_95_high",
  "ci_68_low",
  "ci_68_high"
)

missing_irf_cols <- setdiff(required_irf_cols, names(all_irfs))
missing_irf_cols

# Expected:
# 3 assets × 2 central banks × 2 shock types × 26 horizons = 312 rows
nrow(all_irfs)

irf_count_check <- all_irfs %>%
  count(central_bank, shock_type, asset) %>%
  arrange(central_bank, shock_type, asset)

irf_count_check

horizon_coverage_check <- all_irfs %>%
  group_by(central_bank, shock_type, asset) %>%
  summarise(
    min_horizon = min(horizon),
    max_horizon = max(horizon),
    n_horizons = n(),
    .groups = "drop"
  ) %>%
  arrange(central_bank, shock_type, asset)

horizon_coverage_check

# write_csv(
#   irf_count_check,
#   file.path(TABLE_DIR, "irf_count_check_step5.csv")
# )
# 
# write_csv(
#   horizon_coverage_check,
#   file.path(TABLE_DIR, "horizon_coverage_check_step5.csv")
# )


# ------------------------------------------------------------
# 5.3 Separate main MP results and appendix CBI results
# ------------------------------------------------------------

main_mp_irfs <- all_irfs %>%
  filter(shock_type == "MP")

appendix_cbi_irfs <- all_irfs %>%
  filter(shock_type == "CBI")

# Expected:
# Main MP results: 3 assets × 2 central banks × 26 horizons = 156 rows
nrow(main_mp_irfs)

# Expected:
# Appendix CBI results: 3 assets × 2 central banks × 26 horizons = 156 rows
nrow(appendix_cbi_irfs)

# write_csv(
#   main_mp_irfs,
#   file.path(TABLE_DIR, "main_mp_irfs_step5.csv")
# )
# 
# write_csv(
#   appendix_cbi_irfs,
#   file.path(TABLE_DIR, "appendix_cbi_irfs_step5.csv")
# )

saveRDS(
  main_mp_irfs,
  file.path(PROCESSED_DIR, "main_mp_irfs_step5.rds")
)

saveRDS(
  appendix_cbi_irfs,
  file.path(PROCESSED_DIR, "appendix_cbi_irfs_step5.rds")
)

#if (FALSE) {

# ------------------------------------------------------------
# 5.3b Separate stock-market MP and CBI IRFs
# ------------------------------------------------------------
# These are kept separate from the cryptocurrency IRFs because the main thesis
# tests are defined for crypto assets. The stock-market IRFs are additional
# benchmark/diagnostic figures.

main_mp_stock_irfs <- stock_irfs %>%
  filter(shock_type == "MP")

appendix_cbi_stock_irfs <- stock_irfs %>%
  filter(shock_type == "CBI")

# Expected with the current dataset:
# Main MP stock results: 2 stock assets × 2 central banks × 26 horizons = 104 rows
nrow(main_mp_stock_irfs)

# Appendix CBI stock results: 2 stock assets × 2 central banks × 26 horizons = 104 rows
nrow(appendix_cbi_stock_irfs)

write_csv(
  main_mp_stock_irfs,
  file.path(TABLE_DIR, "main_mp_stock_irfs_step5.csv")
)

write_csv(
  appendix_cbi_stock_irfs,
  file.path(TABLE_DIR, "appendix_cbi_stock_irfs_step5.csv")
)

saveRDS(
  main_mp_stock_irfs,
  file.path(PROCESSED_DIR, "main_mp_stock_irfs_step5.rds")
)

saveRDS(
  appendix_cbi_stock_irfs,
  file.path(PROCESSED_DIR, "appendix_cbi_stock_irfs_step5.rds")
)

#}

# ------------------------------------------------------------
# 5.3c Detailed MP and CBI impulse-response tables
# ------------------------------------------------------------
# This creates six supervisor-style tables:
# 3 cryptocurrencies × 2 central banks = 6 CSV files.
# Each file contains horizons 0–25, with MP and CBI coefficients
# and p-values shown in separate columns.
#
# Important: MP and CBI estimates come from the separate local-projection
# regressions estimated in Step 4. This block only reshapes those Step 4
# estimates into the requested table format.

DETAILED_IRF_TABLE_DIR <- file.path(TABLE_DIR, "detailed_impulse_response_tables_mp_cbi")
dir.create(DETAILED_IRF_TABLE_DIR, recursive = TRUE, showWarnings = FALSE)

make_detailed_mp_cbi_irf_table <- function(irf_data, selected_asset, selected_central_bank) {
  
  irf_data %>%
    filter(
      asset == selected_asset,
      central_bank == selected_central_bank,
      shock_type %in% c("MP", "CBI")
    ) %>%
    mutate(
      shock_type = tolower(shock_type)
    ) %>%
    select(
      horizon,
      shock_type,
      estimate,
      p_value
    ) %>%
    pivot_wider(
      names_from = shock_type,
      values_from = c(estimate, p_value),
      names_glue = "{shock_type}_{.value}"
    ) %>%
    rename(
      mp_coefficient = mp_estimate,
      cbi_coefficient = cbi_estimate
    ) %>%
    select(
      horizon,
      mp_coefficient,
      mp_p_value,
      cbi_coefficient,
      cbi_p_value
    ) %>%
    arrange(horizon)
}

detailed_mp_cbi_irf_specs <- all_irfs %>%
  distinct(asset, central_bank) %>%
  arrange(asset, central_bank)

for (i in seq_len(nrow(detailed_mp_cbi_irf_specs))) {
  selected_asset <- detailed_mp_cbi_irf_specs$asset[i]
  selected_central_bank <- detailed_mp_cbi_irf_specs$central_bank[i]
  
  detailed_table <- make_detailed_mp_cbi_irf_table(
    irf_data = all_irfs,
    selected_asset = selected_asset,
    selected_central_bank = selected_central_bank
  )
  
  file_name <- paste0(
    tolower(selected_asset),
    "_response_to_",
    tolower(selected_central_bank),
    "_mp_cbi_detailed_irf.csv"
  )
  
  write_csv(
    detailed_table,
    file.path(DETAILED_IRF_TABLE_DIR, file_name)
  )
}

# Optional check: should show 6 files.
# list.files(DETAILED_IRF_TABLE_DIR)

if (FALSE) {
# ------------------------------------------------------------
# 5.4 Sample-size diagnostics
# ------------------------------------------------------------

sample_size_summary <- all_irfs %>%
  group_by(central_bank, shock_type, asset) %>%
  summarise(
    min_n_obs = min(n_obs, na.rm = TRUE),
    max_n_obs = max(n_obs, na.rm = TRUE),
    mean_n_obs = mean(n_obs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(asset, central_bank, shock_type)

sample_size_summary

write_csv(
  sample_size_summary,
  file.path(TABLE_DIR, "sample_size_summary_step5.csv")
)

# ------------------------------------------------------------
# 5.5 Significance diagnostics
# ------------------------------------------------------------

significance_summary <- all_irfs %>%
  mutate(
    sig_95 = p_value < 0.05,
    sig_90 = p_value < 0.10
  ) %>%
  group_by(central_bank, shock_type, asset) %>%
  summarise(
    n_sig_95 = sum(sig_95, na.rm = TRUE),
    horizons_sig_95 = paste(horizon[sig_95], collapse = ", "),
    n_sig_90 = sum(sig_90, na.rm = TRUE),
    horizons_sig_90 = paste(horizon[sig_90], collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(asset, central_bank, shock_type)

significance_summary

write_csv(
  significance_summary,
  file.path(TABLE_DIR, "significance_summary_step5.csv")
)

# ------------------------------------------------------------
# 5.6 Shock diagnostics
# ------------------------------------------------------------

shock_diagnostics_one <- function(data, central_bank_name, shock_type_name, raw_var, std_var, event_var) {
  
  raw_x <- data[[raw_var]]
  std_x <- data[[std_var]]
  event_x <- data[[event_var]]
  
  tibble(
    central_bank = central_bank_name,
    shock_type = shock_type_name,
    raw_var = raw_var,
    std_var = std_var,
    total_event_rows = sum(!is.na(event_x)),
    nonzero_shocks = sum(raw_x != 0, na.rm = TRUE),
    positive_shocks = sum(raw_x > 0, na.rm = TRUE),
    negative_shocks = sum(raw_x < 0, na.rm = TRUE),
    raw_sd_nonzero = sd(raw_x[raw_x != 0], na.rm = TRUE),
    std_sd_nonzero = sd(std_x[raw_x != 0], na.rm = TRUE),
    raw_min_nonzero = min(raw_x[raw_x != 0], na.rm = TRUE),
    raw_max_nonzero = max(raw_x[raw_x != 0], na.rm = TRUE)
  )
}

shock_diagnostics <- bind_rows(
  shock_diagnostics_one(
    data = us_dt,
    central_bank_name = "Fed",
    shock_type_name = "MP",
    raw_var = "fed_mp",
    std_var = "fed_mp_std",
    event_var = "fed_pc1"
  ),
  shock_diagnostics_one(
    data = us_dt,
    central_bank_name = "Fed",
    shock_type_name = "CBI",
    raw_var = "fed_cbi",
    std_var = "fed_cbi_std",
    event_var = "fed_pc1"
  ),
  shock_diagnostics_one(
    data = eu_dt,
    central_bank_name = "ECB",
    shock_type_name = "MP",
    raw_var = "ecb_mp",
    std_var = "ecb_mp_std",
    event_var = "ecb_pc1"
  ),
  shock_diagnostics_one(
    data = eu_dt,
    central_bank_name = "ECB",
    shock_type_name = "CBI",
    raw_var = "ecb_cbi",
    std_var = "ecb_cbi_std",
    event_var = "ecb_pc1"
  )
)

shock_diagnostics

write_csv(
  shock_diagnostics,
  file.path(TABLE_DIR, "shock_diagnostics_step5.csv")
)

# ------------------------------------------------------------
# 5.7 Yield duplicate diagnostic
# ------------------------------------------------------------

same_numeric_with_na <- function(x, y, tolerance = 1e-12) {
  both_na <- is.na(x) & is.na(y)
  both_present_same <- !is.na(x) & !is.na(y) & abs(x - y) < tolerance
  
  all(both_na | both_present_same)
}

yield_duplicate_check_step5 <- tibble(
  pair = c(
    "us_2y vs de_2y",
    "us_3m vs de_3m",
    "us_10y vs de_10y"
  ),
  identical_or_nearly_identical = c(
    same_numeric_with_na(us_dt$us_2y, us_dt$de_2y),
    same_numeric_with_na(us_dt$us_3m, us_dt$de_3m),
    same_numeric_with_na(us_dt$us_10y, us_dt$de_10y)
  )
)

yield_duplicate_check_step5

write_csv(
  yield_duplicate_check_step5,
  file.path(TABLE_DIR, "yield_duplicate_check_step5.csv")
)


# ------------------------------------------------------------
# 5.8 Peak-response tables
# ------------------------------------------------------------

make_peak_response_summary <- function(irf_data) {
  
  irf_data %>%
    group_by(asset, central_bank, shock_type) %>%
    summarise(
      max_positive_response = max(estimate, na.rm = TRUE),
      horizon_max_positive = horizon[which.max(estimate)],
      max_negative_response = min(estimate, na.rm = TRUE),
      horizon_max_negative = horizon[which.min(estimate)],
      max_absolute_response = estimate[which.max(abs(estimate))],
      horizon_max_absolute = horizon[which.max(abs(estimate))],
      n_sig_95 = sum(p_value < 0.05, na.rm = TRUE),
      horizons_sig_95 = paste(horizon[p_value < 0.05], collapse = ", "),
      n_sig_90 = sum(p_value < 0.10, na.rm = TRUE),
      horizons_sig_90 = paste(horizon[p_value < 0.10], collapse = ", "),
      min_n_obs = min(n_obs, na.rm = TRUE),
      max_n_obs = max(n_obs, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(asset, central_bank, shock_type)
}

peak_response_all <- make_peak_response_summary(all_irfs)
peak_response_mp <- make_peak_response_summary(main_mp_irfs)
peak_response_cbi <- make_peak_response_summary(appendix_cbi_irfs)

peak_response_mp
peak_response_cbi

write_csv(
  peak_response_all,
  file.path(TABLE_DIR, "peak_response_all_step5.csv")
)

write_csv(
  peak_response_mp,
  file.path(TABLE_DIR, "peak_response_mp_main_step5.csv")
)

write_csv(
  peak_response_cbi,
  file.path(TABLE_DIR, "peak_response_cbi_appendix_step5.csv")
)


# ------------------------------------------------------------
# 5.9 Selected-horizon result tables
# ------------------------------------------------------------

selected_horizons <- c(0, 1, 2, 5, 10, 15, 20, 25)

selected_horizon_table_all <- all_irfs %>%
  filter(horizon %in% selected_horizons) %>%
  select(
    asset,
    central_bank,
    shock_type,
    horizon,
    estimate,
    std_error,
    ci_95_low,
    ci_95_high,
    p_value,
    n_obs,
    r_squared
  ) %>%
  arrange(asset, central_bank, shock_type, horizon)

selected_horizon_table_mp <- selected_horizon_table_all %>%
  filter(shock_type == "MP")

selected_horizon_table_cbi <- selected_horizon_table_all %>%
  filter(shock_type == "CBI")

selected_horizon_table_mp
selected_horizon_table_cbi

write_csv(
  selected_horizon_table_all,
  file.path(TABLE_DIR, "selected_horizon_table_all_step5.csv")
)

write_csv(
  selected_horizon_table_mp,
  file.path(TABLE_DIR, "selected_horizon_table_mp_main_step5.csv")
)

write_csv(
  selected_horizon_table_cbi,
  file.path(TABLE_DIR, "selected_horizon_table_cbi_appendix_step5.csv")
)
}

if (FALSE) {
# ------------------------------------------------------------
# 5.10 Thesis-ready individual IRF plots
# ------------------------------------------------------------

plot_irf_95 <- function(irf_data, selected_asset, selected_central_bank, selected_shock_type) {
  
  plot_data <- irf_data %>%
    filter(
      asset == selected_asset,
      central_bank == selected_central_bank,
      shock_type == selected_shock_type
    )
  
  ggplot(plot_data, aes(x = horizon, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_ribbon(
      aes(ymin = ci_95_low, ymax = ci_95_high),
      alpha = 0.25
    ) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.8) +
    labs(
      title = paste0(
        selected_asset,
        " response to a 1-SD ",
        selected_central_bank,
        " ",
        selected_shock_type,
        " shock"
      ),
      subtitle = "Cumulative log return response, Newey-West 95% confidence interval",
      x = "Horizon in trading days",
      y = paste0(selected_asset, " cumulative log return response, percentage points")
    ) +
    theme_minimal()
}

# Save main MP plots
main_plot_specs <- main_mp_irfs %>%
  distinct(asset, central_bank, shock_type) %>%
  arrange(asset, central_bank)

for (i in seq_len(nrow(main_plot_specs))) {
  
  selected_asset <- main_plot_specs$asset[i]
  selected_central_bank <- main_plot_specs$central_bank[i]
  selected_shock_type <- main_plot_specs$shock_type[i]
  
  p <- plot_irf_95(
    irf_data = main_mp_irfs,
    selected_asset = selected_asset,
    selected_central_bank = selected_central_bank,
    selected_shock_type = selected_shock_type
  )
  
  file_name <- paste0(
    tolower(selected_asset), "_",
    tolower(selected_central_bank), "_",
    tolower(selected_shock_type), "_irf_main_95_step5.png"
  )
  
  ggsave(
    filename = file.path(FIGURE_DIR, file_name),
    plot = p,
    width = 8,
    height = 5,
    dpi = 300
  )
}

# Save appendix CBI plots
cbi_plot_specs <- appendix_cbi_irfs %>%
  distinct(asset, central_bank, shock_type) %>%
  arrange(asset, central_bank)

for (i in seq_len(nrow(cbi_plot_specs))) {
  
  selected_asset <- cbi_plot_specs$asset[i]
  selected_central_bank <- cbi_plot_specs$central_bank[i]
  selected_shock_type <- cbi_plot_specs$shock_type[i]
  
  p <- plot_irf_95(
    irf_data = appendix_cbi_irfs,
    selected_asset = selected_asset,
    selected_central_bank = selected_central_bank,
    selected_shock_type = selected_shock_type
  )
  
  file_name <- paste0(
    tolower(selected_asset), "_",
    tolower(selected_central_bank), "_",
    tolower(selected_shock_type), "_irf_appendix_95_step5.png"
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
# 5.10b Thesis-ready individual stock-market MP IRF plots
# ------------------------------------------------------------
# These figures mirror the main cryptocurrency MP plots, but for the
# stock-market benchmarks in the dataset.

STOCK_FIGURE_DIR <- file.path(FIGURE_DIR, "stock_irfs")
dir.create(STOCK_FIGURE_DIR, recursive = TRUE, showWarnings = FALSE)

stock_main_plot_specs <- main_mp_stock_irfs %>%
  distinct(asset, central_bank, shock_type) %>%
  arrange(asset, central_bank)

for (i in seq_len(nrow(stock_main_plot_specs))) {
  
  selected_asset <- stock_main_plot_specs$asset[i]
  selected_central_bank <- stock_main_plot_specs$central_bank[i]
  selected_shock_type <- stock_main_plot_specs$shock_type[i]
  
  p <- plot_irf_95(
    irf_data = main_mp_stock_irfs,
    selected_asset = selected_asset,
    selected_central_bank = selected_central_bank,
    selected_shock_type = selected_shock_type
  )
  
  file_name <- paste0(
    tolower(selected_asset), "_",
    tolower(selected_central_bank), "_",
    tolower(selected_shock_type), "_stock_irf_main_95_step5.png"
  )
  
  ggsave(
    filename = file.path(STOCK_FIGURE_DIR, file_name),
    plot = p,
    width = 8,
    height = 5,
    dpi = 300
  )
}


# ------------------------------------------------------------
# 5.10c Appendix individual stock-market CBI IRF plots
# ------------------------------------------------------------

stock_cbi_plot_specs <- appendix_cbi_stock_irfs %>%
  distinct(asset, central_bank, shock_type) %>%
  arrange(asset, central_bank)

for (i in seq_len(nrow(stock_cbi_plot_specs))) {
  
  selected_asset <- stock_cbi_plot_specs$asset[i]
  selected_central_bank <- stock_cbi_plot_specs$central_bank[i]
  selected_shock_type <- stock_cbi_plot_specs$shock_type[i]
  
  p <- plot_irf_95(
    irf_data = appendix_cbi_stock_irfs,
    selected_asset = selected_asset,
    selected_central_bank = selected_central_bank,
    selected_shock_type = selected_shock_type
  )
  
  file_name <- paste0(
    tolower(selected_asset), "_",
    tolower(selected_central_bank), "_",
    tolower(selected_shock_type), "_stock_irf_appendix_95_step5.png"
  )
  
  ggsave(
    filename = file.path(STOCK_FIGURE_DIR, file_name),
    plot = p,
    width = 8,
    height = 5,
    dpi = 300
  )
}

# ------------------------------------------------------------
# 5.10d Combined crypto-stock comparison IRF plots
# ------------------------------------------------------------
# These figures compare the two main cryptocurrencies (BTC, ETH)
# with the relevant stock-market benchmark in a single figure.
#
# Required figures:
# 1. ECB-MP:  BTC, ETH, STOXX50
# 2. Fed-MP:  BTC, ETH, SP500
# 3. ECB-CBI: BTC, ETH, STOXX50
# 4. Fed-CBI: BTC, ETH, SP500

COMPARISON_FIGURE_DIR <- file.path(FIGURE_DIR, "crypto_stock_comparison_irfs")
dir.create(COMPARISON_FIGURE_DIR, recursive = TRUE, showWarnings = FALSE)

# Combine crypto and stock IRFs into one object for plotting
comparison_irfs <- bind_rows(all_irfs, stock_irfs)

plot_multi_asset_irf_95 <- function(irf_data, selected_assets, selected_central_bank, selected_shock_type) {
  
  plot_data <- irf_data %>%
    filter(
      asset %in% selected_assets,
      central_bank == selected_central_bank,
      shock_type == selected_shock_type
    ) %>%
    mutate(
      asset = factor(asset, levels = selected_assets)
    )
  
  # End-of-line labels at the final horizon
  label_data <- plot_data %>%
    group_by(asset) %>%
    filter(horizon == max(horizon)) %>%
    slice_tail(n = 1) %>%
    ungroup()
  
  ggplot(
    plot_data,
    aes(
      x = horizon,
      y = estimate,
      color = asset,
      fill = asset,
      linetype = asset,
      shape = asset
    )
  ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_ribbon(
      aes(ymin = ci_95_low, ymax = ci_95_high),
      alpha = 0.16,
      colour = NA
    ) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.9) +
    geom_text(
      data = label_data,
      aes(label = asset),
      nudge_x = 0.7,
      hjust = 0,
      size = 3.6,
      show.legend = FALSE
    ) +
    scale_x_continuous(
      breaks = seq(0, 25, by = 5),
      limits = c(0, 27)
    ) +
    scale_color_manual(
      values = c(
        "BTC" = "#1f77b4",
        "ETH" = "#d62728",
        "STOXX50" = "#2ca02c",
        "SP500" = "#9467bd"
      )
    ) +
    scale_fill_manual(
      values = c(
        "BTC" = "#1f77b4",
        "ETH" = "#d62728",
        "STOXX50" = "#2ca02c",
        "SP500" = "#9467bd"
      )
    ) +
    scale_linetype_manual(
      values = c(
        "BTC" = "solid",
        "ETH" = "dashed",
        "STOXX50" = "dotdash",
        "SP500" = "twodash"
      )
    ) +
    scale_shape_manual(
      values = c(
        "BTC" = 16,
        "ETH" = 17,
        "STOXX50" = 15,
        "SP500" = 18
      )
    ) +
    labs(
      title = paste0(
        selected_central_bank, " ", selected_shock_type,
        " shock responses: crypto versus stock benchmark"
      ),
      subtitle = paste0(
        "Assets shown: ",
        paste(selected_assets, collapse = ", "),
        ". Cumulative log return response to a 1-SD shock; shaded areas are 95% Newey-West confidence intervals"
      ),
      x = "Horizon in trading days",
      y = "Cumulative log return response, percentage points",
      color = "Asset",
      fill = "Asset",
      linetype = "Asset",
      shape = "Asset"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    )
}

# ------------------------------------------------------------
# 5.10d.1 ECB-MP: BTC, ETH, STOXX50
# ------------------------------------------------------------

p_ecb_mp_crypto_stock <- plot_multi_asset_irf_95(
  irf_data = comparison_irfs,
  selected_assets = c("BTC", "ETH", "STOXX50"),
  selected_central_bank = "ECB",
  selected_shock_type = "MP"
)

ggsave(
  filename = file.path(
    COMPARISON_FIGURE_DIR,
    "ecb_mp_btc_eth_stoxx50_comparison_step5.png"
  ),
  plot = p_ecb_mp_crypto_stock,
  width = 9,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 5.10d.2 Fed-MP: BTC, ETH, SP500
# ------------------------------------------------------------

p_fed_mp_crypto_stock <- plot_multi_asset_irf_95(
  irf_data = comparison_irfs,
  selected_assets = c("BTC", "ETH", "SP500"),
  selected_central_bank = "Fed",
  selected_shock_type = "MP"
)

ggsave(
  filename = file.path(
    COMPARISON_FIGURE_DIR,
    "fed_mp_btc_eth_sp500_comparison_step5.png"
  ),
  plot = p_fed_mp_crypto_stock,
  width = 9,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 5.10d.3 ECB-CBI: BTC, ETH, STOXX50
# ------------------------------------------------------------

p_ecb_cbi_crypto_stock <- plot_multi_asset_irf_95(
  irf_data = comparison_irfs,
  selected_assets = c("BTC", "ETH", "STOXX50"),
  selected_central_bank = "ECB",
  selected_shock_type = "CBI"
)

ggsave(
  filename = file.path(
    COMPARISON_FIGURE_DIR,
    "ecb_cbi_btc_eth_stoxx50_comparison_step5.png"
  ),
  plot = p_ecb_cbi_crypto_stock,
  width = 9,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 5.10d.4 Fed-CBI: BTC, ETH, SP500
# ------------------------------------------------------------

p_fed_cbi_crypto_stock <- plot_multi_asset_irf_95(
  irf_data = comparison_irfs,
  selected_assets = c("BTC", "ETH", "SP500"),
  selected_central_bank = "Fed",
  selected_shock_type = "CBI"
)

ggsave(
  filename = file.path(
    COMPARISON_FIGURE_DIR,
    "fed_cbi_btc_eth_sp500_comparison_step5.png"
  ),
  plot = p_fed_cbi_crypto_stock,
  width = 9,
  height = 6,
  dpi = 300
)
}


# ------------------------------------------------------------
# 5.11 Main six-panel MP figure
# ------------------------------------------------------------

main_mp_panel_plot <- ggplot(
  main_mp_irfs,
  aes(x = horizon, y = estimate)
) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(
    aes(ymin = ci_95_low, ymax = ci_95_high),
    alpha = 0.25
  ) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.4) +
  facet_grid(asset ~ central_bank, scales = "free_y") +
  labs(
    title = "Impulse responses to pure monetary-policy surprises",
    subtitle = "Cumulative log return response to a 1-SD tightening shock; shaded areas are 95% Newey-West confidence intervals",
    x = "Horizon in trading days",
    y = "Cumulative log return response, percentage points"
  ) +
  theme_minimal()

main_mp_panel_plot

ggsave(
  filename = file.path(FIGURE_DIR, "main_mp_irfs_six_panel_step5.png"),
  plot = main_mp_panel_plot,
  width = 10,
  height = 8,
  dpi = 300
)


# ------------------------------------------------------------
# 5.12 Central-bank comparison plot for H3
# ------------------------------------------------------------

central_bank_comparison_plot <- ggplot(
  main_mp_irfs,
  aes(
    x = horizon,
    y = estimate,
    linetype = central_bank,
    shape = central_bank
  )
) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.6) +
  facet_wrap(~ asset, scales = "free_y") +
  labs(
    title = "Fed versus ECB monetary-policy shock responses",
    subtitle = "Cumulative log return response to a 1-SD pure MP shock",
    x = "Horizon in trading days",
    y = "Cumulative log return response, percentage points",
    linetype = "Central bank",
    shape = "Central bank"
  ) +
  theme_minimal()

central_bank_comparison_plot

ggsave(
  filename = file.path(FIGURE_DIR, "fed_vs_ecb_comparison_mp_step5.png"),
  plot = central_bank_comparison_plot,
  width = 10,
  height = 5.5,
  dpi = 300
)

if (FALSE) {
# ------------------------------------------------------------
# 5.13 Hypothesis-oriented diagnostics
# ------------------------------------------------------------

# H1: Are short-horizon MP responses negative?
h1_short_horizon_summary <- main_mp_irfs %>%
  filter(horizon %in% 0:5) %>%
  group_by(asset, central_bank) %>%
  summarise(
    mean_response_h0_h5 = mean(estimate, na.rm = TRUE),
    min_response_h0_h5 = min(estimate, na.rm = TRUE),
    horizon_min_response_h0_h5 = horizon[which.min(estimate)],
    n_negative_h0_h5 = sum(estimate < 0, na.rm = TRUE),
    n_sig_95_h0_h5 = sum(p_value < 0.05, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(asset, central_bank)

h1_short_horizon_summary

# H2: Are BTC/ETH responses larger than USDT?
h2_cross_asset_summary <- peak_response_mp %>%
  select(
    asset,
    central_bank,
    shock_type,
    max_absolute_response,
    horizon_max_absolute,
    n_sig_95
  ) %>%
  arrange(central_bank, desc(abs(max_absolute_response)))

h2_cross_asset_summary

# H3: Compare absolute peak responses by central bank
h3_central_bank_summary <- peak_response_mp %>%
  select(
    asset,
    central_bank,
    max_absolute_response,
    horizon_max_absolute
  ) %>%
  pivot_wider(
    names_from = central_bank,
    values_from = c(max_absolute_response, horizon_max_absolute)
  ) %>%
  mutate(
    abs_fed_peak = abs(max_absolute_response_Fed),
    abs_ecb_peak = abs(max_absolute_response_ECB),
    fed_minus_ecb_abs_peak = abs_fed_peak - abs_ecb_peak
  )

h3_central_bank_summary

# H4: Tether response size
h4_tether_summary <- main_mp_irfs %>%
  filter(asset == "USDT") %>%
  group_by(central_bank) %>%
  summarise(
    max_abs_usdt_response = max(abs(estimate), na.rm = TRUE),
    horizon_max_abs_usdt_response = horizon[which.max(abs(estimate))],
    n_sig_95 = sum(p_value < 0.05, na.rm = TRUE),
    .groups = "drop"
  )

h4_tether_summary

write_csv(
  h1_short_horizon_summary,
  file.path(TABLE_DIR, "h1_short_horizon_summary_step5.csv")
)

write_csv(
  h2_cross_asset_summary,
  file.path(TABLE_DIR, "h2_cross_asset_summary_step5.csv")
)

write_csv(
  h3_central_bank_summary,
  file.path(TABLE_DIR, "h3_central_bank_summary_step5.csv")
)

write_csv(
  h4_tether_summary,
  file.path(TABLE_DIR, "h4_tether_summary_step5.csv")
)

# ------------------------------------------------------------
# 5.14 Save thesis figure note
# ------------------------------------------------------------

figure_note_main_mp <- paste(
  "Note: The figure reports local-projection impulse responses of cumulative cryptocurrency log returns,",
  "expressed in percentage points, to a one-standard-deviation positive pure monetary-policy surprise.",
  "Positive shocks correspond to tightening surprises. Horizons are measured in trading days after the announcement.",
  "Each panel is estimated separately and includes one lag of the cryptocurrency return, lagged market controls,",
  "yield-spread controls, and event-period dummies. Shaded areas are pointwise 95% confidence intervals based on",
  "Newey-West HAC standard errors."
)

writeLines(
  figure_note_main_mp,
  con = file.path(TABLE_DIR, "figure_note_main_mp_step5.txt")
)

figure_note_main_mp

}


