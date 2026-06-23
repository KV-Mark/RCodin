# ============================================================
# Step 9: Thesis-oriented results tables
# Purpose:
# 1. Create compact tables for H1-H4 interpretation
# 2. Separate short-run contraction from full-horizon peak response
# 3. Create final selected-horizon tables for the thesis
# 4. Create thesis-ready notes for results and robustness sections
# ============================================================

source(file.path(path.expand("~"), "Documents", "Timi", "RThesis", "RCodin", "R", "0_setup.R"))
# source(file.path(path.expand("~"), "Downloads", "RThesis", "R", "0_setup.R"))

# ------------------------------------------------------------
# 9.1 Load outputs from previous steps
# ------------------------------------------------------------

main_mp_irfs <- readRDS(file.path(PROCESSED_DIR, "main_mp_irfs_step5.rds"))
appendix_cbi_irfs <- readRDS(file.path(PROCESSED_DIR, "appendix_cbi_irfs_step5.rds"))

robust_irfs <- readRDS(file.path(PROCESSED_DIR, "robust_irfs_step7.rds"))

return_summary <- read_csv(
  file.path(TABLE_DIR, "return_summary_statistics_rounded_step6.csv"),
  show_col_types = FALSE
)

control_summary <- read_csv(
  file.path(TABLE_DIR, "control_summary_statistics_rounded_step6.csv"),
  show_col_types = FALSE
)

shock_summary <- read_csv(
  file.path(TABLE_DIR, "shock_summary_all_rounded_step6.csv"),
  show_col_types = FALSE
)

event_counts <- read_csv(
  file.path(TABLE_DIR, "event_counts_all_step6.csv"),
  show_col_types = FALSE
)

thesis_robustness_table <- read_csv(
  file.path(TABLE_DIR, "thesis_robustness_table_step8.csv"),
  show_col_types = FALSE
)



# ------------------------------------------------------------
# 9.2 Helper functions
# ------------------------------------------------------------

sig_stars <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE ~ ""
  )
}

round_numeric_cols <- function(data, digits = 3) {
  data %>%
    mutate(
      across(
        .cols = where(is.numeric),
        .fns = ~ round(.x, digits)
      )
    )
}



# ------------------------------------------------------------
# 9.3 Selected-horizon main MP results
# ------------------------------------------------------------

selected_horizons <- c(0, 1, 2, 5, 10, 15, 20, 25)

main_selected_horizon_table <- main_mp_irfs %>%
  filter(horizon %in% selected_horizons) %>%
  mutate(
    significance = sig_stars(p_value),
    estimate_with_stars = paste0(round(estimate, 3), significance)
  ) %>%
  select(
    asset,
    central_bank,
    horizon,
    estimate,
    std_error,
    ci_95_low,
    ci_95_high,
    p_value,
    significance,
    estimate_with_stars,
    n_obs,
    r_squared
  ) %>%
  arrange(asset, central_bank, horizon)

main_selected_horizon_table

main_selected_horizon_table_rounded <- main_selected_horizon_table %>%
  round_numeric_cols(digits = 3)

write_csv(
  main_selected_horizon_table,
  file.path(TABLE_DIR, "main_selected_horizon_table_step9.csv")
)

write_csv(
  main_selected_horizon_table_rounded,
  file.path(TABLE_DIR, "main_selected_horizon_table_rounded_step9.csv")
)



# ------------------------------------------------------------
# 9.4 H1: Short-run contractionary response
# ------------------------------------------------------------

h1_short_run_table <- main_mp_irfs %>%
  filter(horizon %in% 0:5) %>%
  group_by(asset, central_bank) %>%
  summarise(
    min_response_h0_h5 = min(estimate, na.rm = TRUE),
    horizon_min_response_h0_h5 = horizon[which.min(estimate)],
    max_response_h0_h5 = max(estimate, na.rm = TRUE),
    horizon_max_response_h0_h5 = horizon[which.max(estimate)],
    mean_response_h0_h5 = mean(estimate, na.rm = TRUE),
    n_negative_h0_h5 = sum(estimate < 0, na.rm = TRUE),
    n_positive_h0_h5 = sum(estimate > 0, na.rm = TRUE),
    n_sig_95_h0_h5 = sum(p_value < 0.05, na.rm = TRUE),
    horizons_sig_95_h0_h5 = paste(horizon[p_value < 0.05], collapse = ", "),
    short_run_shape = case_when(
      n_negative_h0_h5 > n_positive_h0_h5 ~ "mostly negative",
      n_positive_h0_h5 > n_negative_h0_h5 ~ "mostly positive",
      TRUE ~ "mixed"
    ),
    .groups = "drop"
  ) %>%
  arrange(asset, central_bank)

h1_short_run_table

h1_short_run_table_rounded <- h1_short_run_table %>%
  round_numeric_cols(digits = 3)

write_csv(
  h1_short_run_table,
  file.path(TABLE_DIR, "h1_short_run_table_step9.csv")
)

write_csv(
  h1_short_run_table_rounded,
  file.path(TABLE_DIR, "h1_short_run_table_rounded_step9.csv")
)



# ------------------------------------------------------------
# 9.5 Full-horizon peak response table
# ------------------------------------------------------------

main_peak_response_table <- main_mp_irfs %>%
  group_by(asset, central_bank) %>%
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
  arrange(asset, central_bank)

main_peak_response_table

main_peak_response_table_rounded <- main_peak_response_table %>%
  round_numeric_cols(digits = 3)

write_csv(
  main_peak_response_table,
  file.path(TABLE_DIR, "main_peak_response_table_step9.csv")
)

write_csv(
  main_peak_response_table_rounded,
  file.path(TABLE_DIR, "main_peak_response_table_rounded_step9.csv")
)



# ------------------------------------------------------------
# 9.6 H2: Cross-currency heterogeneity
# ------------------------------------------------------------

h2_cross_currency_table <- main_peak_response_table %>%
  mutate(
    abs_max_response = abs(max_absolute_response)
  ) %>%
  select(
    asset,
    central_bank,
    abs_max_response,
    max_absolute_response,
    horizon_max_absolute,
    n_sig_95
  ) %>%
  arrange(central_bank, desc(abs_max_response))

h2_cross_currency_wide <- h2_cross_currency_table %>%
  select(asset, central_bank, abs_max_response) %>%
  pivot_wider(
    names_from = asset,
    values_from = abs_max_response
  ) %>%
  mutate(
    btc_gt_usdt = BTC > USDT,
    eth_gt_usdt = ETH > USDT,
    eth_gt_btc = ETH > BTC
  )

h2_cross_currency_table
h2_cross_currency_wide

write_csv(
  h2_cross_currency_table,
  file.path(TABLE_DIR, "h2_cross_currency_table_step9.csv")
)

write_csv(
  h2_cross_currency_wide,
  file.path(TABLE_DIR, "h2_cross_currency_wide_step9.csv")
)



# ------------------------------------------------------------
# 9.7 H3: Central-bank asymmetry
# ------------------------------------------------------------

h3_central_bank_asymmetry <- main_peak_response_table %>%
  mutate(
    abs_max_response = abs(max_absolute_response),
    abs_negative_trough = abs(max_negative_response)
  ) %>%
  select(
    asset,
    central_bank,
    abs_max_response,
    max_absolute_response,
    horizon_max_absolute,
    abs_negative_trough,
    max_negative_response,
    horizon_max_negative
  ) %>%
  pivot_wider(
    names_from = central_bank,
    values_from = c(
      abs_max_response,
      max_absolute_response,
      horizon_max_absolute,
      abs_negative_trough,
      max_negative_response,
      horizon_max_negative
    )
  ) %>%
  mutate(
    fed_minus_ecb_abs_peak = abs_max_response_Fed - abs_max_response_ECB,
    fed_larger_abs_peak = abs_max_response_Fed > abs_max_response_ECB,
    fed_minus_ecb_negative_trough = abs_negative_trough_Fed - abs_negative_trough_ECB,
    fed_larger_negative_trough = abs_negative_trough_Fed > abs_negative_trough_ECB
  ) %>%
  arrange(asset)

h3_central_bank_asymmetry

h3_central_bank_asymmetry_rounded <- h3_central_bank_asymmetry %>%
  round_numeric_cols(digits = 3)

write_csv(
  h3_central_bank_asymmetry,
  file.path(TABLE_DIR, "h3_central_bank_asymmetry_step9.csv")
)

write_csv(
  h3_central_bank_asymmetry_rounded,
  file.path(TABLE_DIR, "h3_central_bank_asymmetry_rounded_step9.csv")
)



# ------------------------------------------------------------
# 9.8 H4: Tether insulation
# ------------------------------------------------------------

h4_tether_insulation <- main_mp_irfs %>%
  filter(asset == "USDT") %>%
  group_by(central_bank) %>%
  summarise(
    max_positive_response = max(estimate, na.rm = TRUE),
    horizon_max_positive = horizon[which.max(estimate)],
    max_negative_response = min(estimate, na.rm = TRUE),
    horizon_max_negative = horizon[which.min(estimate)],
    max_absolute_response = estimate[which.max(abs(estimate))],
    abs_max_response = abs(max_absolute_response),
    horizon_max_absolute = horizon[which.max(abs(estimate))],
    n_sig_95 = sum(p_value < 0.05, na.rm = TRUE),
    horizons_sig_95 = paste(horizon[p_value < 0.05], collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(central_bank)

h4_tether_insulation

h4_tether_insulation_rounded <- h4_tether_insulation %>%
  round_numeric_cols(digits = 4)

write_csv(
  h4_tether_insulation,
  file.path(TABLE_DIR, "h4_tether_insulation_step9.csv")
)

write_csv(
  h4_tether_insulation_rounded,
  file.path(TABLE_DIR, "h4_tether_insulation_rounded_step9.csv")
)



# ------------------------------------------------------------
# 9.9 Robustness table for thesis
# ------------------------------------------------------------

robustness_table_for_thesis <- thesis_robustness_table %>%
  select(
    asset,
    central_bank,
    min_abs_peak,
    max_abs_peak,
    mean_abs_peak,
    sd_abs_peak,
    earliest_peak_horizon,
    latest_peak_horizon,
    dominant_peak_sign,
    sign_stable,
    min_sig_95_count,
    max_sig_95_count
  ) %>%
  arrange(asset, central_bank)

robustness_table_for_thesis

write_csv(
  robustness_table_for_thesis,
  file.path(TABLE_DIR, "robustness_table_for_thesis_step9.csv")
)



# ------------------------------------------------------------
# 9.10 Save thesis result notes
# ------------------------------------------------------------

main_results_note <- paste(
  "Main results note:",
  "The main MP tables report cumulative log return responses, in percentage points, to a one-standard-deviation pure monetary-policy tightening shock.",
  "The selected-horizon table reports coefficients at h = 0, 1, 2, 5, 10, 15, 20, and 25 trading days.",
  "The short-run H1 table focuses on horizons h = 0 to h = 5, because the contractionary monetary-policy response is expected to appear shortly after the announcement.",
  "The peak-response table separates the maximum positive and maximum negative responses. This is important because a maximum absolute response can occur at a late positive horizon even when the short-run response is negative.",
  "The H2 table compares response magnitudes across BTC, ETH, and USDT.",
  "The H3 table compares Fed and ECB responses within each cryptocurrency.",
  "The H4 table focuses on whether USDT responses remain economically small and statistically insignificant.",
  sep = "\n"
)

writeLines(
  main_results_note,
  con = file.path(TABLE_DIR, "main_results_note_step9.txt")
)

robustness_results_note <- paste(
  "Robustness results note:",
  "The robustness checks compare the baseline specification against alternative Newey-West bandwidths, a two-lag specification, and a reduced-control specification without event-period dummies.",
  "Alternative Newey-West bandwidths do not change OLS point estimates, but they can change standard errors and significance patterns.",
  "The two-lag and no-dummy specifications can change point estimates because they alter the regression controls.",
  "Robustness should therefore be evaluated by checking whether the broad sign, timing, and magnitude of responses remain similar across specifications.",
  sep = "\n"
)

writeLines(
  robustness_results_note,
  con = file.path(TABLE_DIR, "robustness_results_note_step9.txt")
)

main_results_note
robustness_results_note



# ------------------------------------------------------------
# 9.11 Output checklist
# ------------------------------------------------------------

step9_outputs <- tibble(
  output = c(
    "main_selected_horizon_table_rounded_step9.csv",
    "h1_short_run_table_rounded_step9.csv",
    "main_peak_response_table_rounded_step9.csv",
    "h2_cross_currency_table_step9.csv",
    "h2_cross_currency_wide_step9.csv",
    "h3_central_bank_asymmetry_rounded_step9.csv",
    "h4_tether_insulation_rounded_step9.csv",
    "robustness_table_for_thesis_step9.csv",
    "main_results_note_step9.txt",
    "robustness_results_note_step9.txt"
  ),
  location = TABLE_DIR,
  exists = file.exists(file.path(location, output))
)

step9_outputs

write_csv(
  step9_outputs,
  file.path(TABLE_DIR, "step9_output_checklist.csv")
)



