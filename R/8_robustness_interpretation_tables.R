# ============================================================
# Step 8: Compact robustness interpretation tables
# Purpose:
# 1. Summarise robustness output into readable thesis tables
# 2. Compare baseline, two-lag, and no-dummy specifications
# 3. Compare Newey-West bandwidth choices
# 4. Create final robustness wording helpers
# ============================================================

source(file.path(path.expand("~"), "Documents", "Timi", "RThesis", "RCodin", "R", "0_setup.R"))
# source(file.path(path.expand("~"), "Downloads", "RThesis", "R", "0_setup.R"))

# ------------------------------------------------------------
# 8.1 Load robustness outputs
# ------------------------------------------------------------

robust_irfs <- readRDS(file.path(PROCESSED_DIR, "robust_irfs_step7.rds"))
main_mp_irfs <- readRDS(file.path(PROCESSED_DIR, "main_mp_irfs_step5.rds"))

# Basic checks
nrow(robust_irfs)

robust_irfs %>%
  count(asset, central_bank, specification, bandwidth_type)

# ------------------------------------------------------------
# 8.2 Peak response summary
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

print(robust_peak_summary, n = 54)

write_csv(
  robust_peak_summary,
  file.path(TABLE_DIR, "robust_peak_summary_step8.csv")
)


# ------------------------------------------------------------
# 8.3 Compact robustness range table
# ------------------------------------------------------------

robust_range_summary <- robust_peak_summary %>%
  group_by(asset, central_bank) %>%
  summarise(
    min_peak_response = min(max_absolute_response, na.rm = TRUE),
    max_peak_response = max(max_absolute_response, na.rm = TRUE),
    min_abs_peak = min(abs(max_absolute_response), na.rm = TRUE),
    max_abs_peak = max(abs(max_absolute_response), na.rm = TRUE),
    mean_abs_peak = mean(abs(max_absolute_response), na.rm = TRUE),
    sd_abs_peak = sd(abs(max_absolute_response), na.rm = TRUE),
    earliest_peak_horizon = min(horizon_max_absolute, na.rm = TRUE),
    latest_peak_horizon = max(horizon_max_absolute, na.rm = TRUE),
    min_sig_95_count = min(n_sig_95, na.rm = TRUE),
    max_sig_95_count = max(n_sig_95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(asset, central_bank)

robust_range_summary

write_csv(
  robust_range_summary,
  file.path(TABLE_DIR, "robust_range_summary_step8.csv")
)



# ------------------------------------------------------------
# 8.4 Sign stability table
# ------------------------------------------------------------

robust_sign_stability <- robust_peak_summary %>%
  mutate(
    peak_sign = case_when(
      max_absolute_response > 0 ~ "positive",
      max_absolute_response < 0 ~ "negative",
      TRUE ~ "zero"
    )
  ) %>%
  group_by(asset, central_bank) %>%
  summarise(
    n_variants = n(),
    n_positive_peak = sum(peak_sign == "positive", na.rm = TRUE),
    n_negative_peak = sum(peak_sign == "negative", na.rm = TRUE),
    dominant_peak_sign = case_when(
      n_positive_peak > n_negative_peak ~ "positive",
      n_negative_peak > n_positive_peak ~ "negative",
      TRUE ~ "mixed"
    ),
    sign_stable = n_distinct(peak_sign) == 1,
    .groups = "drop"
  ) %>%
  arrange(asset, central_bank)

robust_sign_stability

write_csv(
  robust_sign_stability,
  file.path(TABLE_DIR, "robust_sign_stability_step8.csv")
)



# ------------------------------------------------------------
# 8.5 Specification sensitivity
# ------------------------------------------------------------

specification_sensitivity <- robust_peak_summary %>%
  filter(bandwidth_type == "h_plus_1") %>%
  select(
    asset,
    central_bank,
    specification,
    max_absolute_response,
    horizon_max_absolute,
    n_sig_95,
    min_n_obs
  ) %>%
  pivot_wider(
    names_from = specification,
    values_from = c(
      max_absolute_response,
      horizon_max_absolute,
      n_sig_95,
      min_n_obs
    )
  ) %>%
  mutate(
    two_lags_minus_baseline = max_absolute_response_two_lags - max_absolute_response_baseline,
    no_dummies_minus_baseline = max_absolute_response_no_dummies - max_absolute_response_baseline,
    abs_two_lags_minus_baseline = abs(two_lags_minus_baseline),
    abs_no_dummies_minus_baseline = abs(no_dummies_minus_baseline)
  ) %>%
  arrange(asset, central_bank)

specification_sensitivity

write_csv(
  specification_sensitivity,
  file.path(TABLE_DIR, "specification_sensitivity_step8.csv")
)



# ------------------------------------------------------------
# 8.6 Newey-West bandwidth sensitivity
# ------------------------------------------------------------

bandwidth_sensitivity <- robust_peak_summary %>%
  filter(specification == "baseline") %>%
  select(
    asset,
    central_bank,
    bandwidth_type,
    max_absolute_response,
    horizon_max_absolute,
    n_sig_95
  ) %>%
  pivot_wider(
    names_from = bandwidth_type,
    values_from = c(
      max_absolute_response,
      horizon_max_absolute,
      n_sig_95
    )
  ) %>%
  mutate(
    fixed_5_minus_h_plus_1 = max_absolute_response_fixed_5 - max_absolute_response_h_plus_1,
    fixed_10_minus_h_plus_1 = max_absolute_response_fixed_10 - max_absolute_response_h_plus_1,
    abs_fixed_5_minus_h_plus_1 = abs(fixed_5_minus_h_plus_1),
    abs_fixed_10_minus_h_plus_1 = abs(fixed_10_minus_h_plus_1)
  ) %>%
  arrange(asset, central_bank)

bandwidth_sensitivity

write_csv(
  bandwidth_sensitivity,
  file.path(TABLE_DIR, "bandwidth_sensitivity_step8.csv")
)




# ------------------------------------------------------------
# 8.7 Significance stability
# ------------------------------------------------------------

robust_significance_summary <- robust_irfs %>%
  mutate(
    sig_95 = p_value < 0.05,
    sig_90 = p_value < 0.10
  ) %>%
  group_by(asset, central_bank, specification, bandwidth_type) %>%
  summarise(
    n_sig_95 = sum(sig_95, na.rm = TRUE),
    horizons_sig_95 = paste(horizon[sig_95], collapse = ", "),
    n_sig_90 = sum(sig_90, na.rm = TRUE),
    horizons_sig_90 = paste(horizon[sig_90], collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(asset, central_bank, specification, bandwidth_type)

robust_significance_summary

write_csv(
  robust_significance_summary,
  file.path(TABLE_DIR, "robust_significance_summary_step8.csv")
)



# ------------------------------------------------------------
# 8.8 Thesis-ready robustness table
# ------------------------------------------------------------

thesis_robustness_table <- robust_range_summary %>%
  left_join(
    robust_sign_stability,
    by = c("asset", "central_bank")
  ) %>%
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
  mutate(
    across(
      .cols = where(is.numeric),
      .fns = ~ round(.x, 4)
    )
  ) %>%
  arrange(asset, central_bank)

thesis_robustness_table

write_csv(
  thesis_robustness_table,
  file.path(TABLE_DIR, "thesis_robustness_table_step8.csv")
)



# ------------------------------------------------------------
# 8.9 Save robustness interpretation helper note
# ------------------------------------------------------------

robustness_interpretation_note <- paste(
  "Robustness interpretation guide:",
  "",
  "The baseline specification is compared with alternative Newey-West bandwidths, a two-lag specification, and a reduced-control specification excluding event-period dummies.",
  "Because the alternative Newey-West bandwidths affect the estimated standard errors but not the OLS point estimates, bandwidth robustness should primarily be evaluated through changes in confidence intervals and significance counts.",
  "The two-lag and no-dummy specifications can change point estimates because they alter the regression controls.",
  "For each asset-central-bank pair, inspect whether the dominant sign of the peak response remains stable, whether the peak horizon remains similar, and whether the magnitude of the response changes materially.",
  "If the sign and broad shape remain stable, the results can be described as qualitatively robust even when conventional statistical significance is weak.",
  sep = "\n"
)

writeLines(
  robustness_interpretation_note,
  con = file.path(TABLE_DIR, "robustness_interpretation_note_step8.txt")
)

robustness_interpretation_note



