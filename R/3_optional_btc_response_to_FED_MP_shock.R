# ============================================================
# Step 3: First thesis-consistent local-projection IRF
# BTC cumulative response to a 1-SD Fed MP shock
# ============================================================

source(file.path(path.expand("~"), "Documents", "Timi", "RThesis", "RCodin", "R", "0_setup.R"))

# ------------------------------------------------------------
# 3.1 Load Step 2 US trading-day data
# ------------------------------------------------------------

us_dt <- readRDS(file.path(PROCESSED_DIR, "us_trading_step2.rds"))

# Basic checks
glimpse(us_dt)
range(us_dt$date, na.rm = TRUE)

# Check that Step 2 contains the new thesis-consistent variables
required_vars_step3 <- c(
  "date",
  "btc_log_price",
  "btc_ret",
  "sp500_ret",
  "vix_log",
  "dxy_ret",
  "us_short_spread",
  "us_long_spread",
  "covid_dummy",
  "mica_dummy",
  "hack_dummy",
  "monday_dummy",
  "fed_mp_std"
)

setdiff(required_vars_step3, names(us_dt))


# ------------------------------------------------------------
# 3.2 Helper function: add lags
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


# ------------------------------------------------------------
# 3.3 Helper function: create cumulative return horizons
# ------------------------------------------------------------
# For each horizon h, this creates:
# 100 * [ln(P_{t+h}) - ln(P_{t-1})]
#
# This matches the thesis definition:
# cumulative log return from the trading day before the announcement
# to horizon h.

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


# ------------------------------------------------------------
# 3.4 Prepare BTC-Fed local-projection dataset
# ------------------------------------------------------------

us_lag_base_vars <- c(
  "btc_ret",
  "sp500_ret",
  "vix_log",
  "dxy_ret",
  "us_short_spread",
  "us_long_spread"
)

us_dt_lp <- us_dt %>%
  add_lags(vars = us_lag_base_vars, lags = 1) %>%
  add_cumulative_return_horizons(
    log_price_var = "btc_log_price",
    y_var = "btc_ret",
    max_h = 25
  )

# Check that lagged controls and cumulative horizons exist
names(us_dt_lp) %>%
  str_subset("_l1$|_cum_h")


# ------------------------------------------------------------
# 3.5 Verify horizon 25 means 25 trading days
# ------------------------------------------------------------

horizon_check <- us_dt_lp %>%
  mutate(
    date_h25 = lead(date, 25),
    calendar_days_to_h25 = as.integer(date_h25 - date)
  ) %>%
  summarise(
    min_calendar_days = min(calendar_days_to_h25, na.rm = TRUE),
    median_calendar_days = median(calendar_days_to_h25, na.rm = TRUE),
    max_calendar_days = max(calendar_days_to_h25, na.rm = TRUE)
  )

horizon_check


# ------------------------------------------------------------
# 3.6 Helper function: estimate local projections
# ------------------------------------------------------------

estimate_lp_irf <- function(data, y_var, shock_var, controls, max_h = 25) {
  
  results <- purrr::map_dfr(0:max_h, function(h) {
    
    dep_var <- paste0(y_var, "_cum_h", h)
    
    rhs_vars <- c(shock_var, controls)
    
    formula_text <- paste(
      dep_var,
      "~",
      paste(rhs_vars, collapse = " + ")
    )
    
    model_formula <- as.formula(formula_text)
    
    model <- lm(model_formula, data = data)
    
    # Newey-West HAC standard errors
    nw_lag <- h + 1
    
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
      ci_95_high = estimate + 1.96 * std_error,
      ci_68_low = estimate - 1.00 * std_error,
      ci_68_high = estimate + 1.00 * std_error
    )
}


# ------------------------------------------------------------
# 3.7 Estimate first thesis-consistent IRF
# ------------------------------------------------------------

us_controls_btc <- c(
  "btc_ret_l1",
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

btc_fed_mp_irf <- estimate_lp_irf(
  data = us_dt_lp,
  y_var = "btc_ret",
  shock_var = "fed_mp_std",
  controls = us_controls_btc,
  max_h = 25
)

btc_fed_mp_irf


# ------------------------------------------------------------
# 3.8 Output checks
# ------------------------------------------------------------

btc_fed_mp_irf %>%
  select(
    horizon,
    estimate,
    std_error,
    p_value,
    ci_95_low,
    ci_95_high,
    n_obs,
    r_squared
  )

nrow(btc_fed_mp_irf)
unique(btc_fed_mp_irf$horizon)


# ------------------------------------------------------------
# 3.9 Plot first thesis-consistent IRF
# ------------------------------------------------------------

btc_fed_mp_plot <- ggplot(btc_fed_mp_irf, aes(x = horizon, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(
    aes(ymin = ci_95_low, ymax = ci_95_high),
    alpha = 0.20
  ) +
  geom_ribbon(
    aes(ymin = ci_68_low, ymax = ci_68_high),
    alpha = 0.35
  ) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  labs(
    title = "BTC response to a 1-SD Fed MP shock",
    subtitle = "Cumulative log return response, US trading-day horizon, Newey-West standard errors",
    x = "Horizon in US trading days",
    y = "BTC cumulative log return response, percentage points"
  ) +
  theme_minimal()

btc_fed_mp_plot


# ------------------------------------------------------------
# 3.10 Save Step 3 outputs
# ------------------------------------------------------------

write_csv(
  btc_fed_mp_irf,
  file.path(TABLE_DIR, "btc_fed_mp_irf_step3.csv")
)

ggsave(
  filename = file.path(FIGURE_DIR, "btc_fed_mp_irf_step3.png"),
  plot = btc_fed_mp_plot,
  width = 8,
  height = 5,
  dpi = 300
)

saveRDS(
  us_dt_lp,
  file.path(PROCESSED_DIR, "us_trading_step3_lp_ready.rds")
)