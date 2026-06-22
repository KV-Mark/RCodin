# ------------------------------------------------------------
# Step 4: Run baseline local-projection IRFs for all assets
# Assets: BTC, ETH, USDT
# Shocks: Fed MP, Fed CBI, ECB MP, ECB CBI
# ------------------------------------------------------------

source(file.path(path.expand("~"), "Documents", "Timi", "RThesis", "RCodin", "R", "0_setup.R"))
# source(file.path(path.expand("~"), "Downloads", "RThesis", "R", "0_setup.R"))

# Load Step 2 trading-day datasets
us_dt <- readRDS(file.path(PROCESSED_DIR, "us_trading_step2.rds"))
eu_dt <- readRDS(file.path(PROCESSED_DIR, "eu_trading_step2.rds"))

# Basic checks
range(us_dt$date, na.rm = TRUE)
range(eu_dt$date, na.rm = TRUE)

names(us_dt)
names(eu_dt)

# ------------------------------------------------------------
# 4.2 Helper function: add lags
# ------------------------------------------------------------

add_lags <- function(data, vars, lags = 1:2) {
  for (v in vars) {
    for (l in lags) {
      new_name <- paste0(v, "_l", l)
      data[[new_name]] <- dplyr::lag(data[[v]], l)
    }
  }
  
  data
}



# ------------------------------------------------------------
# 4.3 Helper function: create cumulative return horizon variables
# ------------------------------------------------------------
# For each horizon h, this creates:
# 100 * [ln(P_{t+h}) - ln(P_{t-1})]
#
# This matches the thesis definition of cumulative log return
# from the trading day before the announcement to horizon h.

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
# 4.4 Helper function: estimate one local-projection IRF
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
    
    # Newey-West HAC standard errors.
    # The lag increases with horizon because forecast errors overlap more
    # at longer local-projection horizons.
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
# 4.5 Prepare US local-projection dataset
# ------------------------------------------------------------

us_lag_vars <- c(
  "btc_ret",
  "eth_ret",
  "usdt_ret",
  "sp500_ret",
  "stoxx50_ret",
  "vix_log",
  "dxy_ret",
  "us_short_spread",
  "us_long_spread"
)

us_dt_lp <- us_dt %>%
  add_lags(vars = us_lag_vars, lags = 1) %>%
  add_cumulative_return_horizons(
    log_price_var = "btc_log_price",
    y_var = "btc_ret",
    max_h = 25
  ) %>%
  add_cumulative_return_horizons(
    log_price_var = "eth_log_price",
    y_var = "eth_ret",
    max_h = 25
  ) %>%
  add_cumulative_return_horizons(
    log_price_var = "usdt_log_price",
    y_var = "usdt_ret",
    max_h = 25
  ) %>%
  add_cumulative_return_horizons(
    log_price_var = "sp500_log_price",
    y_var = "sp500_ret",
    max_h = 25
  ) %>%
  add_cumulative_return_horizons(
    log_price_var = "stoxx50_log_price",
    y_var = "stoxx50_ret",
    max_h = 25
  )

# ------------------------------------------------------------
# 4.6 Prepare EU local-projection dataset
# ------------------------------------------------------------

eu_lag_vars <- c(
  "btc_ret",
  "eth_ret",
  "usdt_ret",
  "sp500_ret",
  "stoxx50_ret",
  "vix_log",
  "dxy_ret",
  "de_short_spread",
  "de_long_spread"
)

eu_dt_lp <- eu_dt %>%
  add_lags(vars = eu_lag_vars, lags = 1) %>%
  add_cumulative_return_horizons(
    log_price_var = "btc_log_price",
    y_var = "btc_ret",
    max_h = 25
  ) %>%
  add_cumulative_return_horizons(
    log_price_var = "eth_log_price",
    y_var = "eth_ret",
    max_h = 25
  ) %>%
  add_cumulative_return_horizons(
    log_price_var = "usdt_log_price",
    y_var = "usdt_ret",
    max_h = 25
  ) %>%
  add_cumulative_return_horizons(
    log_price_var = "sp500_log_price",
    y_var = "sp500_ret",
    max_h = 25
  ) %>%
  add_cumulative_return_horizons(
    log_price_var = "stoxx50_log_price",
    y_var = "stoxx50_ret",
    max_h = 25
  )




# ------------------------------------------------------------
# 4.7 Confirm 25 trading-day horizon
# ------------------------------------------------------------

us_horizon_check <- us_dt_lp %>%
  mutate(
    date_h25 = lead(date, 25),
    calendar_days_to_h25 = as.integer(date_h25 - date)
  ) %>%
  summarise(
    min_calendar_days = min(calendar_days_to_h25, na.rm = TRUE),
    median_calendar_days = median(calendar_days_to_h25, na.rm = TRUE),
    max_calendar_days = max(calendar_days_to_h25, na.rm = TRUE)
  )

eu_horizon_check <- eu_dt_lp %>%
  mutate(
    date_h25 = lead(date, 25),
    calendar_days_to_h25 = as.integer(date_h25 - date)
  ) %>%
  summarise(
    min_calendar_days = min(calendar_days_to_h25, na.rm = TRUE),
    median_calendar_days = median(calendar_days_to_h25, na.rm = TRUE),
    max_calendar_days = max(calendar_days_to_h25, na.rm = TRUE)
  )

us_horizon_check
eu_horizon_check



# ------------------------------------------------------------
# 4.8 Define controls
# ------------------------------------------------------------

make_us_controls <- function(asset_ret) {
  unique(c(
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
  ))
}

make_eu_controls <- function(asset_ret) {
  unique(c(
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
  ))
}






# ------------------------------------------------------------
# 4.9 Model specifications
# ------------------------------------------------------------

asset_specs <- tibble(
  asset = c("BTC", "ETH", "USDT"),
  y_var = c("btc_ret", "eth_ret", "usdt_ret")
)

us_specs <- asset_specs %>%
  crossing(
    central_bank = "Fed",
    shock_type = c("MP", "CBI")
  ) %>%
  mutate(
    shock_var = case_when(
      shock_type == "MP" ~ "fed_mp_std",
      shock_type == "CBI" ~ "fed_cbi_std"
    ),
    calendar = "US"
  )

eu_specs <- asset_specs %>%
  crossing(
    central_bank = "ECB",
    shock_type = c("MP", "CBI")
  ) %>%
  mutate(
    shock_var = case_when(
      shock_type == "MP" ~ "ecb_mp_std",
      shock_type == "CBI" ~ "ecb_cbi_std"
    ),
    calendar = "EU"
  )

model_specs <- bind_rows(us_specs, eu_specs) %>%
  arrange(central_bank, shock_type, asset)

model_specs




# ------------------------------------------------------------
# 4.10 Run all baseline IRFs
# ------------------------------------------------------------

run_one_irf <- function(asset, y_var, central_bank, shock_type, shock_var, calendar) {
  
  if (calendar == "US") {
    data_used <- us_dt_lp
    controls_used <- make_us_controls(y_var)
  } else if (calendar == "EU") {
    data_used <- eu_dt_lp
    controls_used <- make_eu_controls(y_var)
  } else {
    stop("Unknown calendar: ", calendar)
  }
  
  estimate_lp_irf(
    data = data_used,
    y_var = y_var,
    shock_var = shock_var,
    controls = controls_used,
    max_h = 25
  ) %>%
    mutate(
      asset = asset,
      y_var = y_var,
      central_bank = central_bank,
      shock_type = shock_type,
      shock_var = shock_var,
      calendar = calendar,
      .before = horizon
    )
}

all_irfs <- pmap_dfr(
  model_specs,
  run_one_irf
)

all_irfs


# ------------------------------------------------------------
# 4.11 Save full baseline IRF table
# ------------------------------------------------------------

# write_csv(
#   all_irfs,
#   file.path(TABLE_DIR, "all_baseline_irfs_step4.csv")
# )

saveRDS(
  all_irfs,
  file.path(PROCESSED_DIR, "all_baseline_irfs_step4.rds")
)

saveRDS(
  us_dt_lp,
  file.path(PROCESSED_DIR, "us_trading_step4_lp_ready.rds")
)

saveRDS(
  eu_dt_lp,
  file.path(PROCESSED_DIR, "eu_trading_step4_lp_ready.rds")
)


if (FALSE) {
# ------------------------------------------------------------
# 4.11b Run baseline local-projection IRFs for stock-market assets
# ------------------------------------------------------------
# These stock-market IRFs are kept separate from the cryptocurrency IRFs.
# This avoids changing the main crypto thesis tables and hypothesis summaries.
#
# Current stock-market series in the dataset:
# SP500   = US stock-market benchmark
# STOXX50 = Euro-area stock-market benchmark

stock_asset_specs <- tibble(
  asset = c("SP500", "STOXX50"),
  y_var = c("sp500_ret", "stoxx50_ret")
)

stock_us_specs <- stock_asset_specs %>%
  crossing(
    central_bank = "Fed",
    shock_type = c("MP", "CBI")
  ) %>%
  mutate(
    shock_var = case_when(
      shock_type == "MP" ~ "fed_mp_std",
      shock_type == "CBI" ~ "fed_cbi_std"
    ),
    calendar = "US"
  )

stock_eu_specs <- stock_asset_specs %>%
  crossing(
    central_bank = "ECB",
    shock_type = c("MP", "CBI")
  ) %>%
  mutate(
    shock_var = case_when(
      shock_type == "MP" ~ "ecb_mp_std",
      shock_type == "CBI" ~ "ecb_cbi_std"
    ),
    calendar = "EU"
  )

stock_model_specs <- bind_rows(stock_us_specs, stock_eu_specs) %>%
  arrange(central_bank, shock_type, asset)

stock_model_specs

stock_irfs <- pmap_dfr(
  stock_model_specs,
  run_one_irf
)

stock_irfs

# Expected with the current dataset:
# 2 stock-market assets × 2 central banks × 2 shock types × 26 horizons = 208 rows
nrow(stock_irfs)

write_csv(
  stock_irfs,
  file.path(TABLE_DIR, "stock_baseline_irfs_step4.csv")
)

saveRDS(
  stock_irfs,
  file.path(PROCESSED_DIR, "stock_baseline_irfs_step4.rds")
)

}


if (FALSE) {
# ------------------------------------------------------------
# 4.12 Plot one selected IRF 
# ------------------------------------------------------------

plot_irf <- function(irf_data, selected_asset, selected_central_bank, selected_shock_type) {
  
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
      alpha = 0.20
    ) +
    geom_ribbon(
      aes(ymin = ci_68_low, ymax = ci_68_high),
      alpha = 0.35
    ) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.8) +
    labs(
      title = paste0(
        selected_asset,
        " response to ",
        selected_central_bank,
        " ",
        selected_shock_type,
        " shock"
      ),
      subtitle = "Cumulative log return response to a 1-SD shock, Newey-West standard errors",
      x = "Horizon in trading days",
      y = paste0(selected_asset, " cumulative log return response, percentage points")
    ) +
    theme_minimal()
}

btc_fed_mp_plot_step4 <- plot_irf(
  irf_data = all_irfs,
  selected_asset = "BTC",
  selected_central_bank = "Fed",
  selected_shock_type = "MP"
)

btc_fed_mp_plot_step4




# ------------------------------------------------------------
# 4.13 Save all 12 baseline IRF plots
# ------------------------------------------------------------

for (i in seq_len(nrow(model_specs))) {
  
  selected_asset <- model_specs$asset[i]
  selected_central_bank <- model_specs$central_bank[i]
  selected_shock_type <- model_specs$shock_type[i]
  
  p <- plot_irf(
    irf_data = all_irfs,
    selected_asset = selected_asset,
    selected_central_bank = selected_central_bank,
    selected_shock_type = selected_shock_type
  )
  
  file_name <- paste0(
    tolower(selected_asset), "_",
    tolower(selected_central_bank), "_",
    tolower(selected_shock_type), "_irf_step4.png"
  )
  
  ggsave(
    filename = file.path(FIGURE_DIR, file_name),
    plot = p,
    width = 8,
    height = 5,
    dpi = 300
  )
}


}




