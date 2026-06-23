# ============================================================
# Step 2: Prepare crypto returns and trading-day samples
# ============================================================

source(file.path(path.expand("~"), "Documents", "Timi", "RThesis", "RCodin", "R", "0_setup.R"))
# source(file.path(path.expand("~"), "Downloads", "RThesis", "R", "0_setup.R"))

# ------------------------------------------------------------
# 2.2 Load merged data
# ------------------------------------------------------------

dt <- read_csv(
  file.path(RAW_DIR, "merged_data.csv"),
  show_col_types = FALSE
) %>%
  clean_names() %>%
  mutate(date = ymd(date)) %>%
  arrange(date)

# Check merged date parsing
range(dt$date, na.rm = TRUE)
class(dt$date)

# ------------------------------------------------------------
# 2.3 Load hacks data
# ------------------------------------------------------------
# Important:
# hacks.csv uses dates like "19.06.2011", i.e. day-month-year.
# Therefore we use dmy(), not ymd().

hacks_raw <- read_csv(
  file.path(RAW_DIR, "hacks.csv"),
  show_col_types = FALSE
) %>%
  clean_names()

# Create empty Date vector
hacks_dates <- rep(as.Date(NA), nrow(hacks_raw))

# Parse early rows as day-month-year
hacks_dates[hacks_raw$number < 365] <- dmy(
  hacks_raw$date[hacks_raw$number < 365]
)

# Parse later rows as month-day-year
hacks_dates[hacks_raw$number >= 365] <- mdy(
  hacks_raw$date[hacks_raw$number >= 365]
)

# Add parsed date and keep original date as date_raw
hacks <- hacks_raw %>%
  mutate(
    date_raw = date,
    date = hacks_dates
  ) %>%
  arrange(date)


sum(is.na(hacks$date))
range(hacks$date, na.rm = TRUE)

hacks %>%
  filter(number >= 360, number <= 370) %>%
  select(number, entity, date_raw, date)

saveRDS(hacks, file.path(PROCESSED_DIR, "hacks_step2_clean.rds"))


# ------------------------------------------------------------
# 2.4 Check whether German yield columns duplicate US columns
# ------------------------------------------------------------
# Your previous skim() output suggested that US and German yields may be identical.
# This check tells us whether de_2y/de_3m/de_10y are actually duplicates of
# us_2y/us_3m/us_10y.

same_numeric_with_na <- function(x, y, tolerance = 1e-12) {
  both_na <- is.na(x) & is.na(y)
  both_present_same <- !is.na(x) & !is.na(y) & abs(x - y) < tolerance
  
  all(both_na | both_present_same)
}

yield_duplicate_check <- tibble(
  pair = c(
    "us_2y vs de_2y",
    "us_3m vs de_3m",
    "us_10y vs de_10y"
  ),
  identical_or_nearly_identical = c(
    same_numeric_with_na(dt$us_2y, dt$de_2y),
    same_numeric_with_na(dt$us_3m, dt$de_3m),
    same_numeric_with_na(dt$us_10y, dt$de_10y)
  )
)

yield_duplicate_check


# ------------------------------------------------------------
# 2.5 Create US trading-day dataset 
# ------------------------------------------------------------
# For Fed models, use the US trading calendar.
# We anchor this by keeping rows where S&P 500 exists.

us_dt <- dt %>%
  filter(!is.na(sp500)) %>%
  arrange(date) %>%
  mutate(
    market_calendar = "US",
    trading_day_id = row_number(),
    
    # Crypto log prices 
    btc_log_price = log(btc_price),
    eth_log_price = log(eth_price),
    usdt_log_price = log(usdt_price),
    
    # Stock-market log prices
    sp500_log_price = log(sp500),
    stoxx50_log_price = log(stoxx50),
    
    # Crypto trading-day log returns, multiplied by 100
    # These are used for descriptive statistics and lagged return controls.
    btc_ret = 100 * (btc_log_price - lag(btc_log_price)),
    eth_ret = 100 * (eth_log_price - lag(eth_log_price)),
    usdt_ret = 100 * (usdt_log_price - lag(usdt_log_price)),
    
    # Stock-market trading-day log returns, multiplied by 100
    sp500_ret = 100 * (sp500_log_price - lag(sp500_log_price)),
    stoxx50_ret = 100 * (stoxx50_log_price - lag(stoxx50_log_price)),
    
    # US-specific financial-market controls
    dxy_ret = 100 * (log(dxy) - lag(log(dxy))),
    
    # VIX in log form, as described in the thesis
    vix_log = log(vix),
    
    # US yield spreads
    us_short_spread = us_2y - us_3m,
    us_long_spread = us_10y - us_2y,
    
    # Dummy variables
    covid_dummy = as.integer(date >= ymd("2019-12-31") & date <= ymd("2023-05-05")),
    mica_dummy = as.integer(date >= ymd("2024-06-30")),
    monday_dummy = as.integer(wday(date, week_start = 1) == 1),
    hack_dummy = as.integer(date %in% hacks$date)
  )


# ------------------------------------------------------------
# 2.6 Create EU trading-day dataset
# ------------------------------------------------------------
# For ECB models, use the EU trading calendar.
# We anchor this by keeping rows where STOXX50 exists.

eu_dt <- dt %>%
  filter(!is.na(stoxx50)) %>%
  arrange(date) %>%
  mutate(
    market_calendar = "EU",
    trading_day_id = row_number(),
    
    # Crypto log prices
    btc_log_price = log(btc_price),
    eth_log_price = log(eth_price),
    usdt_log_price = log(usdt_price),
    
    # Stock-market log prices
    sp500_log_price = log(sp500),
    stoxx50_log_price = log(stoxx50),
    
    # Crypto trading-day log returns, multiplied by 100
    # These are used for descriptive statistics and lagged return controls.
    btc_ret = 100 * (btc_log_price - lag(btc_log_price)),
    eth_ret = 100 * (eth_log_price - lag(eth_log_price)),
    usdt_ret = 100 * (usdt_log_price - lag(usdt_log_price)),
    
    # Stock-market trading-day log returns, multiplied by 100
    sp500_ret = 100 * (sp500_log_price - lag(sp500_log_price)),
    stoxx50_ret = 100 * (stoxx50_log_price - lag(stoxx50_log_price)),
    
    # EU-specific financial-market controls
    dxy_ret = 100 * (log(dxy) - lag(log(dxy))),
    
    # VIX in log form, as described in the thesis
    vix_log = log(vix),
    
    # German yield spreads
    de_short_spread = de_2y - de_3m,
    de_long_spread = de_10y - de_2y,
    
    # Dummy variables
    covid_dummy = as.integer(date >= ymd("2019-12-31") & date <= ymd("2023-05-05")),
    mica_dummy = as.integer(date >= ymd("2024-06-30")),
    monday_dummy = as.integer(wday(date, week_start = 1) == 1),
    hack_dummy = as.integer(date %in% hacks$date)
  )

# ------------------------------------------------------------
# 2.7 Standardize monetary policy shocks
# ------------------------------------------------------------
# Standardization uses the standard deviation of non-zero event shocks.
# This means a coefficient can be interpreted as the response to a
# one-standard-deviation surprise.

standardize_nonzero_shock <- function(x) {
  x / sd(x[x != 0], na.rm = TRUE)
}

us_dt <- us_dt %>%
  mutate(
    fed_mp_std = standardize_nonzero_shock(fed_mp),
    fed_cbi_std = standardize_nonzero_shock(fed_cbi)
  )

eu_dt <- eu_dt %>%
  mutate(
    ecb_mp_std = standardize_nonzero_shock(ecb_mp),
    ecb_cbi_std = standardize_nonzero_shock(ecb_cbi)
  )



# ------------------------------------------------------------
# 2.8 Confirm 25-trading-day horizon
# ------------------------------------------------------------

us_horizon_check <- us_dt %>%
  mutate(
    date_after_25_trading_days = lead(date, 25),
    calendar_days_between = as.integer(date_after_25_trading_days - date)
  ) %>%
  summarise(
    min_calendar_days = min(calendar_days_between, na.rm = TRUE),
    median_calendar_days = median(calendar_days_between, na.rm = TRUE),
    max_calendar_days = max(calendar_days_between, na.rm = TRUE)
  )

eu_horizon_check <- eu_dt %>%
  mutate(
    date_after_25_trading_days = lead(date, 25),
    calendar_days_between = as.integer(date_after_25_trading_days - date)
  ) %>%
  summarise(
    min_calendar_days = min(calendar_days_between, na.rm = TRUE),
    median_calendar_days = median(calendar_days_between, na.rm = TRUE),
    max_calendar_days = max(calendar_days_between, na.rm = TRUE)
  )

us_horizon_check
eu_horizon_check



# ------------------------------------------------------------
# 2.9 Shock-count checks after trading-day filtering
# ------------------------------------------------------------

us_shock_counts <- us_dt %>%
  summarise(
    fed_mp_nonzero = sum(fed_mp != 0, na.rm = TRUE),
    fed_cbi_nonzero = sum(fed_cbi != 0, na.rm = TRUE)
  )

eu_shock_counts <- eu_dt %>%
  summarise(
    ecb_mp_nonzero = sum(ecb_mp != 0, na.rm = TRUE),
    ecb_cbi_nonzero = sum(ecb_cbi != 0, na.rm = TRUE)
  )

us_shock_counts
eu_shock_counts

# ------------------------------------------------------------
# 2.10 Return summaries
# ------------------------------------------------------------

us_return_summary <- us_dt %>%
  summarise(
    n_rows = n(),
    first_date = min(date, na.rm = TRUE),
    last_date = max(date, na.rm = TRUE),
    
    btc_ret_missing = sum(is.na(btc_ret)),
    eth_ret_missing = sum(is.na(eth_ret)),
    usdt_ret_missing = sum(is.na(usdt_ret)),
    
    btc_ret_mean = mean(btc_ret, na.rm = TRUE),
    btc_ret_sd = sd(btc_ret, na.rm = TRUE),
    
    eth_ret_mean = mean(eth_ret, na.rm = TRUE),
    eth_ret_sd = sd(eth_ret, na.rm = TRUE),
    
    usdt_ret_mean = mean(usdt_ret, na.rm = TRUE),
    usdt_ret_sd = sd(usdt_ret, na.rm = TRUE)
  )

eu_return_summary <- eu_dt %>%
  summarise(
    n_rows = n(),
    first_date = min(date, na.rm = TRUE),
    last_date = max(date, na.rm = TRUE),
    
    btc_ret_missing = sum(is.na(btc_ret)),
    eth_ret_missing = sum(is.na(eth_ret)),
    usdt_ret_missing = sum(is.na(usdt_ret)),
    
    btc_ret_mean = mean(btc_ret, na.rm = TRUE),
    btc_ret_sd = sd(btc_ret, na.rm = TRUE),
    
    eth_ret_mean = mean(eth_ret, na.rm = TRUE),
    eth_ret_sd = sd(eth_ret, na.rm = TRUE),
    
    usdt_ret_mean = mean(usdt_ret, na.rm = TRUE),
    usdt_ret_sd = sd(usdt_ret, na.rm = TRUE)
  )

us_return_summary
eu_return_summary


# # ------------------------------------------------------------
# # 2.11 Quick return plots
# # ------------------------------------------------------------
# 
# ggplot(us_dt, aes(x = date, y = btc_ret)) +
#   geom_line() +
#   labs(
#     title = "Bitcoin trading-day log returns on US trading calendar",
#     x = NULL,
#     y = "BTC return (%)"
#   ) +
#   theme_minimal()
# 
# ggplot(us_dt, aes(x = date, y = eth_ret)) +
#   geom_line() +
#   labs(
#     title = "Ethereum trading-day log returns on US trading calendar",
#     x = NULL,
#     y = "ETH return (%)"
#   ) +
#   theme_minimal()
# 
# ggplot(us_dt, aes(x = date, y = usdt_ret)) +
#   geom_line() +
#   labs(
#     title = "Tether trading-day log returns on US trading calendar",
#     x = NULL,
#     y = "USDT return (%)"
#   ) +
#   theme_minimal()



# ------------------------------------------------------------
# 2.12 Save prepared Step 2 datasets
# ------------------------------------------------------------

saveRDS(dt, file.path(PROCESSED_DIR, "merged_step2_clean.rds"))
saveRDS(us_dt, file.path(PROCESSED_DIR, "us_trading_step2.rds"))
saveRDS(eu_dt, file.path(PROCESSED_DIR, "eu_trading_step2.rds"))

saveRDS(hacks, file.path(PROCESSED_DIR, "hacks_step2_clean.rds"))



