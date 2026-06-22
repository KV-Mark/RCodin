# ============================================================
# Step 6: Descriptive statistics and data-summary outputs
# Purpose:
# 1. Create updated descriptive statistics tables
# 2. Create cumulative log-return figure
# 3. Create return-distribution plots
# 4. Create control-variable summaries
# 5. Create monetary-policy shock summaries
# 6. Create event-count tables
# 7. Create correlation matrix
# ============================================================

source(file.path(path.expand("~"), "Documents", "Timi", "RThesis", "RCodin", "R", "0_setup.R"))
# source(file.path(path.expand("~"), "Downloads", "RThesis", "R", "0_setup.R"))

# ------------------------------------------------------------
# 6.1 Load prepared data
# ------------------------------------------------------------

merged_dt <- readRDS(file.path(PROCESSED_DIR, "merged_step2_clean.rds"))
us_dt <- readRDS(file.path(PROCESSED_DIR, "us_trading_step2.rds"))
eu_dt <- readRDS(file.path(PROCESSED_DIR, "eu_trading_step2.rds"))
hacks <- readRDS(file.path(PROCESSED_DIR, "hacks_step2_clean.rds"))

# Basic checks
range(merged_dt$date, na.rm = TRUE)
range(us_dt$date, na.rm = TRUE)
range(eu_dt$date, na.rm = TRUE)
range(hacks$date, na.rm = TRUE)


# ------------------------------------------------------------
# 6.2 Helper functions
# ------------------------------------------------------------

skewness_raw <- function(x) {
  x <- x[!is.na(x)]
  
  if (length(x) < 3 || sd(x) == 0) {
    return(NA_real_)
  }
  
  mean((x - mean(x))^3) / sd(x)^3
}

kurtosis_raw <- function(x) {
  x <- x[!is.na(x)]
  
  if (length(x) < 4 || sd(x) == 0) {
    return(NA_real_)
  }
  
  mean((x - mean(x))^4) / sd(x)^4
}

summary_from_vector <- function(x, variable_name) {
  x <- x[!is.na(x)]
  
  tibble(
    variable = variable_name,
    n = length(x),
    mean = mean(x),
    median = median(x),
    sd = sd(x),
    min = min(x),
    max = max(x),
    skewness = skewness_raw(x),
    kurtosis = kurtosis_raw(x)
  )
}

summary_from_named_vectors <- function(named_vectors) {
  purrr::imap_dfr(
    named_vectors,
    ~ summary_from_vector(.x, .y)
  )
}

first_non_missing <- function(x) {
  x[which(!is.na(x))[1]]
}

cumulative_log_return_from_price <- function(price) {
  base_price <- first_non_missing(price)
  100 * (log(price) - log(base_price))
}



# ------------------------------------------------------------
# 6.3 Create weekday descriptive dataset
# ------------------------------------------------------------
# The thesis descriptive section says the sample is restricted to trading days.
# Here we use Monday-Friday rows for descriptive tables and figures.

desc_dt <- merged_dt %>%
  arrange(date) %>%
  filter(wday(date, week_start = 1) <= 5) %>%
  mutate(
    # Log prices
    btc_log_price = log(btc_price),
    eth_log_price = log(eth_price),
    usdt_log_price = log(usdt_price),
    
    # Daily log returns, in percent
    btc_ret = 100 * (btc_log_price - lag(btc_log_price)),
    eth_ret = 100 * (eth_log_price - lag(eth_log_price)),
    usdt_ret = 100 * (usdt_log_price - lag(usdt_log_price)),
    sp500_ret = 100 * (log(sp500) - lag(log(sp500))),
    stoxx50_ret = 100 * (log(stoxx50) - lag(log(stoxx50))),
    dxy_ret = 100 * (log(dxy) - lag(log(dxy))),
    
    # Control transformations
    vix_log = log(vix),
    
    us_short_spread = us_2y - us_3m,
    us_long_spread = us_10y - us_2y,
    de_short_spread = de_2y - de_3m,
    de_long_spread = de_10y - de_2y,
    
    covid_dummy = as.integer(date >= ymd("2019-12-31") & date <= ymd("2023-05-05")),
    mica_dummy = as.integer(date >= ymd("2024-06-30")),
    monday_dummy = as.integer(wday(date, week_start = 1) == 1),
    hack_dummy = as.integer(date %in% hacks$date)
  )

# Check date range and number of rows
range(desc_dt$date, na.rm = TRUE)
nrow(desc_dt)


# ------------------------------------------------------------
# 6.4 Return summary statistics
# ------------------------------------------------------------

return_summary <- summary_from_named_vectors(
  list(
    "BTC return" = desc_dt$btc_ret,
    "ETH return" = desc_dt$eth_ret,
    "USDT return" = desc_dt$usdt_ret,
    "S&P 500 return" = desc_dt$sp500_ret,
    "STOXX50 return" = desc_dt$stoxx50_ret
  )
)

return_summary

return_summary_rounded <- return_summary %>%
  mutate(
    across(
      .cols = where(is.numeric),
      .fns = ~ round(.x, 4)
    )
  )

return_summary_rounded

# write_csv(
#   return_summary,
#   file.path(TABLE_DIR, "return_summary_statistics_step6.csv")
# )

write_csv(
  return_summary_rounded,
  file.path(TABLE_DIR, "return_summary_statistics_rounded_step6.csv")
)


# ------------------------------------------------------------
# 6.5 Cumulative log-return figure
# ------------------------------------------------------------

cumulative_return_plot_data <- desc_dt %>%
  transmute(
    date = date,
    BTC = cumulative_log_return_from_price(btc_price),
    ETH = cumulative_log_return_from_price(eth_price),
    USDT = cumulative_log_return_from_price(usdt_price),
    `S&P 500` = cumulative_log_return_from_price(sp500),
    STOXX50 = cumulative_log_return_from_price(stoxx50)
  ) %>%
  pivot_longer(
    cols = -date,
    names_to = "asset",
    values_to = "cumulative_log_return"
  )

cumulative_return_plot <- ggplot(
  cumulative_return_plot_data,
  aes(x = date, y = cumulative_log_return, linetype = asset, shape = asset)
) +
  geom_line(linewidth = 0.5) +
  labs(
    title = "Cumulative log returns: cryptocurrencies and equity indices",
    x = NULL,
    y = "Cumulative log return (%)",
    linetype = NULL,
    shape = NULL
  ) +
  theme_minimal()

cumulative_return_plot

ggsave(
  filename = file.path(FIGURE_DIR, "figure1_cumulative_log_returns_step6.png"),
  plot = cumulative_return_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# write_csv(
#   cumulative_return_plot_data,
#   file.path(TABLE_DIR, "figure1_cumulative_log_returns_data_step6.csv")
# )



# ------------------------------------------------------------
# 6.6 Frequency distribution of daily returns
# ------------------------------------------------------------

return_distribution_data <- desc_dt %>%
  transmute(
    BTC = btc_ret,
    ETH = eth_ret,
    USDT = usdt_ret,
    `S&P 500` = sp500_ret,
    STOXX50 = stoxx50_ret
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "asset",
    values_to = "daily_return"
  ) %>%
  filter(!is.na(daily_return))

return_distribution_plot <- ggplot(
  return_distribution_data,
  aes(x = daily_return)
) +
  geom_histogram(bins = 60) +
  facet_wrap(~ asset, scales = "free") +
  labs(
    title = "Frequency distribution of daily returns",
    x = "Daily log return (%)",
    y = "Frequency"
  ) +
  theme_minimal()

return_distribution_plot

ggsave(
  filename = file.path(FIGURE_DIR, "daily_return_distributions_step6.png"),
  plot = return_distribution_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# write_csv(
#   return_distribution_data,
#   file.path(TABLE_DIR, "daily_return_distribution_data_step6.csv")
# )



# ------------------------------------------------------------
# 6.7 Control-variable summary statistics
# ------------------------------------------------------------

control_summary <- summary_from_named_vectors(
  list(
    "VIX" = desc_dt$vix,
    "VIX log" = desc_dt$vix_log,
    "DXY" = desc_dt$dxy,
    "DXY return" = desc_dt$dxy_ret,
    "US 2Y-3M short spread" = desc_dt$us_short_spread,
    "US 10Y-2Y long spread" = desc_dt$us_long_spread,
    "DE 2Y-3M short spread" = desc_dt$de_short_spread,
    "DE 10Y-2Y long spread" = desc_dt$de_long_spread
  )
)

control_summary

control_summary_rounded <- control_summary %>%
  mutate(
    across(
      .cols = where(is.numeric),
      .fns = ~ round(.x, 4)
    )
  )

control_summary_rounded

# write_csv(
#   control_summary,
#   file.path(TABLE_DIR, "control_summary_statistics_step6.csv")
# )

write_csv(
  control_summary_rounded,
  file.path(TABLE_DIR, "control_summary_statistics_rounded_step6.csv")
)


if (FALSE) {
# ------------------------------------------------------------
# 6.8 Dummy-variable counts
# ------------------------------------------------------------

dummy_summary <- desc_dt %>%
  summarise(
    n_rows = n(),
    covid_days = sum(covid_dummy == 1, na.rm = TRUE),
    mica_days = sum(mica_dummy == 1, na.rm = TRUE),
    monday_days = sum(monday_dummy == 1, na.rm = TRUE),
    hack_days = sum(hack_dummy == 1, na.rm = TRUE)
  )

dummy_summary

write_csv(
  dummy_summary,
  file.path(TABLE_DIR, "dummy_summary_step6.csv")
)

}

# ------------------------------------------------------------
# 6.9 Monetary-policy shock summary statistics
# ------------------------------------------------------------

make_shock_summary <- function(data, central_bank_name, shock_type_name, shock_var, event_var) {
  
  event_data <- data %>%
    filter(!is.na(.data[[event_var]]))
  
  all_event_shocks <- event_data[[shock_var]]
  nonzero_event_shocks <- all_event_shocks[!is.na(all_event_shocks) & all_event_shocks != 0]
  
  bind_rows(
    summary_from_vector(
      all_event_shocks,
      paste0(central_bank_name, " ", shock_type_name, " shock (all meetings)")
    ),
    summary_from_vector(
      nonzero_event_shocks,
      paste0(central_bank_name, " ", shock_type_name, " shock (non-zero only)")
    )
  ) %>%
    mutate(
      central_bank = central_bank_name,
      shock_type = shock_type_name,
      shock_var = shock_var,
      .before = variable
    )
}

shock_summary_all <- bind_rows(
  make_shock_summary(
    data = us_dt,
    central_bank_name = "Fed",
    shock_type_name = "MP",
    shock_var = "fed_mp",
    event_var = "fed_pc1"
  ),
  make_shock_summary(
    data = us_dt,
    central_bank_name = "Fed",
    shock_type_name = "CBI",
    shock_var = "fed_cbi",
    event_var = "fed_pc1"
  ),
  make_shock_summary(
    data = eu_dt,
    central_bank_name = "ECB",
    shock_type_name = "MP",
    shock_var = "ecb_mp",
    event_var = "ecb_pc1"
  ),
  make_shock_summary(
    data = eu_dt,
    central_bank_name = "ECB",
    shock_type_name = "CBI",
    shock_var = "ecb_cbi",
    event_var = "ecb_pc1"
  )
)

shock_summary_all

shock_summary_all_rounded <- shock_summary_all %>%
  mutate(
    across(
      .cols = where(is.numeric),
      .fns = ~ round(.x, 4)
    )
  )

shock_summary_mp <- shock_summary_all %>%
  filter(shock_type == "MP")

shock_summary_cbi <- shock_summary_all %>%
  filter(shock_type == "CBI")

# write_csv(
#   shock_summary_all,
#   file.path(TABLE_DIR, "shock_summary_all_step6.csv")
# )

write_csv(
  shock_summary_all_rounded,
  file.path(TABLE_DIR, "shock_summary_all_rounded_step6.csv")
)

# write_csv(
#   shock_summary_mp,
#   file.path(TABLE_DIR, "shock_summary_mp_step6.csv")
# )
# 
# write_csv(
#   shock_summary_cbi,
#   file.path(TABLE_DIR, "shock_summary_cbi_step6.csv")
# )



# ------------------------------------------------------------
# 6.10 Event-count tables
# ------------------------------------------------------------

make_event_count <- function(data, central_bank_name, shock_type_name, shock_var, event_var) {
  
  event_data <- data %>%
    filter(!is.na(.data[[event_var]]))
  
  x <- event_data[[shock_var]]
  
  tibble(
    central_bank = central_bank_name,
    shock_type = shock_type_name,
    sample_start = min(event_data$date, na.rm = TRUE),
    sample_end = max(event_data$date, na.rm = TRUE),
    total_meetings = nrow(event_data),
    nonzero_shock_meetings = sum(x != 0, na.rm = TRUE),
    tightening_surprises_positive = sum(x > 0, na.rm = TRUE),
    easing_surprises_negative = sum(x < 0, na.rm = TRUE),
    fully_anticipated_zero = sum(x == 0, na.rm = TRUE)
  )
}

event_counts_all <- bind_rows(
  make_event_count(
    data = us_dt,
    central_bank_name = "Fed",
    shock_type_name = "MP",
    shock_var = "fed_mp",
    event_var = "fed_pc1"
  ),
  make_event_count(
    data = us_dt,
    central_bank_name = "Fed",
    shock_type_name = "CBI",
    shock_var = "fed_cbi",
    event_var = "fed_pc1"
  ),
  make_event_count(
    data = eu_dt,
    central_bank_name = "ECB",
    shock_type_name = "MP",
    shock_var = "ecb_mp",
    event_var = "ecb_pc1"
  ),
  make_event_count(
    data = eu_dt,
    central_bank_name = "ECB",
    shock_type_name = "CBI",
    shock_var = "ecb_cbi",
    event_var = "ecb_pc1"
  )
)

event_counts_all

event_counts_mp <- event_counts_all %>%
  filter(shock_type == "MP")

event_counts_cbi <- event_counts_all %>%
  filter(shock_type == "CBI")

# write_csv(
#   event_counts_all,
#   file.path(TABLE_DIR, "event_counts_all_step6.csv")
# )

write_csv(
  event_counts_mp,
  file.path(TABLE_DIR, "event_counts_mp_step6.csv")
)

write_csv(
  event_counts_cbi,
  file.path(TABLE_DIR, "event_counts_cbi_step6.csv")
)


if (FALSE) {
# ------------------------------------------------------------
# 6.11 Event weekday distribution
# ------------------------------------------------------------

fed_event_weekdays <- us_dt %>%
  filter(!is.na(fed_pc1)) %>%
  mutate(
    central_bank = "Fed",
    weekday = wday(date, label = TRUE, week_start = 1)
  ) %>%
  count(central_bank, weekday)

ecb_event_weekdays <- eu_dt %>%
  filter(!is.na(ecb_pc1)) %>%
  mutate(
    central_bank = "ECB",
    weekday = wday(date, label = TRUE, week_start = 1)
  ) %>%
  count(central_bank, weekday)

event_weekday_distribution <- bind_rows(
  fed_event_weekdays,
  ecb_event_weekdays
)

event_weekday_distribution

write_csv(
  event_weekday_distribution,
  file.path(TABLE_DIR, "event_weekday_distribution_step6.csv")
)
}


# ------------------------------------------------------------
# 6.12 Correlation matrix
# ------------------------------------------------------------

correlation_data <- desc_dt %>%
  transmute(
    BTC = btc_ret,
    ETH = eth_ret,
    USDT = usdt_ret,
    `S&P 500` = sp500_ret,
    STOXX50 = stoxx50_ret,
    DXY = dxy_ret,
    `VIX log` = vix_log,
    `US 2Y-3M` = us_short_spread,
    `US 10Y-2Y` = us_long_spread,
    `DE 2Y-3M` = de_short_spread,
    `DE 10Y-2Y` = de_long_spread
  )

correlation_data_common <- correlation_data %>%
  drop_na()

correlation_matrix_common <- cor(
  correlation_data_common,
  use = "complete.obs"
)

correlation_matrix_pairwise <- cor(
  correlation_data,
  use = "pairwise.complete.obs"
)

correlation_matrix_common
correlation_matrix_pairwise

correlation_matrix_common_csv <- as.data.frame(correlation_matrix_common) %>%
  rownames_to_column("variable")

correlation_matrix_pairwise_csv <- as.data.frame(correlation_matrix_pairwise) %>%
  rownames_to_column("variable")

# write_csv(
#   correlation_matrix_common_csv,
#   file.path(TABLE_DIR, "correlation_matrix_common_sample_step6.csv")
# )

write_csv(
  correlation_matrix_pairwise_csv,
  file.path(TABLE_DIR, "correlation_matrix_pairwise_step6.csv")
)


if (FALSE) {
# ------------------------------------------------------------
# 6.13 Compact output checklist
# ------------------------------------------------------------

step6_outputs <- tibble(
  output = c(
    "return_summary_statistics_step6.csv",
    "return_summary_statistics_rounded_step6.csv",
    "figure1_cumulative_log_returns_step6.png",
    "daily_return_distributions_step6.png",
    "control_summary_statistics_step6.csv",
    "dummy_summary_step6.csv",
    "shock_summary_all_step6.csv",
    "shock_summary_mp_step6.csv",
    "shock_summary_cbi_step6.csv",
    "event_counts_all_step6.csv",
    "event_counts_mp_step6.csv",
    "event_counts_cbi_step6.csv",
    "event_weekday_distribution_step6.csv",
    "correlation_matrix_common_sample_step6.csv",
    "correlation_matrix_pairwise_step6.csv"
  ),
  location = c(
    rep(TABLE_DIR, 2),
    FIGURE_DIR,
    FIGURE_DIR,
    rep(TABLE_DIR, 11)
  ),
  exists = file.exists(file.path(location, output))
)

step6_outputs

write_csv(
  step6_outputs,
  file.path(TABLE_DIR, "step6_output_checklist.csv")
)
}

return_summary_rounded
control_summary_rounded
shock_summary_all_rounded
event_counts_all



