# ==============================================================================
# 6_descriptive_tables.R
# Thesis project: The Effect of Monetary Policy Announcements on Cryptocurrency Returns
#
# Purpose:
#   - Generate the required descriptive/statistical thesis tables
#   - Save tables as CSV files for MS Word
#   - Use clean variable labels, uniform notation, and 5% / 95% convention
#
# Inputs:
#   data/processed/02_clean_daily_data.csv
#   data/processed/03_model_sample_ecb.csv
#   data/processed/03_model_sample_fed.csv
#
# Outputs:
#   output/tables/table_01_descriptive_statistics_all_variables.csv
#   output/tables/table_03_crypto_stock_return_summary.csv
#   output/tables/table_04_mp_surprise_summary_statistics.csv
#   output/tables/table_05_control_variable_correlation_matrix.csv
#   output/tables/table_11_event_counts_shock_direction_breakdown.csv
#   output/tables/table_12_announcement_timing_summary.csv
#   output/tables/table_notes_06_descriptive_tables.csv
# ==============================================================================


# ------------------------------------------------------------------------------
# 6.0 Load setup
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
# 6.1 Read required processed data
# ------------------------------------------------------------------------------

clean_data_file <- file.path(PATHS$data_processed, "02_clean_daily_data.csv")
ecb_sample_file <- file.path(PATHS$data_processed, "03_model_sample_ecb.csv")
fed_sample_file <- file.path(PATHS$data_processed, "03_model_sample_fed.csv")

if (!file.exists(clean_data_file)) {
  stop(
    "Could not find cleaned daily data: ",
    clean_data_file,
    "\nRun source('RCodin/R/2_clean_prepare.R') first.",
    call. = FALSE
  )
}

if (!file.exists(ecb_sample_file)) {
  stop(
    "Could not find ECB model sample: ",
    ecb_sample_file,
    "\nRun source('RCodin/R/3_make_model_samples.R') first.",
    call. = FALSE
  )
}

if (!file.exists(fed_sample_file)) {
  stop(
    "Could not find Fed model sample: ",
    fed_sample_file,
    "\nRun source('RCodin/R/3_make_model_samples.R') first.",
    call. = FALSE
  )
}

clean_data <- safe_read_csv(clean_data_file) %>%
  mutate(date = as.Date(.data$date))

ecb_sample <- safe_read_csv(ecb_sample_file) %>%
  mutate(date = as.Date(.data$date))

fed_sample <- safe_read_csv(fed_sample_file) %>%
  mutate(date = as.Date(.data$date))

message("Clean daily data loaded. Rows: ", nrow(clean_data))
message("ECB model sample loaded. Rows: ", nrow(ecb_sample))
message("Fed model sample loaded. Rows: ", nrow(fed_sample))


# ------------------------------------------------------------------------------
# 6.2 Helper functions
# ------------------------------------------------------------------------------

VAR_LABELS_STEP6 <- c(
  VAR_LABELS,
  ecb_mp_observed = "ECB monetary policy surprise, nonzero observations only",
  fed_mp_observed = "Fed monetary policy surprise, nonzero observations only"
)

label_variable_step6 <- function(x) {
  labels <- VAR_LABELS_STEP6[x]
  ifelse(is.na(labels), x, labels)
}

safe_mean <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  mean(x)
}

safe_sd <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) <= 1) return(NA_real_)
  stats::sd(x)
}

safe_min <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  min(x)
}

safe_max <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  max(x)
}

safe_median <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  stats::median(x)
}

safe_skewness <- function(x) {
  x <- x[!is.na(x)]
  
  if (length(x) < 3) {
    return(NA_real_)
  }
  
  if (is.na(stats::sd(x)) || stats::sd(x) == 0) {
    return(NA_real_)
  }
  
  moments::skewness(x)
}

summary_stats_for_variables <- function(
    data,
    variables,
    sample_scope,
    nonzero_only_vars = character(0)
) {
  variables_present <- intersect(variables, names(data))
  variables_missing <- setdiff(variables, names(data))
  
  if (length(variables_missing) > 0) {
    warning(
      "The following variables were not found and will be skipped: ",
      paste(variables_missing, collapse = ", "),
      call. = FALSE
    )
  }
  
  purrr::map_dfr(
    variables_present,
    function(v) {
      x_raw <- suppressWarnings(as.numeric(data[[v]]))
      
      if (v %in% nonzero_only_vars) {
        x <- x_raw[!is.na(x_raw) & x_raw != 0]
        row_sample_scope <- paste0(sample_scope, "; nonzero observations only")
      } else {
        x <- x_raw
        row_sample_scope <- sample_scope
      }
      
      tibble::tibble(
        sample_scope = row_sample_scope,
        variable = v,
        label = label_variable_step6(v),
        N = sum(!is.na(x)),
        mean = safe_mean(x),
        sd = safe_sd(x),
        min = safe_min(x),
        max = safe_max(x),
        skewness = safe_skewness(x)
      )
    }
  )
}

make_meeting_events <- function(
    model_sample,
    central_bank,
    shock_var,
    observed_shock_var,
    meeting_var
) {
  check_required_columns(
    model_sample,
    c("date", "trading_day_index", shock_var, observed_shock_var, meeting_var),
    data_name = paste0(central_bank, " model sample")
  )
  
  weekday_labels <- c(
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday",
    "Sunday"
  )
  
  model_sample %>%
    filter(.data[[meeting_var]] == 1) %>%
    transmute(
      central_bank = central_bank,
      date = as.Date(.data$date),
      trading_day_index = .data$trading_day_index,
      shock_variable = shock_var,
      observed_shock_variable = observed_shock_var,
      shock_value = .data[[observed_shock_var]],
      weekday_num = lubridate::wday(.data$date, week_start = 1),
      weekday = weekday_labels[weekday_num]
    )
}

count_shock_directions <- function(x) {
  tibble::tibble(
    negative_less_than_zero = sum(x < 0, na.rm = TRUE),
    zero_equal_zero = sum(x == 0, na.rm = TRUE),
    positive_greater_than_zero = sum(x > 0, na.rm = TRUE),
    nonzero_total = sum(x != 0, na.rm = TRUE)
  )
}


# ------------------------------------------------------------------------------
# 6.3 Table 1: Descriptive statistics for all variables
# ------------------------------------------------------------------------------

table_01_base_vars <- unique(c(
  CRYPTO_RETURN_VARS,
  STOCK_RETURN_VARS,
  "dxy_log_return",
  "us_short",
  "us_long",
  "us_3m",
  "de_short",
  "de_long",
  "de_3m",
  "covid_dummy",
  "mica_dummy",
  "ecb_mp_observed",
  "fed_mp_observed"
))

table_01_clean_data <- summary_stats_for_variables(
  data = clean_data,
  variables = table_01_base_vars,
  sample_scope = "Full cleaned daily dataset",
  nonzero_only_vars = c("ecb_mp_observed", "fed_mp_observed")
)

table_01_crypto_lag_controls <- bind_rows(
  summary_stats_for_variables(
    data = ecb_sample,
    variables = CRYPTO_5D_LAG_RETURN_VARS,
    sample_scope = "ECB trading-day model sample; lagged 5-trading-day crypto controls"
  ),
  summary_stats_for_variables(
    data = fed_sample,
    variables = CRYPTO_5D_LAG_RETURN_VARS,
    sample_scope = "Fed trading-day model sample; lagged 5-trading-day crypto controls"
  )
)

table_01 <- bind_rows(
  table_01_clean_data,
  table_01_crypto_lag_controls
)

write_table_csv(
  table_01,
  "table_01_descriptive_statistics_all_variables.csv",
  digits = TABLE_DIGITS
)


# ------------------------------------------------------------------------------
# 6.4 Table 3: Cryptocurrency and stock returns summary statistics
# ------------------------------------------------------------------------------

table_03_vars <- unique(c(
  CRYPTO_RETURN_VARS,
  STOCK_RETURN_VARS
))

table_03 <- summary_stats_for_variables(
  data = clean_data,
  variables = table_03_vars,
  sample_scope = "Full cleaned daily dataset"
) %>%
  mutate(
    asset_class = case_when(
      variable %in% CRYPTO_RETURN_VARS ~ "Cryptocurrency",
      variable %in% STOCK_RETURN_VARS ~ "Stock index",
      TRUE ~ "Other"
    )
  ) %>%
  select(
    asset_class,
    variable,
    label,
    N,
    mean,
    sd,
    min,
    max,
    skewness
  )

write_table_csv(
  table_03,
  "table_03_crypto_stock_return_summary.csv",
  digits = TABLE_DIGITS
)


# ------------------------------------------------------------------------------
# 6.5 Monetary policy announcement event data
# ------------------------------------------------------------------------------

ecb_events <- make_meeting_events(
  model_sample = ecb_sample,
  central_bank = "ECB",
  shock_var = "ecb_mp",
  observed_shock_var = "ecb_mp_observed",
  meeting_var = "ecb_meeting"
)

fed_events <- make_meeting_events(
  model_sample = fed_sample,
  central_bank = "Fed",
  shock_var = "fed_mp",
  observed_shock_var = "fed_mp_observed",
  meeting_var = "fed_meeting"
)

mp_events <- bind_rows(
  ecb_events,
  fed_events
)

if (nrow(mp_events) == 0) {
  stop(
    "No monetary policy announcement events found in the model samples.",
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 6.6 Table 4: Monetary Policy Surprises summary statistics
# ------------------------------------------------------------------------------

table_04 <- mp_events %>%
  group_by(.data$central_bank, .data$shock_variable, .data$observed_shock_variable) %>%
  summarise(
    N = sum(!is.na(.data$shock_value)),
    mean = safe_mean(.data$shock_value),
    sd = safe_sd(.data$shock_value),
    min = safe_min(.data$shock_value),
    max = safe_max(.data$shock_value),
    skewness = safe_skewness(.data$shock_value),
    .groups = "drop"
  ) %>%
  mutate(
    shock_name = case_when(
      central_bank == "ECB" ~ "ECB monetary policy surprise",
      central_bank == "Fed" ~ "Fed monetary policy surprise",
      TRUE ~ shock_variable
    ),
    sample_scope = "Announcement days retained in the central-bank-specific model sample"
  ) %>%
  select(
    central_bank,
    shock_name,
    shock_variable,
    observed_shock_variable,
    sample_scope,
    N,
    mean,
    sd,
    min,
    max,
    skewness
  )

write_table_csv(
  table_04,
  "table_04_mp_surprise_summary_statistics.csv",
  digits = TABLE_DIGITS
)


# ------------------------------------------------------------------------------
# 6.7 Table 5: Correlation matrix for control variables
# ------------------------------------------------------------------------------

make_control_correlation_table <- function(data, control_vars, central_bank) {
  vars_present <- intersect(control_vars, names(data))
  
  if (length(vars_present) == 0) {
    return(tibble::tibble())
  }
  
  control_correlation_data <- data %>%
    select(all_of(vars_present))
  
  correlation_matrix <- stats::cor(
    control_correlation_data,
    use = "pairwise.complete.obs"
  )
  
  out <- as.data.frame(correlation_matrix) %>%
    tibble::rownames_to_column(var = "variable") %>%
    mutate(
      central_bank = central_bank,
      .before = "variable"
    )
  
  out$variable <- label_variable_step6(out$variable)
  
  colnames(out) <- c(
    "central_bank",
    "variable",
    label_variable_step6(colnames(correlation_matrix))
  )
  
  out
}

table_05 <- bind_rows(
  make_control_correlation_table(
    data = ecb_sample,
    control_vars = ECB_BASELINE_CONTROL_VARS,
    central_bank = "ECB"
  ),
  make_control_correlation_table(
    data = fed_sample,
    control_vars = FED_BASELINE_CONTROL_VARS,
    central_bank = "Fed"
  )
)

write_table_csv(
  table_05,
  "table_05_control_variable_correlation_matrix.csv",
  digits = TABLE_DIGITS
)


# ------------------------------------------------------------------------------
# 6.8 Table 11: Event counts and shock-direction breakdown
# ------------------------------------------------------------------------------

ecb_sample_counts <- tibble::tibble(
  central_bank = "ECB",
  sample_rows = nrow(ecb_sample),
  non_announcement_days = sum(ecb_sample$ecb_meeting == 0, na.rm = TRUE)
)

fed_sample_counts <- tibble::tibble(
  central_bank = "Fed",
  sample_rows = nrow(fed_sample),
  non_announcement_days = sum(fed_sample$fed_meeting == 0, na.rm = TRUE)
)

sample_counts <- bind_rows(
  ecb_sample_counts,
  fed_sample_counts
)

table_11 <- mp_events %>%
  group_by(.data$central_bank, .data$shock_variable) %>%
  summarise(
    meetings_total = n(),
    negative_less_than_zero = sum(.data$shock_value < 0, na.rm = TRUE),
    zero_equal_zero = sum(.data$shock_value == 0, na.rm = TRUE),
    positive_greater_than_zero = sum(.data$shock_value > 0, na.rm = TRUE),
    nonzero_total = sum(.data$shock_value != 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(sample_counts, by = "central_bank") %>%
  mutate(
    share_negative = negative_less_than_zero / meetings_total,
    share_zero = zero_equal_zero / meetings_total,
    share_positive = positive_greater_than_zero / meetings_total,
    share_nonzero = nonzero_total / meetings_total
  ) %>%
  select(
    central_bank,
    shock_variable,
    sample_rows,
    meetings_total,
    non_announcement_days,
    negative_less_than_zero,
    zero_equal_zero,
    positive_greater_than_zero,
    nonzero_total,
    share_negative,
    share_zero,
    share_positive,
    share_nonzero
  )

write_table_csv(
  table_11,
  "table_11_event_counts_shock_direction_breakdown.csv",
  digits = TABLE_DIGITS
)


# ------------------------------------------------------------------------------
# 6.9 Table 12: Announcement weekday and trading-day gap summary
# ------------------------------------------------------------------------------

weekday_labels <- c(
  "Monday",
  "Tuesday",
  "Wednesday",
  "Thursday",
  "Friday",
  "Saturday",
  "Sunday"
)

weekday_base <- tidyr::expand_grid(
  central_bank = c("ECB", "Fed"),
  weekday_num = 1:7
) %>%
  mutate(
    weekday = weekday_labels[weekday_num]
  )

weekday_counts <- mp_events %>%
  count(
    .data$central_bank,
    .data$weekday_num,
    name = "announcements"
  )

gap_stats <- mp_events %>%
  arrange(.data$central_bank, .data$trading_day_index) %>%
  group_by(.data$central_bank) %>%
  mutate(
    trading_day_gap = .data$trading_day_index - dplyr::lag(.data$trading_day_index)
  ) %>%
  summarise(
    total_announcements = n(),
    mean_trading_days_between_announcements = safe_mean(.data$trading_day_gap),
    median_trading_days_between_announcements = safe_median(.data$trading_day_gap),
    min_trading_days_between_announcements = safe_min(.data$trading_day_gap),
    max_trading_days_between_announcements = safe_max(.data$trading_day_gap),
    .groups = "drop"
  )

table_12 <- weekday_base %>%
  left_join(
    weekday_counts,
    by = c("central_bank", "weekday_num")
  ) %>%
  mutate(
    announcements = tidyr::replace_na(.data$announcements, 0L)
  ) %>%
  left_join(
    gap_stats,
    by = "central_bank"
  ) %>%
  mutate(
    share_of_announcements = ifelse(
      .data$total_announcements > 0,
      .data$announcements / .data$total_announcements,
      NA_real_
    )
  ) %>%
  select(
    central_bank,
    weekday,
    announcements,
    share_of_announcements,
    total_announcements,
    mean_trading_days_between_announcements,
    median_trading_days_between_announcements,
    min_trading_days_between_announcements,
    max_trading_days_between_announcements
  ) %>%
  arrange(
    .data$central_bank,
    match(.data$weekday, weekday_labels)
  )

write_table_csv(
  table_12,
  "table_12_announcement_timing_summary.csv",
  digits = TABLE_DIGITS
)


# ------------------------------------------------------------------------------
# 6.10 Table notes for Word
# ------------------------------------------------------------------------------

table_notes <- tibble::tibble(
  table_file = c(
    "table_01_descriptive_statistics_all_variables.csv",
    "table_03_crypto_stock_return_summary.csv",
    "table_04_mp_surprise_summary_statistics.csv",
    "table_05_control_variable_correlation_matrix.csv",
    "table_11_event_counts_shock_direction_breakdown.csv",
    "table_12_announcement_timing_summary.csv"
  ),
  notes = c(
    "Notes: This table reports descriptive statistics for the cleaned daily dataset and the lagged crypto trend controls used in the ECB and Fed model samples. N is the number of non-missing observations. Return variables are log returns. The lagged 5-trading-day crypto controls are computed as the sum of the previous five crypto daily log returns within each central-bank-specific trading-day sample. For ECB and Fed monetary policy surprise variables, statistics are computed using nonzero observed surprise values only.",
    "Notes: This table reports summary statistics for cryptocurrency and stock-index daily log returns. N is the number of non-missing observations. Statistics are based on the cleaned daily dataset.",
    "Notes: This table reports summary statistics for monetary policy surprises on announcement days retained in the central-bank-specific model samples. ECB and Fed samples are based on their respective trading-day filters.",
    "Notes: This table reports pairwise correlations among control variables used in the ECB and Fed baseline models. The baseline controls include the lagged 5-trading-day cryptocurrency return controls. Correlations are computed using pairwise complete observations within each central-bank-specific model sample.",
    "Notes: This table reports the number of monetary policy announcement days and the direction of monetary policy surprises in the model samples. Negative, zero, and positive shocks refer to observed monetary policy surprise values on announcement days.",
    "Notes: This table reports the weekday distribution of monetary policy announcements and the average number of trading days between consecutive announcements within each central-bank-specific model sample."
  )
)

readr::write_csv(
  table_notes,
  file.path(PATHS$tables, "table_notes_06_descriptive_tables.csv"),
  na = ""
)

message("Saved table notes: ", file.path(PATHS$tables, "table_notes_06_descriptive_tables.csv"))


# ------------------------------------------------------------------------------
# 6.11 Console preview
# ------------------------------------------------------------------------------

message("Step 6 table preview:")

print(table_01)
print(table_04)
print(table_11)
print(table_12)


# ------------------------------------------------------------------------------
# 6.12 Final message
# ------------------------------------------------------------------------------

message("Step 6 complete: descriptive and summary tables saved.")