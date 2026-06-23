# ============================================================
# 0_setup.R
# Shared setup for thesis project:
# "The Effect of Monetary Policy Announcements on Cryptocurrency Returns"
# ============================================================

# -----------------------------
# 1. Package management
# -----------------------------

required_packages <- c(
  "tidyverse",
  "lubridate",
  "janitor",
  "sandwich",
  "lmtest",
  "broom",
  "lpirfs",
  "patchwork",
  "scales"
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

invisible(lapply(required_packages, install_if_missing))

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(janitor)
  library(sandwich)
  library(lmtest)
  library(broom)
  library(lpirfs)
  library(patchwork)
  library(scales)
})

# -----------------------------
# 2. Project paths
# -----------------------------

# Assumption:
# Run scripts from the project root directory, i.e. the folder containing:
# data/
# output/
# RCodin/

PROJECT_ROOT <- file.path(path.expand("~"), "Documents", "Timi", "RThesis")

DATA_RAW_DIR       <- file.path(PROJECT_ROOT, "data", "raw")
DATA_PROCESSED_DIR <- file.path(PROJECT_ROOT, "data", "processed")
OUTPUT_DIR         <- file.path(PROJECT_ROOT, "output")
TABLES_DIR         <- file.path(OUTPUT_DIR, "tables")
FIGURES_DIR        <- file.path(OUTPUT_DIR, "figures")
R_DIR              <- file.path(PROJECT_ROOT, "RCodin", "R")

dir.create(DATA_RAW_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(DATA_PROCESSED_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TABLES_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIGURES_DIR, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 3. Global analysis settings
# -----------------------------

HORIZONS <- 0:25

CONF_LEVEL <- 0.95
SIGNIF_LEVEL <- 0.05

Z_CRIT_95 <- qnorm(1 - (1 - CONF_LEVEL) / 2)

# File names
RAW_DATA_FILE <- file.path(DATA_RAW_DIR, "merged_data_simplified.csv")

# Main processed files
MASTER_CLEAN_FILE <- file.path(DATA_PROCESSED_DIR, "master_clean_daily.rds")
EU_TRADING_FILE   <- file.path(DATA_PROCESSED_DIR, "eu_trading_days.rds")
US_TRADING_FILE   <- file.path(DATA_PROCESSED_DIR, "us_trading_days.rds")

IRF_CUSTOM_FILE    <- file.path(DATA_PROCESSED_DIR, "irf_results_custom.rds")
IRF_LPIRFS_FILE    <- file.path(DATA_PROCESSED_DIR, "irf_results_lpirfs_validation.rds")

# -----------------------------
# 4. Variable groups
# -----------------------------

price_vars <- c(
  "btc_price",
  "eth_price",
  "usdt_price",
  "sp500",
  "stoxx50",
  "dxy"
)

crypto_price_vars <- c(
  "btc_price",
  "eth_price",
  "usdt_price"
)

stock_price_vars <- c(
  "sp500",
  "stoxx50"
)

return_vars <- c(
  "btc_ret",
  "eth_ret",
  "usdt_ret",
  "sp500_ret",
  "stoxx50_ret",
  "dxy_ret"
)

crypto_return_vars <- c(
  "btc_ret",
  "eth_ret",
  "usdt_ret"
)

stock_return_vars <- c(
  "sp500_ret",
  "stoxx50_ret"
)

shock_vars <- c(
  "ecb_mp",
  "fed_mp"
)

eu_control_vars <- c(
  "stoxx50_ret",
  "de_short",
  "de_long",
  "de_3m",
  "covid_dummy",
  "mica_dummy"
)

us_control_vars <- c(
  "sp500_ret",
  "dxy_ret",
  "us_short",
  "us_long",
  "us_3m",
  "covid_dummy"
)

all_control_vars <- unique(c(
  eu_control_vars,
  us_control_vars
))

# -----------------------------
# 5. Helper functions: safe parsing
# -----------------------------

parse_date_safe <- function(x) {
  x_chr <- as.character(x)
  
  parsed <- suppressWarnings(lubridate::dmy(x_chr))
  
  if (all(is.na(parsed))) {
    parsed <- suppressWarnings(lubridate::ymd(x_chr))
  }
  
  if (all(is.na(parsed))) {
    parsed <- suppressWarnings(lubridate::mdy(x_chr))
  }
  
  parsed
}

parse_numeric_safe <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }
  
  x_chr <- as.character(x)
  
  x_chr <- stringr::str_trim(x_chr)
  x_chr <- stringr::str_replace_all(x_chr, "\\s", "")
  x_chr <- stringr::str_replace_all(x_chr, "%", "")
  
  # Handles European decimal format, e.g. "1.234,56"
  has_comma_decimal <- stringr::str_detect(x_chr, ",")
  
  x_chr[has_comma_decimal] <- x_chr[has_comma_decimal] |>
    stringr::str_replace_all("\\.", "") |>
    stringr::str_replace_all(",", ".")
  
  suppressWarnings(as.numeric(x_chr))
}

# -----------------------------
# 6. Helper functions: returns and leads
# -----------------------------

make_log_return <- function(price) {
  100 * (log(price) - log(dplyr::lag(price, 1)))
}

make_future_return <- function(ret, h) {
  dplyr::lead(ret, h)
}

# -----------------------------
# 7. Helper functions: summary statistics
# -----------------------------

calc_skewness <- function(x) {
  x <- x[is.finite(x)]
  
  n <- length(x)
  
  if (n < 3) {
    return(NA_real_)
  }
  
  x_mean <- mean(x, na.rm = TRUE)
  x_sd <- sd(x, na.rm = TRUE)
  
  if (is.na(x_sd) || x_sd == 0) {
    return(NA_real_)
  }
  
  mean(((x - x_mean) / x_sd)^3, na.rm = TRUE)
}

summary_stats <- function(data, vars) {
  data |>
    dplyr::select(dplyr::any_of(vars)) |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "variable",
      values_to = "value"
    ) |>
    dplyr::group_by(variable) |>
    dplyr::summarise(
      n = sum(!is.na(value)),
      mean = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      min = min(value, na.rm = TRUE),
      max = max(value, na.rm = TRUE),
      skewness = calc_skewness(value),
      .groups = "drop"
    )
}

# -----------------------------
# 8. Helper functions: Newey-West LP output
# -----------------------------

tidy_lm_newey_west <- function(model, lag = NULL, prewhite = FALSE, adjust = TRUE) {
  if (is.null(lag)) {
    lag <- 0
  }
  
  vcov_nw <- sandwich::NeweyWest(
    model,
    lag = lag,
    prewhite = prewhite,
    adjust = adjust
  )
  
  lmtest::coeftest(model, vcov. = vcov_nw) |>
    broom::tidy() |>
    dplyr::rename(
      coefficient = estimate,
      std_error = std.error,
      t_stat = statistic,
      p_value = p.value
    )
}

# -----------------------------
# 9. Helper functions: saving outputs
# -----------------------------

save_table_csv <- function(data, filename) {
  output_path <- file.path(TABLES_DIR, filename)
  
  readr::write_csv(data, output_path, na = "")
  
  message("Saved table: ", output_path)
  
  invisible(output_path)
}

save_figure_png <- function(plot, filename, width = 10, height = 6, dpi = 300) {
  output_path <- file.path(FIGURES_DIR, filename)
  
  ggplot2::ggsave(
    filename = output_path,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    units = "in",
    bg = "white"
  )
  
  message("Saved figure: ", output_path)
  
  invisible(output_path)
}

# -----------------------------
# 10. Thesis plotting theme
# -----------------------------

theme_thesis_bw <- function(base_size = 11) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0),
      plot.subtitle = ggplot2::element_text(hjust = 0),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "white", color = "black"),
      strip.text = ggplot2::element_text(face = "bold")
    )
}

# -----------------------------
# 11. Utility checks
# -----------------------------

check_required_columns <- function(data, required_cols, data_name = "data") {
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns in ", data_name, ": ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}

check_file_exists <- function(path) {
  if (!file.exists(path)) {
    stop("File does not exist: ", path, call. = FALSE)
  }
  
  invisible(TRUE)
}

# -----------------------------
# 12. Console confirmation
# -----------------------------

message("Setup loaded successfully.")
message("Project root: ", PROJECT_ROOT)
message("Tables will be saved to: ", TABLES_DIR)
message("Figures will be saved to: ", FIGURES_DIR)
message("Processed data will be saved to: ", DATA_PROCESSED_DIR)

