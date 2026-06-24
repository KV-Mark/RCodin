# ==============================================================================
# 0_setup.R
# Thesis project: The Effect of Monetary Policy Announcements on Cryptocurrency Returns
#
# Purpose:
#   - Detect project root
#   - Define all paths
#   - Load/install packages
#   - Create output folders
#   - Define global model settings
#   - Define shared variable labels and graph/table settings
# ==============================================================================


# ------------------------------------------------------------------------------
# 0.1 Project root detection
# ------------------------------------------------------------------------------

# detect_project_root <- function(start_dir = getwd()) {
#   start_dir <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)
#   current <- start_dir
#   
#   repeat {
#     has_data_dir <- dir.exists(file.path(current, "data"))
#     has_code_dir <- dir.exists(file.path(current, "RCodin", "R"))
#     
#     if (has_data_dir && has_code_dir) {
#       return(current)
#     }
#     
#     parent <- dirname(current)
#     
#     if (identical(parent, current)) {
#       break
#     }
#     
#     current <- parent
#   }
#   
#   # Fallback for when the script is launched from RCodin/R before all folders exist
#   if (basename(start_dir) == "R" && basename(dirname(start_dir)) == "RCodin") {
#     return(dirname(dirname(start_dir)))
#   }
#   
#   if (basename(start_dir) == "RCodin") {
#     return(dirname(start_dir))
#   }
#   
#   # Final fallback: assume current working directory is the project root
#   start_dir
# }

PROJECT_ROOT <- file.path(path.expand("~"), "Documents", "Timi", "RThesis")
Sys.setenv(PROJECT_ROOT = PROJECT_ROOT)


# ------------------------------------------------------------------------------
# 0.2 Path definitions
# ------------------------------------------------------------------------------

PATHS <- list(
  project_root   = PROJECT_ROOT,
  data           = file.path(PROJECT_ROOT, "data"),
  data_raw       = file.path(PROJECT_ROOT, "data", "raw"),
  data_processed = file.path(PROJECT_ROOT, "data", "processed"),
  output         = file.path(PROJECT_ROOT, "output"),
  figures        = file.path(PROJECT_ROOT, "output", "figures"),
  tables         = file.path(PROJECT_ROOT, "output", "tables"),
  code           = file.path(PROJECT_ROOT, "RCodin", "R")
)

RAW_DATA_FILE <- file.path(PATHS$data_raw, "merged.csv")


# ------------------------------------------------------------------------------
# 0.3 Create required folders
# ------------------------------------------------------------------------------

create_dir_if_missing <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
}

invisible(lapply(PATHS, create_dir_if_missing))


# ------------------------------------------------------------------------------
# 0.4 Package loading
# ------------------------------------------------------------------------------

options(repos = c(CRAN = "https://cloud.r-project.org"))

REQUIRED_PACKAGES <- c(
  "tidyverse",  # dplyr, ggplot2, readr, tidyr, purrr, tibble, stringr
  "lubridate",  # date handling
  "janitor",    # clean_names()
  "moments",    # skewness()
  "broom",      # tidy model outputs
  "sandwich",   # Newey-West / HAC standard errors
  "lmtest",     # coeftest()
  "patchwork",  # plot grids
  "scales",     # axis labels
  "lpirfs"      # local projection impulse response functions
)

install_and_load_packages <- function(packages) {
  missing_packages <- packages[!packages %in% rownames(installed.packages())]
  
  if (length(missing_packages) > 0) {
    message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages)
  }
  
  invisible(
    lapply(
      packages,
      function(pkg) {
        suppressPackageStartupMessages(
          library(pkg, character.only = TRUE)
        )
      }
    )
  )
}

install_and_load_packages(REQUIRED_PACKAGES)


# ------------------------------------------------------------------------------
# 0.5 Global model settings
# ------------------------------------------------------------------------------

HORIZON_MAX <- 25
HORIZONS <- 0:HORIZON_MAX

CONF_LEVEL <- 0.95
CONF_Z <- qnorm(1 - (1 - CONF_LEVEL) / 2)

SIG_LEVEL <- 0.05

# Main LP settings
LP_LAGS_ENDOG <- 1
LP_LAGS_EXOG <- 1
LP_TREND <- 0

# For manual Newey-West regressions, the lag can be horizon-specific.
# A common LP convention is to allow the HAC lag to grow with horizon h.
NW_LAG_RULE <- "horizon"


# ------------------------------------------------------------------------------
# 0.6 Canonical variable names
# ------------------------------------------------------------------------------

DATE_VAR <- "date"

CRYPTO_RETURN_VARS <- c(
  "btc_log_return",
  "eth_log_return",
  "usdt_log_return"
)

STOCK_RETURN_VARS <- c(
  "sp500_log_return",
  "stoxx50_log_return"
)

MARKET_RETURN_VARS <- c(
  CRYPTO_RETURN_VARS,
  STOCK_RETURN_VARS,
  "dxy_log_return"
)

SHOCK_VARS <- c(
  ECB = "ecb_mp",
  Fed = "fed_mp"
)

FED_CONTROL_VARS <- c(
  "sp500_log_return",
  "dxy_log_return",
  "us_short",
  "us_long",
  "us_3m",
  "covid_dummy"
)

ECB_CONTROL_VARS <- c(
  "stoxx50_log_return",
  "dxy_log_return",
  "de_short",
  "de_long",
  "de_3m",
  "covid_dummy",
  "mica_dummy"
)

# Crypto trend controls:
# These are created later in Step 3, after each central-bank-specific
# trading-day sample has been filtered. They are not raw-data columns.
CRYPTO_TREND_LAG <- 5L

CRYPTO_5D_LAG_RETURN_VARS <- c(
  "btc_5d_lag_log_return",
  "eth_5d_lag_log_return",
  "usdt_5d_lag_log_return"
)

ECB_BASELINE_CONTROL_VARS <- unique(c(
  ECB_CONTROL_VARS,
  CRYPTO_5D_LAG_RETURN_VARS
))

FED_BASELINE_CONTROL_VARS <- unique(c(
  FED_CONTROL_VARS,
  CRYPTO_5D_LAG_RETURN_VARS
))

ALL_MODEL_VARS <- unique(c(
  DATE_VAR,
  CRYPTO_RETURN_VARS,
  STOCK_RETURN_VARS,
  "dxy_log_return",
  SHOCK_VARS,
  FED_CONTROL_VARS,
  ECB_CONTROL_VARS
))


# ------------------------------------------------------------------------------
# 0.7 Variable labels for tables and figures
# ------------------------------------------------------------------------------

VAR_LABELS <- c(
  date = "Date",
  
  btc_log_return = "BTC log return",
  eth_log_return = "ETH log return",
  usdt_log_return = "USDT log return",
  
  btc_5d_lag_log_return = "BTC lagged 5-trading-day log return",
  eth_5d_lag_log_return = "ETH lagged 5-trading-day log return",
  usdt_5d_lag_log_return = "USDT lagged 5-trading-day log return",
  
  sp500_log_return = "S&P 500 log return",
  stoxx50_log_return = "STOXX50 log return",
  dxy_log_return = "DXY log return",
  
  ecb_mp = "ECB monetary policy surprise",
  fed_mp = "Fed monetary policy surprise",
  
  us_short = "US short-term yield",
  us_long = "US long-term yield",
  us_3m = "US 3-month yield",
  
  de_short = "German short-term yield",
  de_long = "German long-term yield",
  de_3m = "German 3-month yield",
  
  covid_dummy = "COVID-19 dummy",
  mica_dummy = "MiCA dummy"
)

ASSET_LABELS <- c(
  btc_log_return = "BTC",
  eth_log_return = "ETH",
  usdt_log_return = "USDT"
)

CENTRAL_BANK_LABELS <- c(
  ECB = "ECB",
  Fed = "Fed"
)


# ------------------------------------------------------------------------------
# 0.8 Table formatting helpers
# ------------------------------------------------------------------------------

TABLE_DIGITS <- 4

round_numeric_columns <- function(data, digits = TABLE_DIGITS) {
  data %>%
    mutate(
      across(
        where(is.numeric),
        ~ round(.x, digits = digits)
      )
    )
}

write_table_csv <- function(data, filename, digits = TABLE_DIGITS) {
  output_path <- file.path(PATHS$tables, filename)
  
  data_out <- data %>%
    round_numeric_columns(digits = digits)
  
  readr::write_csv(data_out, output_path, na = "")
  
  message("Saved table: ", output_path)
  invisible(output_path)
}


# ------------------------------------------------------------------------------
# 0.9 Figure formatting helpers
# ------------------------------------------------------------------------------

FIG_DPI <- 320
FIG_WIDTH_DEFAULT <- 10
FIG_HEIGHT_DEFAULT <- 6

theme_thesis <- function(base_size = 11) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0),
      plot.subtitle = ggplot2::element_text(hjust = 0),
      plot.caption = ggplot2::element_text(hjust = 0, size = base_size - 2),
      
      axis.title = ggplot2::element_text(face = "plain"),
      axis.text = ggplot2::element_text(color = "black"),
      
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(linewidth = 0.20, color = "grey85"),
      
      strip.background = ggplot2::element_rect(fill = "white", color = "black"),
      strip.text = ggplot2::element_text(face = "bold")
    )
}

THESIS_LINETYPES <- c(
  "solid",
  "dashed",
  "dotted",
  "dotdash",
  "longdash"
)

THESIS_SHAPES <- c(
  16,
  1,
  2,
  0,
  4
)

THESIS_GREYS <- c(
  "black",
  "grey25",
  "grey45",
  "grey65",
  "grey80"
)

save_figure_png <- function(
    plot,
    filename,
    width = FIG_WIDTH_DEFAULT,
    height = FIG_HEIGHT_DEFAULT,
    dpi = FIG_DPI
) {
  output_path <- file.path(PATHS$figures, filename)
  
  ggplot2::ggsave(
    filename = output_path,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    units = "in",
    device = "png",
    bg = "white"
  )
  
  message("Saved figure: ", output_path)
  invisible(output_path)
}


# ------------------------------------------------------------------------------
# 0.10 Small utility functions
# ------------------------------------------------------------------------------

label_variable <- function(x) {
  labels <- VAR_LABELS[x]
  ifelse(is.na(labels), x, labels)
}

label_asset <- function(x) {
  labels <- ASSET_LABELS[x]
  ifelse(is.na(labels), x, labels)
}

safe_read_csv <- function(path) {
  if (!file.exists(path)) {
    stop("File does not exist: ", path, call. = FALSE)
  }
  
  readr::read_csv(path, show_col_types = FALSE)
}

check_required_columns <- function(data, required_cols, data_name = "data") {
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    stop(
      data_name,
      " is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}

standardize_na_shock_to_zero <- function(x) {
  ifelse(is.na(x), 0, x)
}


# ------------------------------------------------------------------------------
# 0.11 Setup summary
# ------------------------------------------------------------------------------

message("Setup complete.")
message("Project root: ", PROJECT_ROOT)
message("Raw data file expected at: ", RAW_DATA_FILE)

if (!file.exists(RAW_DATA_FILE)) {
  warning(
    "Raw data file not found yet. Expected location: ",
    RAW_DATA_FILE,
    call. = FALSE
  )
}

message("Step 0 complete: path setup complete.")