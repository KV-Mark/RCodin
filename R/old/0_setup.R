# ============================================================
# 00_setup.R
# Shared setup for the crypto monetary policy thesis project
# ============================================================


# setwd("~/Downloads/RThesis")


# ------------------------------------------------------------
# 0.1 Packages
# ------------------------------------------------------------

packages <- c(
  "tidyverse",
  "lubridate",
  "janitor",
  "skimr",
  "here",
  "sandwich",
  "lmtest",
  "broom"
)

missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(tidyverse)
library(lubridate)
library(janitor)
library(skimr)
library(here)
library(sandwich)
library(lmtest)
library(broom)

# ------------------------------------------------------------
# 0.2 Project paths 
# ------------------------------------------------------------

PROJECT_DIR <- file.path(path.expand("~"), "Documents", "Timi", "RThesis")
# PROJECT_DIR <- file.path(path.expand("~"), "Downloads", "RThesis")


RAW_DIR <- file.path(PROJECT_DIR, "data", "raw")
PROCESSED_DIR <- file.path(PROJECT_DIR, "data", "processed")

OUTPUT_DIR <- file.path(PROJECT_DIR, "output")
FIGURE_DIR <- file.path(OUTPUT_DIR, "figures")
TABLE_DIR <- file.path(OUTPUT_DIR, "tables")

# ------------------------------------------------------------
# 0.3 Create folders if missing
# ------------------------------------------------------------

dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(PROCESSED_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIGURE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TABLE_DIR, recursive = TRUE, showWarnings = FALSE)

