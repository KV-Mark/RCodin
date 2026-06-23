# ============================================================
# Step 1: Import and inspect raw data
# ============================================================

source(file.path(path.expand("~"), "Documents", "Timi", "RThesis", "RCodin", "R", "0_setup.R"))
# source(file.path(path.expand("~"), "Downloads", "RThesis", "R", "0_setup.R"))


merged_raw <- read_csv(
  file.path(RAW_DIR, "merged_data.csv"),
  show_col_types = FALSE
)

hacks_raw <- read_csv(
  file.path(RAW_DIR, "hacks.csv"),
  show_col_types = FALSE
)


# Basic dimensions: rows and columns 
dim(merged_raw)
dim(hacks_raw)

# Column names
names(merged_raw)
names(hacks_raw)

# First few rows
head(merged_raw)
head(hacks_raw)

# Compact overview of column types and sample values
glimpse(merged_raw)
glimpse(hacks_raw)


# cleaning column names
merged <- merged_raw %>%
  clean_names() %>%
  mutate(date = ymd(date))


hacks <- hacks_raw %>%
  clean_names()

# Check names after cleaning
names(merged)
names(hacks)

# Check date range
range(merged$date, na.rm = TRUE)


# Number of rows and columns
nrow(merged)
ncol(merged)

# Confirm date column is a Date object
class(merged$date)

# Check whether dates are sorted
is.unsorted(merged$date)

# Check for duplicate dates
sum(duplicated(merged$date))

# Missing values per column
missing_summary <- merged %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing_count"
  ) %>%
  arrange(desc(missing_count))

missing_summary

# Quick summary of all variables
skim(merged)


# checking key variables
key_vars <- c(
  "date",
  "btc_price",
  "eth_price",
  "usdt_price",
  "fed_mp",
  "ecb_mp",
  "fed_cbi",
  "ecb_cbi",
  "sp500",
  "stoxx50",
  "vix",
  "dxy",
  "us_3m",
  "us_10y",
  "de_3m",
  "de_10y"
)

setdiff(key_vars, names(merged))

merged %>%
  summarise(
    fed_mp_nonzero = sum(fed_mp != 0, na.rm = TRUE),
    ecb_mp_nonzero = sum(ecb_mp != 0, na.rm = TRUE),
    fed_cbi_nonzero = sum(fed_cbi != 0, na.rm = TRUE),
    ecb_cbi_nonzero = sum(ecb_cbi != 0, na.rm = TRUE)
  )


# Save cleaned imported data as RDS
# RDS is an R-specific file format that preserves column types
saveRDS(merged, file.path(PROCESSED_DIR, "merged_step1_clean_import.rds"))
saveRDS(hacks, file.path(PROCESSED_DIR, "hacks_step1_clean_import.rds"))







