# ==============================================================================
# Project Setup Script
# ==============================================================================
# Green Innovation and Climate Shocks in Brazil
#
# This script sets up the R environment with all required packages,
# defines common functions, and sets global parameters for the analysis.
#
# Run this script before running any other analysis scripts.
# ==============================================================================

# Clear workspace
rm(list = ls())
gc()

# ==============================================================================
# 1. PACKAGE INSTALLATION AND LOADING
# ==============================================================================

cat("Setting up R environment for Green Innovation project...\n\n")

# List of required packages
required_packages <- c(
  # Data manipulation
  "data.table",      # Fast data manipulation
  "dplyr",          # Data wrangling
  "tidyr",          # Data tidying
  "readr",          # Fast data reading
  "readxl",         # Excel file reading

  # Statistical analysis
  "fixest",         # Fast fixed effects regressions
  "lfe",            # Linear fixed effects models
  "plm",            # Panel data models
  "sandwich",       # Robust covariance matrices
  "lmtest",         # Hypothesis testing

  # Spatial analysis
  "conleyreg",      # Conley (spatial HAC) standard errors
  "sf",             # Spatial data handling
  "geobr",          # Brazilian geographic data
  "spdep",          # Spatial dependence

  # Data import/export
  "haven",          # Stata/SPSS data files
  "foreign",        # Other foreign data formats

  # Visualization
  "ggplot2",        # Advanced plotting
  "gridExtra",      # Multiple plots
  "scales",         # Scale functions for visualization
  "viridis",        # Color palettes
  "RColorBrewer",   # Color palettes

  # Climate data
  "raster",         # Raster data handling
  "ncdf4",          # NetCDF climate files

  # Utilities
  "lubridate",      # Date handling
  "stringr",        # String manipulation
  "future",         # Parallel processing
  "future.apply",   # Apply functions in parallel

  # Tables and reporting
  "stargazer",      # LaTeX/HTML tables
  "xtable",         # Export tables
  "knitr",          # Dynamic reports
  "kableExtra"      # Enhanced tables
)

# Function to install missing packages
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages) > 0) {
    cat("Installing missing packages:", paste(new_packages, collapse = ", "), "\n")
    install.packages(new_packages, dependencies = TRUE)
  } else {
    cat("All required packages are already installed.\n")
  }
}

# Install missing packages
install_if_missing(required_packages)

# Load all packages
cat("\nLoading packages...\n")
invisible(lapply(required_packages, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  cat("  ✓", pkg, "\n")
}))

# ==============================================================================
# 2. SET WORKING DIRECTORY AND PATHS
# ==============================================================================

cat("\nSetting up project directories...\n")

# Get project root directory (assumes script is run from project root)
project_root <- getwd()

# Define directory structure
dirs <- list(
  root = project_root,

  # Data directories (create these manually and add to .gitignore)
  data_raw = file.path(project_root, "data", "raw"),
  data_processed = file.path(project_root, "data", "processed"),
  data_merged = file.path(project_root, "data", "merged"),

  # Output directories
  output = file.path(project_root, "output"),
  figures = file.path(project_root, "output", "figures"),
  tables = file.path(project_root, "output", "tables"),
  logs = file.path(project_root, "output", "logs"),

  # Script directories
  processing = file.path(project_root, "01_data_processing"),
  merge = file.path(project_root, "02_data_merge"),
  analysis = file.path(project_root, "03_analysis"),
  visualization = file.path(project_root, "04_visualization")
)

# Create output directories if they don't exist
output_dirs <- c(dirs$output, dirs$figures, dirs$tables, dirs$logs)
for (dir in output_dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat("  Created:", dir, "\n")
  }
}

# Make directories globally available
list2env(dirs, envir = .GlobalEnv)

# ==============================================================================
# 3. SET GLOBAL OPTIONS
# ==============================================================================

cat("\nSetting global options...\n")

# General options
options(
  scipen = 999,                    # Disable scientific notation
  stringsAsFactors = FALSE,        # Don't auto-convert strings to factors
  mc.cores = parallel::detectCores() - 1  # Use all but one core for parallel processing
)

# data.table options
setDTthreads(threads = 0)  # Use all available threads

# ggplot2 theme
theme_set(theme_minimal(base_size = 12))

# ==============================================================================
# 4. DEFINE COMMON FUNCTIONS
# ==============================================================================

cat("\nLoading common functions...\n")

# Function to save regression results as LaTeX table
save_regression_table <- function(models, file, title = "",
                                  covariate.labels = NULL,
                                  dep.var.labels = NULL,
                                  add.lines = NULL) {
  stargazer(models,
            type = "latex",
            title = title,
            covariate.labels = covariate.labels,
            dep.var.labels = dep.var.labels,
            add.lines = add.lines,
            omit.stat = c("ser", "f"),
            star.cutoffs = c(0.1, 0.05, 0.01),
            notes = c("Standard errors in parentheses.",
                     "* p<0.1, ** p<0.05, *** p<0.01"),
            notes.align = "l",
            out = file.path(dirs$tables, file))
  cat("  Saved regression table:", file, "\n")
}

# Function to save figures
save_figure <- function(plot, filename, width = 10, height = 7, dpi = 300) {
  ggsave(filename = file.path(dirs$figures, filename),
         plot = plot,
         width = width,
         height = height,
         dpi = dpi)
  cat("  Saved figure:", filename, "\n")
}

# Function to log script execution
log_execution <- function(script_name) {
  log_file <- file.path(dirs$logs, "execution_log.txt")
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- paste0(timestamp, " - Executed: ", script_name, "\n")
  cat(log_entry, file = log_file, append = TRUE)
  cat("Logged execution:", script_name, "\n")
}

# Function to load processed data with error handling
load_data <- function(filename, dir = dirs$data_processed) {
  filepath <- file.path(dir, filename)

  if (!file.exists(filepath)) {
    stop("File not found: ", filepath)
  }

  cat("Loading:", filename, "...")

  # Determine file type and load accordingly
  if (grepl("\\.rds$", filename)) {
    data <- readRDS(filepath)
  } else if (grepl("\\.csv$", filename)) {
    data <- fread(filepath)
  } else if (grepl("\\.dta$", filename)) {
    data <- as.data.table(haven::read_dta(filepath))
  } else {
    stop("Unsupported file format: ", filename)
  }

  cat(" Done! (", format(nrow(data), big.mark = ","), " rows)\n")
  return(data)
}

# Function to save processed data
save_data <- function(data, filename, dir = dirs$data_processed, format = "rds") {
  filepath <- file.path(dir, filename)

  cat("Saving:", filename, "...")

  if (format == "rds") {
    saveRDS(data, filepath)
  } else if (format == "csv") {
    fwrite(data, filepath)
  } else if (format == "dta") {
    haven::write_dta(data, filepath)
  } else {
    stop("Unsupported format: ", format)
  }

  cat(" Done!\n")
}

# Function to print summary statistics
print_summary <- function(data, vars = NULL) {
  if (is.null(vars)) {
    vars <- names(data)
  }

  cat("\nSummary Statistics:\n")
  cat("-------------------\n")
  cat("Observations:", format(nrow(data), big.mark = ","), "\n")
  cat("Variables:", length(vars), "\n\n")

  print(summary(data[, ..vars]))
}

# ==============================================================================
# 5. DEFINE COMMON PARAMETERS
# ==============================================================================

cat("\nSetting analysis parameters...\n")

# Analysis time period
params <- list(
  # Time periods
  year_start = 1940,
  year_end = 2020,

  # Climate shock definitions
  temp_threshold_high = 95,    # 95th percentile for heat shocks
  temp_threshold_low = 5,       # 5th percentile for cold shocks
  precip_threshold_high = 95,   # 95th percentile for flood shocks
  precip_threshold_low = 5,     # 5th percentile for drought shocks

  # Fixed effects specifications
  fe_municipality = TRUE,
  fe_year = TRUE,
  fe_state_year = TRUE,

  # Clustering
  cluster_municipality = TRUE,

  # Significance levels
  alpha_1 = 0.01,
  alpha_5 = 0.05,
  alpha_10 = 0.10
)

# Make parameters globally available
list2env(params, envir = .GlobalEnv)

# ==============================================================================
# SETUP COMPLETE
# ==============================================================================

cat("\n")
cat("==============================================================================\n")
cat("Setup complete! Ready to run analysis scripts.\n")
cat("==============================================================================\n")
cat("\nProject root:", project_root, "\n")
cat("R version:", R.version.string, "\n")
cat("Available cores:", options()$mc.cores, "\n")
cat("\nTo run the full analysis pipeline, execute: source('run_all.R')\n")
cat("==============================================================================\n\n")
