# ============================================================================
# Utility Functions - R Equivalents to Stata Helper Functions
# ============================================================================
#
# This file provides R equivalents to the Stata utility functions found in
# reg_robustness.do (tmpdir and confirmdir functions).
#
# The original Stata file contained:
# 1. tmpdir: Function to get/set temporary directory
# 2. confirmdir: Function to check if a directory exists
#
# In R, these functionalities are built-in and don't require custom functions.
# ============================================================================

# ============================================================================
# Temporary Directory Functions
# ============================================================================

#' Get Temporary Directory
#'
#' R equivalent to Stata's tmpdir function.
#' Returns the path to the system's temporary directory.
#'
#' @return Character string with path to temporary directory
#' @examples
#' temp_dir <- get_temp_dir()
#' print(temp_dir)
get_temp_dir <- function() {
  # tempdir() returns the per-session temporary directory
  temp_path <- tempdir()

  # Ensure the path ends with a separator
  if (!endsWith(temp_path, .Platform$file.sep)) {
    temp_path <- paste0(temp_path, .Platform$file.sep)
  }

  return(temp_path)
}

#' Create Temporary File
#'
#' Creates a temporary file path (file doesn't exist until you write to it).
#' R equivalent to Stata's tempfile command.
#'
#' @param pattern Optional prefix for the temporary file name
#' @param fileext Optional file extension (default: "")
#' @return Character string with path to temporary file
#' @examples
#' temp_file <- create_temp_file(pattern = "mydata", fileext = ".csv")
#' # Now you can write to this file
#' # write.csv(data, temp_file)
create_temp_file <- function(pattern = "file", fileext = "") {
  return(tempfile(pattern = pattern, fileext = fileext))
}

#' Create Temporary Directory
#'
#' Creates a temporary directory.
#'
#' @param pattern Optional prefix for the temporary directory name
#' @return Character string with path to temporary directory
#' @examples
#' temp_subdir <- create_temp_subdir(pattern = "analysis")
create_temp_subdir <- function(pattern = "dir") {
  temp_path <- tempfile(pattern = pattern)
  dir.create(temp_path, recursive = TRUE)
  return(temp_path)
}

# ============================================================================
# Directory Checking Functions
# ============================================================================

#' Check if Directory Exists
#'
#' R equivalent to Stata's confirmdir function.
#' Checks if a directory exists at the given path.
#'
#' @param path Character string with directory path
#' @return Logical: TRUE if directory exists, FALSE otherwise
#' @examples
#' if (confirm_dir("/path/to/dir")) {
#'   print("Directory exists")
#' } else {
#'   print("Directory does not exist")
#' }
confirm_dir <- function(path) {
  return(dir.exists(path))
}

#' Check if File Exists
#'
#' Checks if a file exists at the given path.
#'
#' @param path Character string with file path
#' @return Logical: TRUE if file exists, FALSE otherwise
#' @examples
#' if (file_exists("/path/to/file.csv")) {
#'   data <- read.csv("/path/to/file.csv")
#' }
file_exists <- function(path) {
  return(file.exists(path))
}

#' Create Directory if it Doesn't Exist
#'
#' Creates a directory if it doesn't already exist.
#' Equivalent to Stata's mkdir command.
#'
#' @param path Character string with directory path
#' @param recursive Logical: create parent directories if needed (default: TRUE)
#' @return Logical: TRUE if directory was created or already exists
#' @examples
#' ensure_dir_exists("output/tables")
#' # Now you can safely write files to this directory
ensure_dir_exists <- function(path, recursive = TRUE) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = recursive)
    return(TRUE)
  }
  return(TRUE)
}

# ============================================================================
# Path Manipulation Functions
# ============================================================================

#' Normalize Path
#'
#' Converts a path to use the correct separator for the current OS
#' and expands relative paths to absolute paths.
#'
#' @param path Character string with path
#' @return Normalized absolute path
#' @examples
#' norm_path <- normalize_path("~/data/myfile.csv")
normalize_path <- function(path) {
  return(normalizePath(path, mustWork = FALSE))
}

#' Join Path Components
#'
#' Joins multiple path components using the correct separator for the OS.
#'
#' @param ... Character strings with path components
#' @return Combined path string
#' @examples
#' full_path <- join_path("output", "tables", "results.csv")
join_path <- function(...) {
  return(file.path(...))
}

#' Get Working Directory
#'
#' Returns the current working directory.
#' Equivalent to Stata's pwd command.
#'
#' @return Character string with current working directory
#' @examples
#' current_dir <- get_working_dir()
get_working_dir <- function() {
  return(getwd())
}

#' Set Working Directory
#'
#' Changes the current working directory.
#' Equivalent to Stata's cd command.
#'
#' @param path Character string with new working directory path
#' @return Previous working directory (invisibly)
#' @examples
#' old_dir <- set_working_dir("/path/to/project")
#' # Do some work...
#' # Restore previous directory
#' set_working_dir(old_dir)
set_working_dir <- function(path) {
  old_dir <- getwd()
  setwd(path)
  return(invisible(old_dir))
}

# ============================================================================
# Usage Examples
# ============================================================================

#' Example: Working with Temporary Files
#'
#' This example shows how to use temporary files in R,
#' equivalent to Stata's tempfile command.
example_temp_files <- function() {
  # Get temp directory
  temp_dir <- get_temp_dir()
  cat("Temp directory:", temp_dir, "\n")

  # Create a temporary file for data
  temp_data <- create_temp_file(pattern = "mydata", fileext = ".csv")
  cat("Temp file path:", temp_data, "\n")

  # Write some data to it
  df <- data.frame(x = 1:5, y = 6:10)
  write.csv(df, temp_data, row.names = FALSE)

  # Read it back
  df_read <- read.csv(temp_data)
  print(df_read)

  # File will be automatically deleted when R session ends
}

#' Example: Directory Management
#'
#' This example shows how to manage directories in R.
example_directory_management <- function() {
  # Check if a directory exists
  if (confirm_dir("output")) {
    cat("Output directory exists\n")
  } else {
    cat("Output directory does not exist\n")
    ensure_dir_exists("output")
    cat("Output directory created\n")
  }

  # Create nested directories
  ensure_dir_exists("output/tables/latex")

  # Get and set working directory
  old_dir <- get_working_dir()
  cat("Current directory:", old_dir, "\n")

  # Change directory
  if (confirm_dir("output")) {
    set_working_dir("output")
    cat("New directory:", get_working_dir(), "\n")

    # Go back
    set_working_dir(old_dir)
    cat("Back to:", get_working_dir(), "\n")
  }
}

# ============================================================================
# Note on Cross-Platform Compatibility
# ============================================================================
#
# Unlike Stata's tmpdir function which has special handling for Windows
# short file names (8.3 format), R's built-in functions handle paths
# automatically across different operating systems.
#
# R uses forward slashes (/) internally for all paths, regardless of OS,
# and converts them automatically when needed. You can also use double
# backslashes (\\) on Windows if needed.
#
# Examples:
# - Windows: "C:/Users/Name/Documents/data.csv" or "C:\\Users\\Name\\Documents\\data.csv"
# - Linux/Mac: "/home/user/documents/data.csv"
# - file.path() handles separators automatically
#
# ============================================================================

# Print information when sourced
if (interactive()) {
  cat("Utility functions loaded successfully.\n")
  cat("Available functions:\n")
  cat("  - get_temp_dir(): Get temporary directory path\n")
  cat("  - create_temp_file(): Create temporary file path\n")
  cat("  - create_temp_subdir(): Create temporary directory\n")
  cat("  - confirm_dir(): Check if directory exists\n")
  cat("  - file_exists(): Check if file exists\n")
  cat("  - ensure_dir_exists(): Create directory if needed\n")
  cat("  - normalize_path(): Normalize file path\n")
  cat("  - join_path(): Join path components\n")
  cat("  - get_working_dir(): Get current directory\n")
  cat("  - set_working_dir(): Change working directory\n")
  cat("\nRun example_temp_files() or example_directory_management() to see examples.\n")
}
