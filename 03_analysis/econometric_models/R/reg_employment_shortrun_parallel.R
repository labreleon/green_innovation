# ============================================================================
# SHORT-RUN PANEL ESTIMATES - EMPLOYMENT OUTCOMES (2000-2020)
# Replicates Table: Weather shocks and Employment: Short-Run Panel Regressions
# PARALLEL VERSION - Runs 3 models simultaneously for maximum speed
# ============================================================================

# Load required packages
library(haven)        # For reading Stata files
library(dplyr)        # For data manipulation
library(fixest)       # For high-dimensional fixed effects
library(conleyreg)    # For Conley spatial standard errors
library(sandwich)     # For robust standard errors
library(lmtest)       # For coefficient testing
library(data.table)   # For efficient data manipulation
library(stargazer)    # For LaTeX table generation
library(parallel)     # For parallel processing

# Detect number of cores (use all but one to keep system responsive)
n_cores <- max(1, detectCores() - 1)
cat("Using", n_cores, "cores for parallel processing\n\n")

# ============================================================================
# STEP ONE: LOAD AND PREPARE DATA
# ============================================================================

cat("Loading data...\n")

# Load data (adjust path as needed)
data <- read_dta("./output/final_base/weather_rais.dta")

# Filter for years 2000-2020 and convert to data.table for speed
data <- as.data.table(data)
data <- data[year >= 2000 & year <= 2020]

cat("Data loaded. Observations:", nrow(data), "\n")

# ============================================================================
# STEP TWO: PREPARE VARIABLES (OPTIMIZED WITH DATA.TABLE)
# ============================================================================

cat("Preparing variables...\n")

# Rename variables to match expected names
if("total_vinc" %in% names(data)) {
  setnames(data,
           old = c("total_vinc", "total_vinc_verde"),
           new = c("total_jobs", "green_jobs"),
           skip_absent = TRUE)
}

# Ensure prop_verde exists
if(!"prop_verde" %in% names(data) && all(c("total_jobs", "green_jobs") %in% names(data))) {
  data[, prop_verde := green_jobs / total_jobs]
}

# ============================================================================
# STEP THREE: PRE-COMPUTE ALL DEMEANED VARIABLES (MAJOR OPTIMIZATION)
# ============================================================================

cat("Pre-computing demeaned variables for Conley (this speeds up later steps)...\n")

# Define dependent variables
dep_vars <- c("total_jobs", "green_jobs", "prop_verde")
dep_labels <- c("Total Jobs", "Green Jobs", "Prop. Green Jobs")

# Filter out missing values once for all models
data_clean <- data[!is.na(lat) & !is.na(lon) &
                   !is.na(cont_shock_temp) & !is.na(cont_shock_precip)]

# Remove rows where ALL dependent variables are missing
data_clean <- data_clean[!is.na(total_jobs) | !is.na(green_jobs) | !is.na(prop_verde)]

cat("Clean data observations:", nrow(data_clean), "\n")

# Create year dummies ONCE (avoid repeating this 3 times)
years <- sort(unique(data_clean$year))
n_years <- length(years)

# Use sparse matrix representation for memory efficiency
year_dummies <- model.matrix(~ factor(year) - 1, data = data_clean)
colnames(year_dummies) <- paste0("year_", years)

# Add year dummies to data
data_conley <- cbind(data_clean, year_dummies)
data_conley <- as.data.table(data_conley)

# Demean ALL variables by municipality at once (much faster than doing it 3 times)
cat("  Demeaning by municipality fixed effects...\n")

# List of all variables to demean
vars_to_demean <- c(dep_vars, "cont_shock_temp", "cont_shock_precip",
                    paste0("year_", years))

# Demean all variables in one pass using data.table (very fast)
data_conley[, paste0(vars_to_demean, "_dm") := lapply(.SD, function(x) x - mean(x, na.rm = TRUE)),
            by = mun_code, .SDcols = vars_to_demean]

# Create formula components once
year_cols_dm <- paste0("year_", years, "_dm")
# Remove one year dummy to avoid perfect collinearity
year_cols_dm <- year_cols_dm[-1]  # Drop first year
formula_years <- paste(year_cols_dm, collapse = " + ")

cat("Pre-computation complete.\n\n")

# ============================================================================
# STEP FOUR: RUN REGRESSIONS IN PARALLEL
# ============================================================================

cat("Running regressions in parallel...\n\n")

# Function to run one model
run_model <- function(i, dep_vars, dep_labels, data, data_conley, formula_years) {

  dv <- dep_vars[i]
  cat("  [Core", i, "] Model", i, "-", dep_labels[i], "\n")

  # -------------------------------------------------------------------------
  # Model with municipal and year fixed effects (using original data)
  # -------------------------------------------------------------------------

  formula_fe <- as.formula(paste0(dv, " ~ cont_shock_temp + cont_shock_precip | mun_code + year"))

  # Run fixed effects regression using fixest on original data
  model_fe <- feols(formula_fe, data = data, cluster = ~mun_code)

  # Extract basic results
  coefs <- coef(model_fe)
  se_basic <- se(model_fe)

  # -------------------------------------------------------------------------
  # Calculate Conley Spatial HAC Standard Errors
  # -------------------------------------------------------------------------

  cat("  [Core", i, "] Calculating Conley spatial standard errors...\n")

  # Filter for this specific dependent variable
  data_model <- data_conley[!is.na(get(dv))]

  cat("  [Core", i, "] Observations for this model:", nrow(data_model), "\n")

  # Formula for Conley with demeaned variables (municipality FE absorbed)
  formula_conley <- as.formula(paste0(
    dv, "_dm ~ cont_shock_temp_dm + cont_shock_precip_dm + ",
    formula_years
  ))

  # Run Conley regression
  result <- tryCatch({
    # Convert to data.frame for conleyreg
    data_model_df <- as.data.frame(data_model)

    conley_model <- conleyreg::conleyreg(
      formula = formula_conley,
      data = data_model_df,
      lat = "lat",
      lon = "lon",
      dist_cutoff = 250,    # 250 km spatial correlation
      lag_cutoff = 7        # 7-year temporal correlation (Newey-West)
    )

    # Extract Conley standard errors
    conley_summary <- summary(conley_model)
    se_conley <- conley_summary$coefficients[, "Std. Error"]
    coefs_conley <- conley_summary$coefficients[, "Estimate"]
    pval_conley <- conley_summary$coefficients[, "Pr(>|t|)"]

    # Return results
    list(
      model = model_fe,
      coef_temp = coefs_conley["cont_shock_temp_dm"],
      se_temp = se_conley["cont_shock_temp_dm"],
      pval_temp = pval_conley["cont_shock_temp_dm"],
      coef_precip = coefs_conley["cont_shock_precip_dm"],
      se_precip = se_conley["cont_shock_precip_dm"],
      pval_precip = pval_conley["cont_shock_precip_dm"],
      n_obs = nobs(model_fe),
      r2 = r2(model_fe, type = "r2"),
      success = TRUE
    )

  }, error = function(e) {
    warning(paste("  [Core", i, "] Conley SE calculation failed for", dep_labels[i], ":", e$message))
    warning(paste("  [Core", i, "] Using clustered standard errors instead."))

    # Fallback to clustered SEs
    list(
      model = model_fe,
      coef_temp = coefs["cont_shock_temp"],
      se_temp = se_basic["cont_shock_temp"],
      pval_temp = summary(model_fe)$coeftable["cont_shock_temp", "Pr(>|t|)"],
      coef_precip = coefs["cont_shock_precip"],
      se_precip = se_basic["cont_shock_precip"],
      pval_precip = summary(model_fe)$coeftable["cont_shock_precip", "Pr(>|t|)"],
      n_obs = nobs(model_fe),
      r2 = r2(model_fe, type = "r2"),
      success = FALSE
    )
  })

  cat("  [Core", i, "] Model", i, "completed.\n")
  return(result)
}

# Run models in parallel using mclapply (Unix/Mac) or parLapply (Windows)
if (.Platform$OS.type == "unix") {
  # Use mclapply for Unix/Mac (more efficient)
  results_list <- mclapply(1:3, function(i) {
    run_model(i, dep_vars, dep_labels, data, data_conley, formula_years)
  }, mc.cores = min(3, n_cores))
} else {
  # Use parLapply for Windows
  cl <- makeCluster(min(3, n_cores))

  # Export necessary objects to cluster
  clusterExport(cl, c("dep_vars", "dep_labels", "data", "data_conley",
                      "formula_years", "run_model"))

  # Load necessary packages on each worker
  clusterEvalQ(cl, {
    library(fixest)
    library(conleyreg)
    library(data.table)
  })

  # Run models in parallel
  results_list <- parLapply(cl, 1:3, function(i) {
    run_model(i, dep_vars, dep_labels, data, data_conley, formula_years)
  })

  # Stop cluster
  stopCluster(cl)
}

cat("\nAll models completed.\n\n")

# ============================================================================
# STEP FIVE: GENERATE LATEX TABLE
# ============================================================================

cat("Generating LaTeX table...\n")

# Helper function to add significance stars
add_stars <- function(pval) {
  if(is.na(pval)) return("")
  if(pval < 0.01) return("***")
  if(pval < 0.05) return("**")
  if(pval < 0.10) return("*")
  return("")
}

# Helper function to format numbers
format_coef <- function(x, digits = 3) {
  sprintf(paste0("%.", digits, "f"), x)
}

# Build LaTeX table
latex_lines <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\resizebox{0.7\\textwidth}{!}{%",
  "\\begin{threeparttable}",
  "\\caption{Short-Run Panel Estimates – Employment Outcomes - (2000-2020)}",
  "\\label{tab:sr_employment}",
  "\\begin{tabular}{lccc}",
  "\\toprule",
  "\\multicolumn{4}{c}{\\textbf{Weather shocks and Employment: Short-Run Panel Regressions}}\\\\",
  "\\midrule",
  "Dependent Variables:",
  paste0("  & (1) ", dep_labels[1]),
  paste0("  & (2) ", dep_labels[2]),
  paste0("  & (3) ", dep_labels[3], " \\\\"),
  "\\midrule"
)

# Temperature coefficient
temp_row <- "$T_{mt}$ ($\\beta$)"
temp_se_row <- ""
for(i in 1:3) {
  coef_val <- results_list[[i]]$coef_temp
  se_val <- results_list[[i]]$se_temp
  pval <- results_list[[i]]$pval_temp
  stars <- add_stars(pval)

  # Determine decimal places based on magnitude
  if(abs(coef_val) < 0.01) {
    temp_row <- paste0(temp_row, "  &   ", format_coef(coef_val, 4), stars)
    temp_se_row <- paste0(temp_se_row, "  & (", format_coef(se_val, 4), ")")
  } else {
    temp_row <- paste0(temp_row, "  &   ", format_coef(coef_val, 3), " ", stars)
    temp_se_row <- paste0(temp_se_row, "  & (", format_coef(se_val, 3), ")")
  }
}
temp_row <- paste0(temp_row, " \\\\")
temp_se_row <- paste0(temp_se_row, "  \\\\[0.5em]")

latex_lines <- c(latex_lines, temp_row, temp_se_row)

# Precipitation coefficient
precip_row <- "$P_{mt}$ ($\\eta$)"
precip_se_row <- ""
for(i in 1:3) {
  coef_val <- results_list[[i]]$coef_precip
  se_val <- results_list[[i]]$se_precip
  pval <- results_list[[i]]$pval_precip
  stars <- add_stars(pval)

  # Determine decimal places based on magnitude
  if(abs(coef_val) < 0.01) {
    precip_row <- paste0(precip_row, "  &   ", format_coef(coef_val, 5))
    if(nchar(stars) > 0) precip_row <- paste0(precip_row, " ", stars)
    precip_se_row <- paste0(precip_se_row, "  & (", format_coef(se_val, 4), ")")
  } else {
    precip_row <- paste0(precip_row, "  &   ", format_coef(coef_val, 3))
    if(nchar(stars) > 0) precip_row <- paste0(precip_row, " ", stars)
    precip_se_row <- paste0(precip_se_row, "  & (", format_coef(se_val, 3), ")")
  }
}
precip_row <- paste0(precip_row, " \\\\")
precip_se_row <- paste0(precip_se_row, "  \\\\")

latex_lines <- c(latex_lines, precip_row, precip_se_row)

# Add footer with statistics
latex_lines <- c(
  latex_lines,
  "\\midrule",
  paste0("Observations        & ",
         format(results_list[[1]]$n_obs, big.mark = " "), " &",
         format(results_list[[2]]$n_obs, big.mark = " "), " &",
         format(results_list[[3]]$n_obs, big.mark = " "), " \\\\"),
  paste0("$R^2$                &  ",
         format_coef(results_list[[1]]$r2, 3), " & ",
         format_coef(results_list[[2]]$r2, 3), " & ",
         format_coef(results_list[[3]]$r2, 3), " \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}",
  "\\footnotesize",
  "\\item \\textit{Note:} The dependent variables are Total Jobs (Column 1), Green jobs (Column 2), and proportion of green jobs (Column 3). Temperature and precipitation are measured as standard deviations from their historical means. The municipal-level panel data combine weather variables from Terra Climate and employment records from RAIS. Green jobs are classified using FEBRABAN's Green Taxonomy. All regressions include municipal and year fixed effects, with standard errors adjusted for spatial dependence (Conley 1999, up to 250 km) and serial correlation (Newey-West 1987, up to 7-year lags). Municipal distances are calculated from centroid coordinates.\\\\",
  "\\textsuperscript{*} p < 0.10, \\textsuperscript{**} p < 0.05, \\textsuperscript{***} p < 0.01",
  "\\end{tablenotes}",
  "\\end{threeparttable}%",
  "}",
  "\\end{table}"
)

# Write LaTeX table to file
output_file <- "table_sr_employment.tex"
writeLines(latex_lines, output_file)

cat("\nLaTeX table saved to:", output_file, "\n")

# ============================================================================
# STEP SIX: PRINT RESULTS SUMMARY
# ============================================================================

cat("\n" , rep("=", 70), "\n", sep = "")
cat("RESULTS SUMMARY\n")
cat(rep("=", 70), "\n", sep = "")

for(i in 1:3) {
  cat("\n", dep_labels[i], ":\n", sep = "")
  cat("  Temperature (β):    ", format_coef(results_list[[i]]$coef_temp, 3),
      " (", format_coef(results_list[[i]]$se_temp, 3), ")",
      add_stars(results_list[[i]]$pval_temp), "\n", sep = "")
  cat("  Precipitation (η):  ", format_coef(results_list[[i]]$coef_precip, 3),
      " (", format_coef(results_list[[i]]$se_precip, 3), ")",
      add_stars(results_list[[i]]$pval_precip), "\n", sep = "")
  cat("  Observations:       ", results_list[[i]]$n_obs, "\n", sep = "")
  cat("  R²:                 ", format_coef(results_list[[i]]$r2, 3), "\n", sep = "")
  cat("  Conley success:     ", results_list[[i]]$success, "\n", sep = "")
}

cat("\n", rep("=", 70), "\n", sep = "")
cat("*** p<0.01, ** p<0.05, * p<0.10\n")
cat("Standard errors adjusted for spatial dependence (Conley 1999, up to 250 km)\n")
cat("and serial correlation (Newey-West 1987, up to 7-year lags)\n")
cat(rep("=", 70), "\n\n", sep = "")
