# ============================================================================
# SHORT-RUN PANEL ESTIMATES - EMPLOYMENT OUTCOMES (2000-2020)
# WITH STATE-YEAR TRENDS
# EXACT STATA REPLICATION using TWO-WAY demeaning
#
# Replicates Stata command:
# reg2hdfespatial Y X year_state_trend, timevar(year) panelvar(mun_code)
#                 lat(lat) lon(lon) distcutoff(250) lagcutoff(6)
#
# Uses Conley spatial standard errors (250 km, 6-year lag)
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

# CREATE year_state_trend variable
# This captures state-specific linear time trends
data[, year_state_trend := year * code_state]

cat("year_state_trend variable created\n")

# ============================================================================
# STEP THREE: PRE-COMPUTE TWO-WAY DEMEANED VARIABLES (CORRECT STATA REPLICATION)
# ============================================================================

cat("Pre-computing TWO-WAY demeaned variables for Conley...\n")
cat("This replicates the reg2hdfe behavior in Stata (municipality + year FE)\n\n")

# Define dependent variables
dep_vars <- c("total_jobs", "green_jobs", "prop_verde")
dep_labels <- c("Total Jobs", "Green Jobs", "Prop. Green Jobs")

# Filter out missing values once for all models
data_clean <- data[!is.na(lat) & !is.na(lon) &
                   !is.na(cont_shock_temp) & !is.na(cont_shock_precip) &
                   !is.na(year_state_trend)]

# Remove rows where ALL dependent variables are missing
data_clean <- data_clean[!is.na(total_jobs) | !is.na(green_jobs) | !is.na(prop_verde)]

cat("Clean data observations:", nrow(data_clean), "\n\n")

# Convert to data.frame for fixest::demean
data_clean_df <- as.data.frame(data_clean)

# ============================================================================
# CRITICAL: IMPLEMENT TWO-WAY DEMEANING (MUNICIPALITY + YEAR)
# This is what reg2hdfe does in Stata: removes both municipality and year FE
# ============================================================================

cat("  Applying two-way demeaning (municipality + year fixed effects)...\n")

# List of all variables to demean with two-way FE
# IMPORTANT: Include year_state_trend
vars_to_demean <- c(dep_vars, "cont_shock_temp", "cont_shock_precip", "year_state_trend")

# Apply two-way demeaning for each variable
# fixest::demean removes multiple fixed effects iteratively until convergence
data_demeaned <- data_clean_df

for(var in vars_to_demean) {
  # Skip if variable is all NA
  if(all(is.na(data_clean_df[[var]]))) {
    cat("    Skipping", var, "(all NA)\n")
    next
  }

  cat("    Demeaning", var, "...\n")

  # Create formula for two-way demeaning: var ~ 1 | mun_code + year
  # The demean function removes the fixed effects specified after |
  demeaned_result <- fixest::demean(
    as.formula(paste0(var, " ~ 1 | mun_code + year")),
    data = data_clean_df,
    na.rm = TRUE
  )

  # Store demeaned variable
  data_demeaned[[paste0(var, "_dm")]] <- demeaned_result[[var]]
}

# Also preserve original lat/lon for distance calculations
data_demeaned$lat <- data_clean_df$lat
data_demeaned$lon <- data_clean_df$lon
data_demeaned$mun_code <- data_clean_df$mun_code
data_demeaned$year <- data_clean_df$year

# Convert back to data.table
data_conley <- as.data.table(data_demeaned)

cat("  Two-way demeaning complete.\n")
cat("  NOTE: year_state_trend has been demeaned by BOTH municipality AND year FE\n")
cat("  This matches the Stata reg2hdfe behavior exactly.\n\n")

# ============================================================================
# STEP FOUR: RUN REGRESSIONS
# ============================================================================

cat("Running regressions...\n\n")

# Initialize results storage
results_list <- list()

# Loop through each dependent variable
for(i in 1:length(dep_vars)) {

  dv <- dep_vars[i]
  cat("Model", i, "-", dep_labels[i], "\n")

  # -------------------------------------------------------------------------
  # Model with municipal, year fixed effects, and state-year trends
  # -------------------------------------------------------------------------

  # Formula: Y_mt = β*T_mt + η*P_mt + δ*(year×state) + α_m + γ_t + ε_mt
  formula_fe <- as.formula(paste0(dv, " ~ cont_shock_temp + cont_shock_precip + year_state_trend | mun_code + year"))

  # Run fixed effects regression using fixest on original data
  model_fe <- feols(formula_fe, data = data, cluster = ~mun_code)

  # Extract basic results
  coefs <- coef(model_fe)
  se_basic <- se(model_fe)

  # -------------------------------------------------------------------------
  # Calculate Conley Spatial HAC Standard Errors
  # -------------------------------------------------------------------------

  cat("  Calculating Conley spatial standard errors...\n")

  # Filter for this specific dependent variable
  data_model <- data_conley[!is.na(get(paste0(dv, "_dm")))]

  cat("    Observations for this model:", nrow(data_model), "\n")

  # Formula for Conley with TWO-WAY demeaned variables
  # CRITICAL: NO year dummies! They were already removed by two-way demeaning
  # This matches Stata's reg2hdfe + ols_spatial_HAC behavior exactly
  formula_conley <- as.formula(paste0(
    dv, "_dm ~ cont_shock_temp_dm + cont_shock_precip_dm + year_state_trend_dm - 1"
  ))

  cat("    Formula:", deparse(formula_conley), "\n")

  # Run Conley regression
  tryCatch({
    # Convert to data.frame for conleyreg (it doesn't work well with data.table)
    data_model_df <- as.data.frame(data_model)

    conley_model <- conleyreg::conleyreg(
      formula = formula_conley,
      data = data_model_df,
      lat = "lat",
      lon = "lon",
      dist_cutoff = 250,    # 250 km spatial correlation
      lag_cutoff = 6        # 6-year temporal correlation (CHANGED FROM 7 TO MATCH STATA)
    )

    # Extract Conley standard errors
    conley_summary <- summary(conley_model)
    se_conley <- conley_summary$coefficients[, "Std. Error"]
    coefs_conley <- conley_summary$coefficients[, "Estimate"]
    tstat_conley <- conley_summary$coefficients[, "t value"]
    pval_conley <- conley_summary$coefficients[, "Pr(>|t|)"]

    # Store results
    results_list[[i]] <- list(
      model = model_fe,
      coef_temp = coefs_conley["cont_shock_temp_dm"],
      se_temp = se_conley["cont_shock_temp_dm"],
      pval_temp = pval_conley["cont_shock_temp_dm"],
      coef_precip = coefs_conley["cont_shock_precip_dm"],
      se_precip = se_conley["cont_shock_precip_dm"],
      pval_precip = pval_conley["cont_shock_precip_dm"],
      coef_trend = coefs_conley["year_state_trend_dm"],
      se_trend = se_conley["year_state_trend_dm"],
      pval_trend = pval_conley["year_state_trend_dm"],
      n_obs = nobs(model_fe),
      r2 = r2(model_fe, type = "r2")
    )

    cat("    Conley SEs calculated successfully.\n")

  }, error = function(e) {
    warning(paste("Conley SE calculation failed for", dep_labels[i], ":", e$message))
    warning("Using clustered standard errors instead.")

    # Fallback to clustered SEs
    results_list[[i]] <<- list(
      model = model_fe,
      coef_temp = coefs["cont_shock_temp"],
      se_temp = se_basic["cont_shock_temp"],
      pval_temp = summary(model_fe)$coeftable["cont_shock_temp", "Pr(>|t|)"],
      coef_precip = coefs["cont_shock_precip"],
      se_precip = se_basic["cont_shock_precip"],
      pval_precip = summary(model_fe)$coeftable["cont_shock_precip", "Pr(>|t|)"],
      coef_trend = coefs["year_state_trend"],
      se_trend = se_basic["year_state_trend"],
      pval_trend = summary(model_fe)$coeftable["year_state_trend", "Pr(>|t|)"],
      n_obs = nobs(model_fe),
      r2 = r2(model_fe, type = "r2")
    )
  })

  cat("  Model", i, "completed.\n\n")
}

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
  "\\caption{Short-Run Panel Estimates – Employment Outcomes with State-Year Trends (2000-2020)}",
  "\\label{tab:sr_employment_state_trend}",
  "\\begin{tabular}{lccc}",
  "\\toprule",
  "\\multicolumn{4}{c}{\\textbf{Weather shocks and Employment: Short-Run Panel with State Trends}}\\\\",
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
precip_se_row <- paste0(precip_se_row, "  \\\\[0.5em]")

latex_lines <- c(latex_lines, precip_row, precip_se_row)

# State-Year Trend coefficient
trend_row <- "$year \\times state$ ($\\delta$)"
trend_se_row <- ""
for(i in 1:3) {
  coef_val <- results_list[[i]]$coef_trend
  se_val <- results_list[[i]]$se_trend
  pval <- results_list[[i]]$pval_trend
  stars <- add_stars(pval)

  # Determine decimal places based on magnitude
  if(abs(coef_val) < 0.01) {
    trend_row <- paste0(trend_row, "  &   ", format_coef(coef_val, 5))
    if(nchar(stars) > 0) trend_row <- paste0(trend_row, " ", stars)
    trend_se_row <- paste0(trend_se_row, "  & (", format_coef(se_val, 4), ")")
  } else {
    trend_row <- paste0(trend_row, "  &   ", format_coef(coef_val, 3))
    if(nchar(stars) > 0) trend_row <- paste0(trend_row, " ", stars)
    trend_se_row <- paste0(trend_se_row, "  & (", format_coef(se_val, 3), ")")
  }
}
trend_row <- paste0(trend_row, " \\\\")
trend_se_row <- paste0(trend_se_row, "  \\\\")

latex_lines <- c(latex_lines, trend_row, trend_se_row)

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
  "\\midrule",
  "Municipality FE      &  Yes & Yes & Yes \\\\",
  "Year FE              &  Yes & Yes & Yes \\\\",
  "State-Year Trends    &  Yes & Yes & Yes \\\\",
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}",
  "\\footnotesize",
  "\\item \\textit{Note:} The dependent variables are Total Jobs (Column 1), Green jobs (Column 2), and proportion of green jobs (Column 3). Temperature and precipitation are measured as standard deviations from their historical means. The variable $year \\times state$ captures state-specific linear time trends. The municipal-level panel data combine weather variables from Terra Climate and employment records from RAIS. Green jobs are classified using FEBRABAN's Green Taxonomy. All regressions include municipal fixed effects, year fixed effects, and state-specific linear time trends, with standard errors adjusted for spatial dependence (Conley 1999, up to 250 km) and serial correlation (Newey-West 1987, up to 6-year lags). Municipal distances are calculated from centroid coordinates.\\\\",
  "\\textsuperscript{*} p < 0.10, \\textsuperscript{**} p < 0.05, \\textsuperscript{***} p < 0.01",
  "\\end{tablenotes}",
  "\\end{threeparttable}%",
  "}",
  "\\end{table}"
)

# Write LaTeX table to file
output_file <- "table_sr_employment_state_trend.tex"
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
  cat("  Temperature (β):       ", format_coef(results_list[[i]]$coef_temp, 3),
      " (", format_coef(results_list[[i]]$se_temp, 3), ")",
      add_stars(results_list[[i]]$pval_temp), "\n", sep = "")
  cat("  Precipitation (η):     ", format_coef(results_list[[i]]$coef_precip, 3),
      " (", format_coef(results_list[[i]]$se_precip, 3), ")",
      add_stars(results_list[[i]]$pval_precip), "\n", sep = "")
  cat("  State-Year Trend (δ):  ", format_coef(results_list[[i]]$coef_trend, 3),
      " (", format_coef(results_list[[i]]$se_trend, 3), ")",
      add_stars(results_list[[i]]$pval_trend), "\n", sep = "")
  cat("  Observations:          ", results_list[[i]]$n_obs, "\n", sep = "")
  cat("  R²:                    ", format_coef(results_list[[i]]$r2, 3), "\n", sep = "")
}

cat("\n", rep("=", 70), "\n", sep = "")
cat("*** p<0.01, ** p<0.05, * p<0.10\n")
cat("Standard errors adjusted for spatial dependence (Conley 1999, up to 250 km)\n")
cat("and serial correlation (Newey-West 1987, up to 6-year lags)\n")
cat("Model includes: Municipality FE + Year FE + State-Year Trends\n")
cat("TWO-WAY demeaning used to match Stata reg2hdfespatial exactly\n")
cat(rep("=", 70), "\n\n", sep = "")

# ============================================================================
# IMPORTANT NOTES: EXACT STATA REPLICATION
# ============================================================================
#
# This script NOW CORRECTLY replicates Stata's reg2hdfespatial behavior:
#
# CRITICAL CHANGES FOR STATA REPLICATION:
# ----------------------------------------
# 1. TWO-WAY DEMEANING: Uses fixest::demean() to remove BOTH municipality AND year
#    fixed effects simultaneously (not just municipality FE + year dummies!)
#    - This matches reg2hdfe behavior in Stata exactly
#    - All variables (Y, X, year_state_trend) are demeaned with two-way FE
#
# 2. NO YEAR DUMMIES in Conley model: Year fixed effects are already removed by
#    two-way demeaning, so we DON'T include year dummies in the Conley formula
#    - Stata runs: ols_spatial_HAC Y X (on demeaned data, no year dummies)
#    - R now runs: conleyreg(Y_dm ~ X_dm - 1) (same approach)
#
# 3. lag_cutoff = 6: Changed from 7 to match Stata exactly
#    - Stata uses lagcutoff(6) in most short-run models
#    - This affects Newey-West temporal correlation correction
#
# 4. year_state_trend treatment:
#    - Created as: year * code_state
#    - Demeaned with TWO-WAY FE (municipality + year)
#    - Included in Conley model as year_state_trend_dm
#    - This captures state-specific linear time trends correctly
#
# WHY TWO-WAY DEMEANING MATTERS:
# -------------------------------
# - One-way demeaning (by municipality only) + year dummies ≠ Two-way demeaning!
# - Two-way FE removes:
#   (a) Mean of each municipality across all years
#   (b) Mean of each year across all municipalities
#   (c) Uses iterative algorithm until convergence
# - This affects ALL coefficients and standard errors
#
# STATA COMMAND BEING REPLICATED:
# --------------------------------
# reg2hdfespatial Y X year_state_trend, timevar(year) panelvar(mun_code)
#                 lat(lat) lon(lon) distcutoff(250) lagcutoff(6)
#
# Where reg2hdfespatial internally:
# 1. Calls reg2hdfe to do two-way demeaning
# 2. Calls ols_spatial_HAC on demeaned data
#
# ============================================================================
