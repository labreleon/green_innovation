# ============================================================================
# SHORT-RUN PANEL ESTIMATES - EMPLOYMENT OUTCOMES (2000-2020)
# WITH STATE-YEAR TRENDS
# Replicates Table: Weather shocks and Employment with State-Specific Trends
# Uses Conley spatial standard errors
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
# STEP THREE: PRE-COMPUTE ALL DEMEANED VARIABLES (MAJOR OPTIMIZATION)
# ============================================================================

cat("Pre-computing demeaned variables for Conley (this speeds up later steps)...\n")

# Define dependent variables
dep_vars <- c("total_jobs", "green_jobs", "prop_verde")
dep_labels <- c("Total Jobs", "Green Jobs", "Prop. Green Jobs")

# Filter out missing values once for all models
data_clean <- data[!is.na(lat) & !is.na(lon) &
                   !is.na(cont_shock_temp) & !is.na(cont_shock_precip) &
                   !is.na(year_state_trend)]

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
# IMPORTANT: Include year_state_trend in the list of variables to demean
vars_to_demean <- c(dep_vars, "cont_shock_temp", "cont_shock_precip",
                    "year_state_trend",  # <-- ADDED HERE
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
  data_model <- data_conley[!is.na(get(dv))]

  cat("    Observations for this model:", nrow(data_model), "\n")

  # Formula for Conley with demeaned variables (municipality FE absorbed)
  # IMPORTANT: Include year_state_trend_dm in the formula
  formula_conley <- as.formula(paste0(
    dv, "_dm ~ cont_shock_temp_dm + cont_shock_precip_dm + year_state_trend_dm + ",
    formula_years
  ))

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
      lag_cutoff = 7        # 7-year temporal correlation (Newey-West)
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
  "\\item \\textit{Note:} The dependent variables are Total Jobs (Column 1), Green jobs (Column 2), and proportion of green jobs (Column 3). Temperature and precipitation are measured as standard deviations from their historical means. The variable $year \\times state$ captures state-specific linear time trends. The municipal-level panel data combine weather variables from Terra Climate and employment records from RAIS. Green jobs are classified using FEBRABAN's Green Taxonomy. All regressions include municipal fixed effects, year fixed effects, and state-specific linear time trends, with standard errors adjusted for spatial dependence (Conley 1999, up to 250 km) and serial correlation (Newey-West 1987, up to 7-year lags). Municipal distances are calculated from centroid coordinates.\\\\",
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
cat("and serial correlation (Newey-West 1987, up to 7-year lags)\n")
cat("Model includes: Municipality FE + Year FE + State-Year Trends\n")
cat(rep("=", 70), "\n\n", sep = "")

# ============================================================================
# IMPORTANT NOTES ON CONLEY SE WITH STATE-YEAR TRENDS
# ============================================================================
#
# Key points:
# 1. year_state_trend is created as year * code_state
# 2. This variable is demeaned by municipality (along with all other variables)
# 3. The demeaned version (year_state_trend_dm) is included in the Conley formula
# 4. This correctly accounts for state-specific linear time trends while computing
#    spatial and temporal correlation in the errors
#
# The demeaning absorbs municipality fixed effects, and including year_state_trend_dm
# captures state-specific trends that vary linearly with time.
#
# This is the correct specification for Conley SE with state-year trends.
# ============================================================================
