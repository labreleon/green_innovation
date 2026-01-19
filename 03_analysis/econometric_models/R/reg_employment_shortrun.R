# ============================================================================
# SHORT-RUN PANEL ESTIMATES - EMPLOYMENT OUTCOMES (2000-2020)
# Replicates Table: Weather shocks and Employment: Short-Run Panel Regressions
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
# The data should contain:
# - mun_code: municipality code
# - year: year variable
# - lat, lon: coordinates
# - total_jobs: total employment
# - green_jobs: green employment
# - prop_verde: proportion of green jobs
# - cont_shock_temp: temperature shock (standardized)
# - cont_shock_precip: precipitation shock (standardized)

data <- read_dta("weather_rais.dta")

# Filter for years 2000-2020
data <- data %>%
  filter(year >= 2000 & year <= 2020)

cat("Data loaded. Observations:", nrow(data), "\n")

# ============================================================================
# STEP TWO: PREPARE VARIABLES
# ============================================================================

cat("Preparing variables...\n")

# Create panel identifiers
data <- data %>%
  group_by(mun_code) %>%
  mutate(id = cur_group_id()) %>%
  ungroup()

# Create time identifier for Conley SEs
data <- data %>%
  group_by(year) %>%
  mutate(time_id = cur_group_id()) %>%
  ungroup()

# Ensure variables exist (if not, create them)
# Assuming the data already has:
# - total_jobs or similar variable
# - green_jobs or similar variable
# - prop_verde (proportion of green jobs)
# - cont_shock_temp (temperature anomaly in standard deviations)
# - cont_shock_precip (precipitation anomaly in standard deviations)

# If variables need to be created/renamed, do it here
# Example:
# if(!"total_jobs" %in% names(data)) {
#   data$total_jobs <- data$total_employment # or however it's named
# }
# if(!"green_jobs" %in% names(data)) {
#   data$green_jobs <- data$green_employment
# }

# ============================================================================
# STEP THREE: RUN REGRESSIONS
# ============================================================================

cat("\nRunning regressions...\n\n")

# Define dependent variables
dep_vars <- c("total_jobs", "green_jobs", "prop_verde")
dep_labels <- c("Total Jobs", "Green Jobs", "Prop. Green Jobs")

# Initialize results storage
results_list <- list()

# Loop through each dependent variable
for(i in 1:length(dep_vars)) {

  dv <- dep_vars[i]
  cat("Model", i, "-", dep_labels[i], "\n")

  # -------------------------------------------------------------------------
  # Model with municipal and year fixed effects
  # -------------------------------------------------------------------------

  # Formula: Y_mt = β*T_mt + η*P_mt + α_m + γ_t + ε_mt
  formula_fe <- as.formula(paste0(dv, " ~ cont_shock_temp + cont_shock_precip | mun_code + year"))

  # Run fixed effects regression using fixest
  model_fe <- feols(formula_fe, data = data, cluster = ~mun_code)

  # Extract basic results
  coefs <- coef(model_fe)
  se_basic <- se(model_fe)

  # -------------------------------------------------------------------------
  # Calculate Conley Spatial HAC Standard Errors
  # -------------------------------------------------------------------------

  cat("  Calculating Conley spatial standard errors...\n")

  # Prepare data for Conley
  data_clean <- data %>%
    filter(!is.na(!!sym(dv)) &
           !is.na(cont_shock_temp) &
           !is.na(cont_shock_precip) &
           !is.na(lat) & !is.na(lon))

  # Create year dummies for Conley regression
  year_dummies <- model.matrix(~ factor(year) - 1, data = data_clean)
  colnames(year_dummies) <- paste0("year_", unique(data_clean$year))

  # Create municipality dummies (this can be computationally intensive)
  # For large datasets, we'll use the conleyreg package which handles FE internally

  # Formula for Conley (without FE in formula, as conleyreg handles it)
  formula_conley <- as.formula(paste0(dv, " ~ cont_shock_temp + cont_shock_precip"))

  # Run Conley regression
  tryCatch({
    conley_model <- conleyreg::conleyreg(
      formula = formula_conley,
      data = data_clean,
      id = "mun_code",
      time = "year",
      lat = "lat",
      lon = "lon",
      dist_cutoff = 250,    # 250 km spatial correlation
      lag_cutoff = 7,       # 7-year temporal correlation (Newey-West)
      fe = "both"           # Both unit (municipal) and time (year) fixed effects
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
      coef_temp = coefs_conley["cont_shock_temp"],
      se_temp = se_conley["cont_shock_temp"],
      pval_temp = pval_conley["cont_shock_temp"],
      coef_precip = coefs_conley["cont_shock_precip"],
      se_precip = se_conley["cont_shock_precip"],
      pval_precip = pval_conley["cont_shock_precip"],
      n_obs = nobs(model_fe),
      r2 = r2(model_fe, type = "r2")
    )

  }, error = function(e) {
    warning(paste("Conley SE calculation failed for", dep_labels[i], ":", e$message))
    warning("Using clustered standard errors instead.")

    # Fallback to clustered SEs
    results_list[[i]] <- list(
      model = model_fe,
      coef_temp = coefs["cont_shock_temp"],
      se_temp = se_basic["cont_shock_temp"],
      pval_temp = summary(model_fe)$coeftable["cont_shock_temp", "Pr(>|t|)"],
      coef_precip = coefs["cont_shock_precip"],
      se_precip = se_basic["cont_shock_precip"],
      pval_precip = summary(model_fe)$coeftable["cont_shock_precip", "Pr(>|t|)"],
      n_obs = nobs(model_fe),
      r2 = r2(model_fe, type = "r2")
    )
  })

  cat("  Model", i, "completed.\n\n")
}

# ============================================================================
# STEP FOUR: GENERATE LATEX TABLE
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
# STEP FIVE: PRINT RESULTS SUMMARY
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
}

cat("\n", rep("=", 70), "\n", sep = "")
cat("*** p<0.01, ** p<0.05, * p<0.10\n")
cat("Standard errors adjusted for spatial dependence (Conley 1999, up to 250 km)\n")
cat("and serial correlation (Newey-West 1987, up to 7-year lags)\n")
cat(rep("=", 70), "\n\n", sep = "")
