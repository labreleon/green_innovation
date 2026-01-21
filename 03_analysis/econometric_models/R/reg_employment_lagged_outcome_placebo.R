# ============================================================================
# LAGGED OUTCOME PLACEBO TEST - EMPLOYMENT OUTCOMES
# Reverse Causality Test: Current Shocks Predicting Past Outcomes
# ============================================================================
#
# OBJETIVO: Testar se choques climaticos ATUAIS predizem outcomes PASSADOS.
# Se sim, indica correlacao espuria (futuro nao pode causar passado).
#
# MODELO:
#   Y_{t-k} = beta_1 * Shock_t + beta_2 * Precip_t + alpha_i + gamma_t + epsilon
#
# LAGS TESTADOS: k = 1, 5, 10, 20 anos
#
# H0: beta = 0 (choques atuais NAO afetam outcomes passados)
# H1: beta != 0 (correlacao espuria - problema de identificacao)
#
# ============================================================================

# Load required packages
library(haven)        # For reading Stata files
library(dplyr)        # For data manipulation
library(fixest)       # For high-dimensional fixed effects
library(data.table)   # For efficient data manipulation
library(knitr)        # For table formatting

# ============================================================================
# CONFIGURATION
# ============================================================================

# Outcome lags to test (in years)
OUTCOME_LAGS <- c(1, 5, 10, 20)

# Significance level
ALPHA <- 0.05

# ============================================================================
# STEP 1: LOAD AND PREPARE DATA
# ============================================================================

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("LAGGED OUTCOME PLACEBO TEST - EMPLOYMENT OUTCOMES\n")
cat("Testing if CURRENT shocks predict PAST outcomes\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

cat("Loading employment data...\n")

# Load data
data <- read_dta("./output/final_base/weather_rais.dta")
data <- as.data.table(data)

# Filter for years with sufficient data (need room for lags)
data <- data[year >= 1985 & year <= 2020]

cat("Data loaded. Observations:", nrow(data), "\n")

# ============================================================================
# STEP 2: PREPARE VARIABLES
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

# Create state-year trend
data[, year_state_trend := year * code_state]
data[, state_year := interaction(code_state, year, drop = TRUE)]

# Order data for proper lag creation
setorderv(data, c("mun_code", "year"))

# Define dependent variables
dep_vars <- c("total_jobs", "green_jobs", "prop_verde")
dep_labels <- c("Total Jobs", "Green Jobs", "Prop. Green Jobs")

cat("Variables prepared.\n")
cat("Municipalities:", length(unique(data$mun_code)), "\n")
cat("Years:", paste(range(data$year), collapse = "-"), "\n\n")

# ============================================================================
# STEP 3: CREATE LAGGED OUTCOME VARIABLES
# ============================================================================

cat("Creating lagged outcome variables...\n")

# Create lagged outcomes for each dependent variable and each lag
for (dep_var in dep_vars) {
  for (k in OUTCOME_LAGS) {
    lag_name <- paste0(dep_var, "_lag", k)
    data[, (lag_name) := shift(get(dep_var), n = k, type = "lag"), by = mun_code]
    cat("  Created:", lag_name, "\n")
  }
}

cat("\n")

# ============================================================================
# STEP 4: PREPARE CLEAN DATA
# ============================================================================

cat("Preparing clean data...\n")

data_clean <- data[!is.na(cont_shock_temp) & !is.na(cont_shock_precip)]

cat("Clean data observations:", nrow(data_clean), "\n\n")

# ============================================================================
# STEP 5: RUN PLACEBO REGRESSIONS
# ============================================================================

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("RUNNING PLACEBO REGRESSIONS\n")
cat("Model: Y_{t-k} ~ Shock_t + Precip_t | mun_code + year\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

# Storage for all results
all_results <- list()
results_table <- data.frame()

for (i in seq_along(dep_vars)) {
  dep_var <- dep_vars[i]
  dep_label <- dep_labels[i]

  cat("\n")
  cat("#" %>% rep(60) %>% paste(collapse = ""), "\n")
  cat("# Outcome:", dep_label, "\n")
  cat("#" %>% rep(60) %>% paste(collapse = ""), "\n\n")

  cat(sprintf("%-15s %12s %12s %12s %12s %10s\n",
              "Outcome Lag", "Temp Coef", "Temp SE", "Precip Coef", "Precip SE", "N Obs"))
  cat("-" %>% rep(75) %>% paste(collapse = ""), "\n")

  var_results <- list()

  for (k in OUTCOME_LAGS) {
    lag_var <- paste0(dep_var, "_lag", k)

    # Check if lagged variable has sufficient observations
    n_valid <- sum(!is.na(data_clean[[lag_var]]) & !is.na(data_clean$cont_shock_temp))
    if (n_valid < 500) {
      cat(sprintf("%-15s Insufficient observations (%d). Skipping.\n",
                  paste0("Y_{t-", k, "}"), n_valid))
      next
    }

    # Run regression: Lagged outcome ~ Current shocks | FE
    formula_placebo <- as.formula(paste0(
      lag_var, " ~ cont_shock_temp + cont_shock_precip | mun_code + year"
    ))

    tryCatch({
      # Model with municipality clustering
      model <- feols(formula_placebo, data = data_clean, cluster = ~mun_code)

      coef_temp <- coef(model)["cont_shock_temp"]
      se_temp <- se(model)["cont_shock_temp"]
      pval_temp <- pvalue(model)["cont_shock_temp"]

      coef_precip <- coef(model)["cont_shock_precip"]
      se_precip <- se(model)["cont_shock_precip"]
      pval_precip <- pvalue(model)["cont_shock_precip"]

      # Significance indicators
      sig_temp <- ifelse(pval_temp < 0.01, "***",
                         ifelse(pval_temp < 0.05, "**",
                                ifelse(pval_temp < 0.10, "*", "")))
      sig_precip <- ifelse(pval_precip < 0.01, "***",
                           ifelse(pval_precip < 0.05, "**",
                                  ifelse(pval_precip < 0.10, "*", "")))

      # Print results
      cat(sprintf("%-15s %11.4f%s %12.4f %11.4f%s %12.4f %10d\n",
                  paste0("Y_{t-", k, "}"),
                  coef_temp, sig_temp, se_temp,
                  coef_precip, sig_precip, se_precip,
                  nobs(model)))

      # Store results
      var_results[[paste0("lag", k)]] <- model

      results_table <- rbind(results_table, data.frame(
        outcome = dep_label,
        outcome_lag = k,
        coef_temp = coef_temp,
        se_temp = se_temp,
        pval_temp = pval_temp,
        coef_precip = coef_precip,
        se_precip = se_precip,
        pval_precip = pval_precip,
        n_obs = nobs(model),
        r2 = r2(model, type = "r2")
      ))

    }, error = function(e) {
      cat(sprintf("%-15s Error: %s\n", paste0("Y_{t-", k, "}"), e$message))
    })
  }

  cat("-" %>% rep(75) %>% paste(collapse = ""), "\n")
  cat("Significance: *** p<0.01, ** p<0.05, * p<0.10\n")

  all_results[[dep_var]] <- var_results
}

# ============================================================================
# STEP 6: ADDITIONAL TEST WITH STATE-YEAR TRENDS
# ============================================================================

cat("\n\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("ROBUSTNESS: WITH STATE-YEAR TRENDS\n")
cat("Model: Y_{t-k} ~ Shock_t + Precip_t + year_state_trend | mun_code + year\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

results_table_trend <- data.frame()

for (i in seq_along(dep_vars)) {
  dep_var <- dep_vars[i]
  dep_label <- dep_labels[i]

  cat("\n# Outcome:", dep_label, "\n")
  cat(sprintf("%-15s %12s %12s %10s\n",
              "Outcome Lag", "Temp Coef", "Temp SE", "N Obs"))
  cat("-" %>% rep(50) %>% paste(collapse = ""), "\n")

  for (k in OUTCOME_LAGS) {
    lag_var <- paste0(dep_var, "_lag", k)

    n_valid <- sum(!is.na(data_clean[[lag_var]]) & !is.na(data_clean$cont_shock_temp) &
                     !is.na(data_clean$year_state_trend))
    if (n_valid < 500) next

    formula_trend <- as.formula(paste0(
      lag_var, " ~ cont_shock_temp + cont_shock_precip + year_state_trend | mun_code + year"
    ))

    tryCatch({
      model <- feols(formula_trend, data = data_clean, cluster = ~mun_code)

      coef_temp <- coef(model)["cont_shock_temp"]
      se_temp <- se(model)["cont_shock_temp"]
      pval_temp <- pvalue(model)["cont_shock_temp"]

      sig_temp <- ifelse(pval_temp < 0.01, "***",
                         ifelse(pval_temp < 0.05, "**",
                                ifelse(pval_temp < 0.10, "*", "")))

      cat(sprintf("%-15s %11.4f%s %12.4f %10d\n",
                  paste0("Y_{t-", k, "}"),
                  coef_temp, sig_temp, se_temp,
                  nobs(model)))

      results_table_trend <- rbind(results_table_trend, data.frame(
        outcome = dep_label,
        outcome_lag = k,
        coef_temp = coef_temp,
        se_temp = se_temp,
        pval_temp = pval_temp,
        n_obs = nobs(model)
      ))

    }, error = function(e) {
      cat(sprintf("%-15s Error: %s\n", paste0("Y_{t-", k, "}"), e$message))
    })
  }
}

# ============================================================================
# STEP 7: SUMMARY AND INTERPRETATION
# ============================================================================

cat("\n\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("SUMMARY: LAGGED OUTCOME PLACEBO TEST - EMPLOYMENT\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

# Count significant results
n_tests <- nrow(results_table)
n_sig_temp <- sum(results_table$pval_temp < ALPHA)
n_sig_precip <- sum(results_table$pval_precip < ALPHA)

cat("BASELINE MODEL (Mun + Year FE):\n")
cat("-" %>% rep(40) %>% paste(collapse = ""), "\n")
cat("Total tests conducted:", n_tests, "\n")
cat("Significant temperature coefficients (p<0.05):", n_sig_temp, "\n")
cat("Significant precipitation coefficients (p<0.05):", n_sig_precip, "\n\n")

# Bonferroni correction
bonf_alpha <- ALPHA / n_tests
n_sig_temp_bonf <- sum(results_table$pval_temp < bonf_alpha)
n_sig_precip_bonf <- sum(results_table$pval_precip < bonf_alpha)

cat("Bonferroni-corrected alpha:", sprintf("%.4f", bonf_alpha), "\n")
cat("Significant after Bonferroni (temp):", n_sig_temp_bonf, "\n")
cat("Significant after Bonferroni (precip):", n_sig_precip_bonf, "\n\n")

# Results with state-year trends
if (nrow(results_table_trend) > 0) {
  n_sig_trend <- sum(results_table_trend$pval_temp < ALPHA)
  cat("WITH STATE-YEAR TRENDS:\n")
  cat("-" %>% rep(40) %>% paste(collapse = ""), "\n")
  cat("Significant temperature coefficients (p<0.05):", n_sig_trend, "\n\n")
}

# Interpretation
cat("INTERPRETATION:\n")
cat("-" %>% rep(50) %>% paste(collapse = ""), "\n")

if (n_sig_temp_bonf == 0 && n_sig_precip_bonf == 0) {
  cat(">> PASS: No coefficients significant after Bonferroni correction.\n")
  cat("   Current climate shocks do NOT predict past employment outcomes.\n")
  cat("   This supports the causal interpretation of the main results.\n")
} else if (n_sig_temp + n_sig_precip <= 2) {
  cat(">> MARGINAL: Few significant results (", n_sig_temp + n_sig_precip, " of ", n_tests * 2, ").\n", sep = "")
  cat("   May be due to multiple testing. Results mostly support identification.\n")
} else {
  cat(">> WARNING: Multiple significant placebo coefficients detected!\n")
  cat("   Current shocks appear to 'predict' past outcomes.\n")
  cat("   This suggests potential issues:\n")
  cat("     - Correlated trends between climate and employment\n")
  cat("     - Serial correlation not fully addressed\n")
  cat("     - Omitted time-varying confounders\n")
  cat("   Consider additional robustness checks.\n")
}

cat("\n")

# Print detailed results table
cat("\nDETAILED RESULTS TABLE (Baseline):\n")
cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
print(results_table[, c("outcome", "outcome_lag", "coef_temp", "pval_temp",
                        "coef_precip", "pval_precip", "n_obs")], row.names = FALSE)

# ============================================================================
# STEP 8: SAVE RESULTS
# ============================================================================

# Create output directory if it doesn't exist
if (!dir.exists("./output/placebo_tests")) {
  dir.create("./output/placebo_tests", recursive = TRUE)
}

# Save results
save(all_results, results_table, results_table_trend,
     file = "./output/placebo_tests/employment_lagged_outcome_placebo.RData")

# Save CSV for easy viewing
write.csv(results_table,
          file = "./output/placebo_tests/employment_lagged_outcome_placebo.csv",
          row.names = FALSE)

if (nrow(results_table_trend) > 0) {
  write.csv(results_table_trend,
            file = "./output/placebo_tests/employment_lagged_outcome_placebo_with_trend.csv",
            row.names = FALSE)
}

cat("\nResults saved to:\n")
cat("  - ./output/placebo_tests/employment_lagged_outcome_placebo.RData\n")
cat("  - ./output/placebo_tests/employment_lagged_outcome_placebo.csv\n")
cat("  - ./output/placebo_tests/employment_lagged_outcome_placebo_with_trend.csv\n")

cat("\nScript completed.\n")
