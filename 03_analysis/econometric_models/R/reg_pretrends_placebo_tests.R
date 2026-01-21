# ============================================================================
# PRE-TRENDS AND PLACEBO TESTS FOR CLIMATE SHOCK IDENTIFICATION
# Tests for Dynamic Specification and Causal Identification
# ============================================================================
#
# Este script implementa testes de pre-trends e placebo para validar a
# estrategia de identificacao baseada em choques climaticos exogenos.
#
# TESTES IMPLEMENTADOS:
#
# 1. PRE-TREND TESTS COM LEADS (Choques Futuros)
#    - Testa se choques futuros afetam outcomes atuais (nao deveriam)
#    - Se beta_lead != 0, indica problema de tendencias pre-existentes
#
# 2. TESTE EM PERIODOS DE BAIXA VARIACAO CLIMATICA
#    - Identifica periodos com menor variacao nos choques
#    - Verifica se ha tendencias espurias nesses periodos
#
# 3. PLACEBO TEMPORAL (Choques Distantes)
#    - Usa choques muito defasados (t-15, t-20) como placebo
#    - Choques antigos nao deveriam afetar outcomes atuais
#
# 4. PLACEBO COM PERMUTACAO ALEATORIA
#    - Permuta choques entre municipios aleatoriamente
#    - Distribuicao sob H0 (sem efeito verdadeiro)
#
# 5. FALSIFICATION TEST COM OUTCOMES NAO-AFETADOS
#    - Testa efeitos em variaveis que nao deveriam responder a clima
#
# 6. PLACEBO COM OUTCOMES PASSADOS (REVERSE CAUSALITY)
#    - Usa choques atuais para prever outcomes PASSADOS
#    - Se Shock_t afeta Y_{t-k}, ha problema de identificacao
#    - Teste fundamental: causa nao pode preceder efeito
#
# ============================================================================

# Load required packages
library(haven)        # For reading Stata files
library(dplyr)        # For data manipulation
library(fixest)       # For high-dimensional fixed effects
library(data.table)   # For efficient data manipulation
library(sandwich)     # For robust standard errors
library(lmtest)       # For coefficient testing
library(ggplot2)      # For visualization

# ============================================================================
# CONFIGURATION
# ============================================================================

# Set seed for reproducibility in permutation tests
set.seed(12345)

# Number of permutations for placebo inference
N_PERMUTATIONS <- 500

# Significance level
ALPHA <- 0.05

# ============================================================================
# STEP 1: LOAD AND PREPARE DATA
# ============================================================================

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("LOADING DATA\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

# Load employment data (primary outcome)
if (file.exists("./output/final_base/weather_rais.dta")) {
  data_emp <- read_dta("./output/final_base/weather_rais.dta")
  data_emp <- as.data.table(data_emp)
  cat("Employment data loaded:", nrow(data_emp), "observations\n")
} else {
  cat("Note: Employment data not found. Skipping employment tests.\n")
  data_emp <- NULL
}

# Load patent data
if (file.exists("./output/final_base/weather_patent.dta")) {
  data_pat <- read_dta("./output/final_base/weather_patent.dta")
  data_pat <- as.data.table(data_pat)
  cat("Patent data loaded:", nrow(data_pat), "observations\n")
} else {
  cat("Note: Patent data not found. Skipping patent tests.\n")
  data_pat <- NULL
}

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Create lead variables for pre-trend testing
#' @param dt data.table with panel data
#' @param var variable name to create leads for
#' @param leads vector of lead periods (e.g., c(1, 2, 3))
#' @param panel_var panel identifier variable
#' @param time_var time variable
create_leads <- function(dt, var, leads, panel_var = "mun_code", time_var = "year") {
  dt <- copy(dt)
  setorderv(dt, c(panel_var, time_var))

  for (l in leads) {
    lead_name <- paste0("lead", l, "_", var)
    dt[, (lead_name) := shift(get(var), n = -l, type = "lead"), by = get(panel_var)]
  }

  return(dt)
}

#' Create lag variables for dynamic analysis
#' @param dt data.table with panel data
#' @param var variable name to create lags for
#' @param lags vector of lag periods
#' @param panel_var panel identifier variable
#' @param time_var time variable
create_lags <- function(dt, var, lags, panel_var = "mun_code", time_var = "year") {
  dt <- copy(dt)
  setorderv(dt, c(panel_var, time_var))

  for (l in lags) {
    lag_name <- paste0("lag", l, "_", var)
    dt[, (lag_name) := shift(get(var), n = l, type = "lag"), by = get(panel_var)]
  }

  return(dt)
}

#' Calculate climate variability by period
#' @param dt data.table with climate shock data
#' @param shock_var shock variable name
#' @param time_var time variable
calculate_variability <- function(dt, shock_var, time_var = "year") {
  variability <- dt[, .(
    sd_shock = sd(get(shock_var), na.rm = TRUE),
    mean_shock = mean(get(shock_var), na.rm = TRUE),
    cv_shock = sd(get(shock_var), na.rm = TRUE) / abs(mean(get(shock_var), na.rm = TRUE))
  ), by = get(time_var)]

  setnames(variability, "get", time_var)
  return(variability)
}

#' Format regression results with significance stars
format_results <- function(coef, se, pval) {
  stars <- ifelse(pval < 0.01, "***",
                  ifelse(pval < 0.05, "**",
                         ifelse(pval < 0.10, "*", "")))
  sprintf("%.4f%s (%.4f)", coef, stars, se)
}

# ============================================================================
# TEST 1: PRE-TREND TESTS COM LEADS (CHOQUES FUTUROS)
# ============================================================================
#
# CONCEITO: Se a identificacao esta correta, choques climaticos FUTUROS
# nao deveriam afetar outcomes PRESENTES. Testamos:
#
#   Y_t = beta_0 + beta_1*Shock_t + gamma_1*Shock_{t+1} + gamma_2*Shock_{t+2} + ... + epsilon
#
# H0: gamma_1 = gamma_2 = ... = 0 (sem pre-trends)
# H1: gamma_j != 0 para algum j (pre-trends presentes)
#
# ============================================================================

run_pretrend_leads_test <- function(data, dep_var, shock_var = "cont_shock_temp",
                                     leads = c(1, 2, 3), label = "") {

  cat("\n", "=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("TEST 1: PRE-TREND WITH LEADS -", label, "\n")
  cat("Dependent Variable:", dep_var, "\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

  if (is.null(data)) {
    cat("Data not available. Skipping test.\n")
    return(NULL)
  }

  dt <- copy(data)

  # Create lead variables
  dt <- create_leads(dt, shock_var, leads)
  dt <- create_leads(dt, "cont_shock_precip", leads)

  # Build formula with leads
  lead_vars_temp <- paste0("lead", leads, "_", shock_var)
  lead_vars_precip <- paste0("lead", leads, "_cont_shock_precip")

  # Check which lead variables exist
  lead_vars_temp <- lead_vars_temp[lead_vars_temp %in% names(dt)]
  lead_vars_precip <- lead_vars_precip[lead_vars_precip %in% names(dt)]

  if (length(lead_vars_temp) == 0) {
    cat("Could not create lead variables. Skipping test.\n")
    return(NULL)
  }

  # Formula: current shock + leads
  formula_leads <- as.formula(paste0(
    dep_var, " ~ ", shock_var, " + cont_shock_precip + ",
    paste(lead_vars_temp, collapse = " + "), " + ",
    paste(lead_vars_precip, collapse = " + "),
    " | mun_code + year"
  ))

  # Run regression
  tryCatch({
    model <- feols(formula_leads, data = dt, cluster = ~mun_code)

    cat("Results:\n\n")
    cat("Current Period Shocks:\n")
    cat("  Temperature (t):     ", format_results(
      coef(model)[shock_var],
      se(model)[shock_var],
      pvalue(model)[shock_var]
    ), "\n")
    cat("  Precipitation (t):   ", format_results(
      coef(model)["cont_shock_precip"],
      se(model)["cont_shock_precip"],
      pvalue(model)["cont_shock_precip"]
    ), "\n\n")

    cat("Lead Coefficients (Future Shocks - Should be zero under H0):\n")

    lead_results <- data.frame(
      lead = integer(),
      coef_temp = numeric(),
      se_temp = numeric(),
      pval_temp = numeric(),
      coef_precip = numeric(),
      se_precip = numeric(),
      pval_precip = numeric()
    )

    for (l in leads) {
      lead_var_t <- paste0("lead", l, "_", shock_var)
      lead_var_p <- paste0("lead", l, "_cont_shock_precip")

      if (lead_var_t %in% names(coef(model))) {
        cat("  Temperature (t+", l, "): ", format_results(
          coef(model)[lead_var_t],
          se(model)[lead_var_t],
          pvalue(model)[lead_var_t]
        ), "\n", sep = "")

        lead_results <- rbind(lead_results, data.frame(
          lead = l,
          coef_temp = coef(model)[lead_var_t],
          se_temp = se(model)[lead_var_t],
          pval_temp = pvalue(model)[lead_var_t],
          coef_precip = ifelse(lead_var_p %in% names(coef(model)),
                               coef(model)[lead_var_p], NA),
          se_precip = ifelse(lead_var_p %in% names(coef(model)),
                             se(model)[lead_var_p], NA),
          pval_precip = ifelse(lead_var_p %in% names(coef(model)),
                               pvalue(model)[lead_var_p], NA)
        ))
      }
    }

    # Joint test for leads = 0
    cat("\nJoint F-test for H0: All lead coefficients = 0:\n")

    lead_coefs <- coef(model)[lead_vars_temp[lead_vars_temp %in% names(coef(model))]]
    if (length(lead_coefs) > 0) {
      # Wald test
      vcov_model <- vcov(model)
      lead_idx <- which(names(coef(model)) %in% lead_vars_temp)

      if (length(lead_idx) > 0) {
        R <- diag(length(coef(model)))[lead_idx, , drop = FALSE]
        r <- rep(0, length(lead_idx))

        tryCatch({
          wald_stat <- t(R %*% coef(model) - r) %*%
            solve(R %*% vcov_model %*% t(R)) %*%
            (R %*% coef(model) - r)

          wald_pval <- pchisq(wald_stat, df = length(lead_idx), lower.tail = FALSE)

          cat("  Wald statistic:  ", sprintf("%.3f", wald_stat), "\n")
          cat("  Degrees of freedom:", length(lead_idx), "\n")
          cat("  P-value:         ", sprintf("%.4f", wald_pval), "\n")

          if (wald_pval < ALPHA) {
            cat("\n  >> WARNING: Joint test rejects H0 at ", ALPHA * 100, "% level.\n")
            cat("     Pre-trends may be present. Review identification strategy.\n")
          } else {
            cat("\n  >> PASS: Cannot reject H0. No evidence of pre-trends.\n")
          }
        }, error = function(e) {
          cat("  Could not compute joint test:", e$message, "\n")
        })
      }
    }

    cat("\nObservations:", nobs(model), "\n")
    cat("R-squared:", sprintf("%.4f", r2(model, type = "r2")), "\n")

    return(list(
      model = model,
      lead_results = lead_results,
      dep_var = dep_var
    ))

  }, error = function(e) {
    cat("Error running pre-trend test:", e$message, "\n")
    return(NULL)
  })
}

# ============================================================================
# TEST 2: TESTE EM PERIODOS DE BAIXA VARIACAO CLIMATICA
# ============================================================================
#
# CONCEITO: Em periodos onde nao houve choques significativos,
# nao deveriamos observar mudancas nos outcomes relacionadas a clima.
#
# Identificamos periodos com baixa variacao (CV < mediana) e testamos
# se os coeficientes sao estatisticamente diferentes de zero.
#
# ============================================================================

run_low_variability_test <- function(data, dep_var, shock_var = "cont_shock_temp",
                                      label = "") {

  cat("\n", "=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("TEST 2: LOW CLIMATE VARIABILITY PERIODS -", label, "\n")
  cat("Dependent Variable:", dep_var, "\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

  if (is.null(data)) {
    cat("Data not available. Skipping test.\n")
    return(NULL)
  }

  dt <- copy(data)

  # Calculate year-level variability
  variability <- dt[, .(
    sd_temp = sd(cont_shock_temp, na.rm = TRUE),
    sd_precip = sd(cont_shock_precip, na.rm = TRUE),
    n_obs = .N
  ), by = year]

  # Classify periods as low vs high variability
  median_sd_temp <- median(variability$sd_temp, na.rm = TRUE)
  variability[, low_variability := sd_temp < median_sd_temp]

  cat("Climate Variability by Year:\n")
  cat("  Median SD (Temperature):", sprintf("%.4f", median_sd_temp), "\n\n")

  print(variability[order(year)])

  # Merge classification back to data
  dt <- merge(dt, variability[, .(year, low_variability)], by = "year")

  # Separate regressions for low and high variability periods
  cat("\n\nResults for LOW VARIABILITY periods:\n")
  cat("(Periods where SD < median SD)\n\n")

  dt_low <- dt[low_variability == TRUE]
  dt_high <- dt[low_variability == FALSE]

  results <- list()

  if (nrow(dt_low) > 100) {
    formula_fe <- as.formula(paste0(dep_var, " ~ cont_shock_temp + cont_shock_precip | mun_code + year"))

    tryCatch({
      model_low <- feols(formula_fe, data = dt_low, cluster = ~mun_code)

      cat("  Temperature:   ", format_results(
        coef(model_low)["cont_shock_temp"],
        se(model_low)["cont_shock_temp"],
        pvalue(model_low)["cont_shock_temp"]
      ), "\n")
      cat("  Precipitation: ", format_results(
        coef(model_low)["cont_shock_precip"],
        se(model_low)["cont_shock_precip"],
        pvalue(model_low)["cont_shock_precip"]
      ), "\n")
      cat("  Observations:", nobs(model_low), "\n")
      cat("  Years included:", paste(sort(unique(dt_low$year)), collapse = ", "), "\n")

      results$low <- model_low

      # Check if coefficients are significant (they shouldn't be in low variability)
      if (pvalue(model_low)["cont_shock_temp"] < ALPHA) {
        cat("\n  >> NOTE: Temperature coefficient significant even in low variability periods.\n")
        cat("     This may indicate underlying trends unrelated to climate shocks.\n")
      } else {
        cat("\n  >> PASS: No significant effect in low variability periods.\n")
      }

    }, error = function(e) {
      cat("  Error:", e$message, "\n")
    })
  } else {
    cat("  Insufficient observations in low variability periods.\n")
  }

  cat("\n\nResults for HIGH VARIABILITY periods (comparison):\n")
  cat("(Periods where SD >= median SD)\n\n")

  if (nrow(dt_high) > 100) {
    tryCatch({
      model_high <- feols(formula_fe, data = dt_high, cluster = ~mun_code)

      cat("  Temperature:   ", format_results(
        coef(model_high)["cont_shock_temp"],
        se(model_high)["cont_shock_temp"],
        pvalue(model_high)["cont_shock_temp"]
      ), "\n")
      cat("  Precipitation: ", format_results(
        coef(model_high)["cont_shock_precip"],
        se(model_high)["cont_shock_precip"],
        pvalue(model_high)["cont_shock_precip"]
      ), "\n")
      cat("  Observations:", nobs(model_high), "\n")
      cat("  Years included:", paste(sort(unique(dt_high$year)), collapse = ", "), "\n")

      results$high <- model_high

    }, error = function(e) {
      cat("  Error:", e$message, "\n")
    })
  }

  return(results)
}

# ============================================================================
# TEST 3: PLACEBO TEMPORAL (CHOQUES MUITO DISTANTES)
# ============================================================================
#
# CONCEITO: Choques climaticos muito antigos (e.g., t-15, t-20) nao deveriam
# afetar outcomes atuais, a menos que hajam tendencias espurias.
#
# Usamos choques distantes como "placebo" - se forem significativos,
# ha problema na especificacao.
#
# ============================================================================

run_distant_lag_placebo <- function(data, dep_var, shock_var = "cont_shock_temp",
                                     placebo_lags = c(15, 20), label = "") {

  cat("\n", "=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("TEST 3: DISTANT LAG PLACEBO -", label, "\n")
  cat("Dependent Variable:", dep_var, "\n")
  cat("Placebo lags:", paste(placebo_lags, collapse = ", "), "years\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

  if (is.null(data)) {
    cat("Data not available. Skipping test.\n")
    return(NULL)
  }

  dt <- copy(data)

  # Create distant lags
  dt <- create_lags(dt, shock_var, placebo_lags)
  dt <- create_lags(dt, "cont_shock_precip", placebo_lags)

  results <- list()

  for (lag in placebo_lags) {
    lag_var_temp <- paste0("lag", lag, "_", shock_var)
    lag_var_precip <- paste0("lag", lag, "_cont_shock_precip")

    if (!lag_var_temp %in% names(dt)) {
      cat("\nCould not create lag", lag, "for temperature. Skipping.\n")
      next
    }

    cat("\n--- Placebo Test: Lag", lag, "years ---\n\n")

    # Formula with only distant lag (no current shock)
    formula_placebo <- as.formula(paste0(
      dep_var, " ~ ", lag_var_temp, " + ", lag_var_precip,
      " | mun_code + year"
    ))

    tryCatch({
      model <- feols(formula_placebo, data = dt, cluster = ~mun_code)

      cat("Results (should be insignificant under H0):\n")
      cat("  Temperature (t-", lag, "): ", format_results(
        coef(model)[lag_var_temp],
        se(model)[lag_var_temp],
        pvalue(model)[lag_var_temp]
      ), "\n", sep = "")
      cat("  Precipitation (t-", lag, "): ", format_results(
        coef(model)[lag_var_precip],
        se(model)[lag_var_precip],
        pvalue(model)[lag_var_precip]
      ), "\n", sep = "")
      cat("  Observations:", nobs(model), "\n")

      # Interpretation
      if (pvalue(model)[lag_var_temp] < ALPHA) {
        cat("\n  >> WARNING: Distant lag (", lag, " years) is significant!\n", sep = "")
        cat("     This suggests potential confounding or spurious correlation.\n")
      } else {
        cat("\n  >> PASS: Distant lag not significant as expected.\n")
      }

      results[[paste0("lag", lag)]] <- model

    }, error = function(e) {
      cat("  Error:", e$message, "\n")
    })
  }

  return(results)
}

# ============================================================================
# TEST 4: PLACEBO COM PERMUTACAO ALEATORIA
# ============================================================================
#
# CONCEITO: Permutamos os choques climaticos aleatoriamente entre municipios
# (quebrando a relacao causal real) e re-estimamos o modelo.
#
# Repetimos N vezes para construir a distribuicao nula dos coeficientes.
# O coeficiente real deve estar na cauda (p < alpha).
#
# ============================================================================

run_permutation_placebo <- function(data, dep_var, shock_var = "cont_shock_temp",
                                     n_perms = 100, label = "") {

  cat("\n", "=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("TEST 4: PERMUTATION PLACEBO -", label, "\n")
  cat("Dependent Variable:", dep_var, "\n")
  cat("Number of permutations:", n_perms, "\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

  if (is.null(data)) {
    cat("Data not available. Skipping test.\n")
    return(NULL)
  }

  dt <- copy(data)

  # First, run actual regression to get true coefficient
  formula_fe <- as.formula(paste0(dep_var, " ~ cont_shock_temp + cont_shock_precip | mun_code + year"))

  tryCatch({
    model_true <- feols(formula_fe, data = dt, cluster = ~mun_code)
    true_coef_temp <- coef(model_true)["cont_shock_temp"]
    true_coef_precip <- coef(model_true)["cont_shock_precip"]

    cat("True coefficients:\n")
    cat("  Temperature:   ", sprintf("%.6f", true_coef_temp), "\n")
    cat("  Precipitation: ", sprintf("%.6f", true_coef_precip), "\n\n")

    cat("Running permutation tests...\n")

    # Storage for permuted coefficients
    perm_coefs_temp <- numeric(n_perms)
    perm_coefs_precip <- numeric(n_perms)

    # Progress tracking
    pb_interval <- max(1, floor(n_perms / 10))

    for (p in 1:n_perms) {
      if (p %% pb_interval == 0) {
        cat("  Progress:", round(p / n_perms * 100), "%\n")
      }

      # Create permuted data - shuffle shocks within each year
      dt_perm <- copy(dt)
      dt_perm[, cont_shock_temp_perm := sample(cont_shock_temp), by = year]
      dt_perm[, cont_shock_precip_perm := sample(cont_shock_precip), by = year]

      # Run regression with permuted shocks
      formula_perm <- as.formula(paste0(
        dep_var, " ~ cont_shock_temp_perm + cont_shock_precip_perm | mun_code + year"
      ))

      tryCatch({
        model_perm <- feols(formula_perm, data = dt_perm, cluster = ~mun_code)
        perm_coefs_temp[p] <- coef(model_perm)["cont_shock_temp_perm"]
        perm_coefs_precip[p] <- coef(model_perm)["cont_shock_precip_perm"]
      }, error = function(e) {
        perm_coefs_temp[p] <- NA
        perm_coefs_precip[p] <- NA
      })
    }

    # Calculate permutation p-values
    perm_coefs_temp <- perm_coefs_temp[!is.na(perm_coefs_temp)]
    perm_coefs_precip <- perm_coefs_precip[!is.na(perm_coefs_precip)]

    # Two-sided p-value
    pval_perm_temp <- mean(abs(perm_coefs_temp) >= abs(true_coef_temp))
    pval_perm_precip <- mean(abs(perm_coefs_precip) >= abs(true_coef_precip))

    cat("\n\nPermutation Test Results:\n")
    cat("-" %>% rep(50) %>% paste(collapse = ""), "\n")
    cat("\nTemperature Shock:\n")
    cat("  True coefficient:        ", sprintf("%.6f", true_coef_temp), "\n")
    cat("  Permutation mean:        ", sprintf("%.6f", mean(perm_coefs_temp)), "\n")
    cat("  Permutation SD:          ", sprintf("%.6f", sd(perm_coefs_temp)), "\n")
    cat("  Permutation p-value:     ", sprintf("%.4f", pval_perm_temp), "\n")

    cat("\nPrecipitation Shock:\n")
    cat("  True coefficient:        ", sprintf("%.6f", true_coef_precip), "\n")
    cat("  Permutation mean:        ", sprintf("%.6f", mean(perm_coefs_precip)), "\n")
    cat("  Permutation SD:          ", sprintf("%.6f", sd(perm_coefs_precip)), "\n")
    cat("  Permutation p-value:     ", sprintf("%.4f", pval_perm_precip), "\n")

    # Interpretation
    cat("\nInterpretation:\n")
    if (pval_perm_temp < ALPHA) {
      cat("  >> Temperature: TRUE coefficient is significant (p < ", ALPHA, ").\n", sep = "")
      cat("     The effect is unlikely to be due to chance.\n")
    } else {
      cat("  >> Temperature: TRUE coefficient is NOT significant (p >= ", ALPHA, ").\n", sep = "")
      cat("     The effect may be spurious.\n")
    }

    return(list(
      true_model = model_true,
      true_coef_temp = true_coef_temp,
      true_coef_precip = true_coef_precip,
      perm_coefs_temp = perm_coefs_temp,
      perm_coefs_precip = perm_coefs_precip,
      pval_perm_temp = pval_perm_temp,
      pval_perm_precip = pval_perm_precip
    ))

  }, error = function(e) {
    cat("Error:", e$message, "\n")
    return(NULL)
  })
}

# ============================================================================
# TEST 5: EVENT STUDY SPECIFICATION (DYNAMIC PRE-TRENDS)
# ============================================================================
#
# CONCEITO: Especificacao de event study com leads e lags para visualizar
# a dinamica completa dos efeitos antes e depois do choque.
#
# Y_t = sum_{k=-K}^{L} beta_k * Shock_{t-k} + alpha_i + gamma_t + epsilon
#
# Coeficientes pre-choque (k < 0) devem ser zero.
#
# ============================================================================

run_event_study <- function(data, dep_var, shock_var = "cont_shock_temp",
                             leads = c(1, 2, 3), lags = c(0, 1, 2, 3, 4, 5),
                             label = "") {

  cat("\n", "=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("TEST 5: EVENT STUDY SPECIFICATION -", label, "\n")
  cat("Dependent Variable:", dep_var, "\n")
  cat("Leads (pre-shock):", paste(leads, collapse = ", "), "\n")
  cat("Lags (post-shock):", paste(lags, collapse = ", "), "\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

  if (is.null(data)) {
    cat("Data not available. Skipping test.\n")
    return(NULL)
  }

  dt <- copy(data)

  # Create leads and lags
  dt <- create_leads(dt, shock_var, leads)
  dt <- create_lags(dt, shock_var, lags[lags > 0])

  # Build variable names
  lead_vars <- paste0("lead", leads, "_", shock_var)
  lag_vars <- c(shock_var, paste0("lag", lags[lags > 0], "_", shock_var))

  # Keep only existing variables
  all_vars <- c(lead_vars, lag_vars)
  all_vars <- all_vars[all_vars %in% names(dt)]

  if (length(all_vars) < 2) {
    cat("Could not create sufficient lead/lag variables. Skipping.\n")
    return(NULL)
  }

  # Formula
  formula_es <- as.formula(paste0(
    dep_var, " ~ ", paste(all_vars, collapse = " + "),
    " | mun_code + year"
  ))

  tryCatch({
    model <- feols(formula_es, data = dt, cluster = ~mun_code)

    # Extract and organize results
    results_df <- data.frame(
      period = integer(),
      coef = numeric(),
      se = numeric(),
      ci_low = numeric(),
      ci_high = numeric()
    )

    cat("Event Study Coefficients:\n")
    cat("-" %>% rep(60) %>% paste(collapse = ""), "\n")
    cat(sprintf("%-20s %12s %12s %12s\n", "Period", "Coefficient", "Std. Error", "95% CI"))
    cat("-" %>% rep(60) %>% paste(collapse = ""), "\n")

    # Leads (negative periods)
    for (l in rev(leads)) {
      var_name <- paste0("lead", l, "_", shock_var)
      if (var_name %in% names(coef(model))) {
        coef_val <- coef(model)[var_name]
        se_val <- se(model)[var_name]
        ci_low <- coef_val - 1.96 * se_val
        ci_high <- coef_val + 1.96 * se_val

        cat(sprintf("t - %d (lead)       %12.4f %12.4f [%7.4f, %7.4f]%s\n",
                    l, coef_val, se_val, ci_low, ci_high,
                    ifelse(pvalue(model)[var_name] < 0.05, " *", "")))

        results_df <- rbind(results_df, data.frame(
          period = -l,
          coef = coef_val,
          se = se_val,
          ci_low = ci_low,
          ci_high = ci_high
        ))
      }
    }

    # Current and lags (non-negative periods)
    for (l in lags) {
      if (l == 0) {
        var_name <- shock_var
        period_label <- "t = 0 (shock)     "
      } else {
        var_name <- paste0("lag", l, "_", shock_var)
        period_label <- sprintf("t + %d (lag)       ", l)
      }

      if (var_name %in% names(coef(model))) {
        coef_val <- coef(model)[var_name]
        se_val <- se(model)[var_name]
        ci_low <- coef_val - 1.96 * se_val
        ci_high <- coef_val + 1.96 * se_val

        cat(sprintf("%s %12.4f %12.4f [%7.4f, %7.4f]%s\n",
                    period_label, coef_val, se_val, ci_low, ci_high,
                    ifelse(pvalue(model)[var_name] < 0.05, " *", "")))

        results_df <- rbind(results_df, data.frame(
          period = l,
          coef = coef_val,
          se = se_val,
          ci_low = ci_low,
          ci_high = ci_high
        ))
      }
    }

    cat("-" %>% rep(60) %>% paste(collapse = ""), "\n")
    cat("* significant at 5% level\n")
    cat("\nObservations:", nobs(model), "\n")

    # Pre-trend test: joint test that all leads = 0
    cat("\nJoint Pre-Trend Test (H0: all lead coefficients = 0):\n")

    lead_coef_names <- lead_vars[lead_vars %in% names(coef(model))]
    if (length(lead_coef_names) > 0) {
      tryCatch({
        vcov_model <- vcov(model)
        lead_idx <- which(names(coef(model)) %in% lead_coef_names)

        R <- diag(length(coef(model)))[lead_idx, , drop = FALSE]
        r <- rep(0, length(lead_idx))

        wald_stat <- t(R %*% coef(model) - r) %*%
          solve(R %*% vcov_model %*% t(R)) %*%
          (R %*% coef(model) - r)

        wald_pval <- pchisq(wald_stat, df = length(lead_idx), lower.tail = FALSE)

        cat("  Wald statistic:  ", sprintf("%.3f", wald_stat), "\n")
        cat("  Degrees of freedom:", length(lead_idx), "\n")
        cat("  P-value:         ", sprintf("%.4f", wald_pval), "\n")

        if (wald_pval < ALPHA) {
          cat("\n  >> WARNING: Pre-trend test rejects H0.\n")
          cat("     Evidence of differential trends before treatment.\n")
        } else {
          cat("\n  >> PASS: Cannot reject parallel trends assumption.\n")
        }

      }, error = function(e) {
        cat("  Could not compute joint test:", e$message, "\n")
      })
    }

    return(list(
      model = model,
      results = results_df
    ))

  }, error = function(e) {
    cat("Error:", e$message, "\n")
    return(NULL)
  })
}

# ============================================================================
# TEST 6: PLACEBO COM OUTCOMES PASSADOS (REVERSE CAUSALITY)
# ============================================================================
#
# CONCEITO: Usamos os choques climaticos ATUAIS (t) como variavel independente
# para prever outcomes PASSADOS (t-k). Isso testa causalidade reversa.
#
# Modelo:
#   Y_{t-k} = beta_0 + beta_1*Shock_t + beta_2*Precip_t + alpha_i + gamma_t + epsilon
#
# H0: beta_1 = beta_2 = 0 (choques atuais nao afetam passado)
# H1: beta != 0 (problema de identificacao - possivelmente tendencias correlacionadas)
#
# Se rejeitarmos H0, significa que ha correlacao espuria entre choques e
# outcomes que nao pode ser causal (futuro nao causa passado).
#
# ============================================================================

run_lagged_outcome_placebo <- function(data, dep_var, shock_var = "cont_shock_temp",
                                        outcome_lags = c(1, 2, 3, 5), label = "") {

  cat("\n", "=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("TEST 6: LAGGED OUTCOME PLACEBO (REVERSE CAUSALITY) -", label, "\n")
  cat("Dependent Variable:", dep_var, "\n")
  cat("Testing if CURRENT shocks predict PAST outcomes\n")
  cat("Outcome lags tested:", paste(outcome_lags, collapse = ", "), "years\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

  if (is.null(data)) {
    cat("Data not available. Skipping test.\n")
    return(NULL)
  }

  dt <- copy(data)
  setorderv(dt, c("mun_code", "year"))

  # Create lagged outcome variables (Y_{t-k})
  for (k in outcome_lags) {
    lag_outcome_name <- paste0(dep_var, "_lag", k)
    dt[, (lag_outcome_name) := shift(get(dep_var), n = k, type = "lag"), by = mun_code]
  }

  cat("Model specification:\n")
  cat("  Y_{t-k} = beta_1 * Shock_t + beta_2 * Precip_t + alpha_i + gamma_t + epsilon\n\n")
  cat("Interpretation: If beta != 0, current shocks 'predict' past outcomes,\n")
  cat("                which is impossible causally => spurious correlation.\n\n")

  results <- list()
  results_summary <- data.frame(
    outcome_lag = integer(),
    coef_temp = numeric(),
    se_temp = numeric(),
    pval_temp = numeric(),
    coef_precip = numeric(),
    se_precip = numeric(),
    pval_precip = numeric(),
    n_obs = integer(),
    significant = character()
  )

  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat(sprintf("%-12s %15s %15s %15s %10s\n",
              "Outcome Lag", "Temp Coef (SE)", "Precip Coef (SE)", "N Obs", "Signif?"))
  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")

  for (k in outcome_lags) {
    lag_outcome_name <- paste0(dep_var, "_lag", k)

    # Skip if lagged outcome doesn't exist or has too few observations
    if (!lag_outcome_name %in% names(dt)) {
      cat(sprintf("%-12s Could not create lagged outcome. Skipping.\n", paste0("Y_{t-", k, "}")))
      next
    }

    # Check for sufficient non-missing observations
    n_valid <- sum(!is.na(dt[[lag_outcome_name]]) & !is.na(dt[[shock_var]]))
    if (n_valid < 100) {
      cat(sprintf("%-12s Insufficient observations (%d). Skipping.\n",
                  paste0("Y_{t-", k, "}"), n_valid))
      next
    }

    # Formula: Lagged outcome ~ Current shocks | FE
    formula_placebo <- as.formula(paste0(
      lag_outcome_name, " ~ ", shock_var, " + cont_shock_precip | mun_code + year"
    ))

    tryCatch({
      model <- feols(formula_placebo, data = dt, cluster = ~mun_code)

      coef_temp <- coef(model)[shock_var]
      se_temp <- se(model)[shock_var]
      pval_temp <- pvalue(model)[shock_var]

      coef_precip <- coef(model)["cont_shock_precip"]
      se_precip <- se(model)["cont_shock_precip"]
      pval_precip <- pvalue(model)["cont_shock_precip"]

      # Determine significance
      sig_temp <- pval_temp < ALPHA
      sig_precip <- pval_precip < ALPHA
      sig_indicator <- ifelse(sig_temp | sig_precip,
                              paste0(ifelse(sig_temp, "T", ""), ifelse(sig_precip, "P", "")),
                              "No")

      # Print results
      cat(sprintf("%-12s %7.4f (%5.4f) %7.4f (%5.4f) %10d %10s\n",
                  paste0("Y_{t-", k, "}"),
                  coef_temp, se_temp,
                  coef_precip, se_precip,
                  nobs(model),
                  sig_indicator))

      # Store results
      results[[paste0("lag", k)]] <- model

      results_summary <- rbind(results_summary, data.frame(
        outcome_lag = k,
        coef_temp = coef_temp,
        se_temp = se_temp,
        pval_temp = pval_temp,
        coef_precip = coef_precip,
        se_precip = se_precip,
        pval_precip = pval_precip,
        n_obs = nobs(model),
        significant = sig_indicator
      ))

    }, error = function(e) {
      cat(sprintf("%-12s Error: %s\n", paste0("Y_{t-", k, "}"), e$message))
    })
  }

  cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("Significance: T = Temperature significant, P = Precipitation significant\n\n")

  # Overall interpretation
  n_significant <- sum(results_summary$significant != "No")
  n_tests <- nrow(results_summary)

  cat("INTERPRETATION:\n")
  cat("-" %>% rep(40) %>% paste(collapse = ""), "\n")

  if (n_significant == 0) {
    cat(">> PASS: No significant effects of current shocks on past outcomes.\n")
    cat("   This supports the causal interpretation - no reverse causality detected.\n")
  } else if (n_significant <= 1 && n_tests >= 3) {
    cat(">> MARGINAL: ", n_significant, " of ", n_tests, " tests show significance.\n", sep = "")
    cat("   Could be due to multiple testing. Consider Bonferroni correction.\n")
    cat("   Bonferroni-adjusted alpha:", sprintf("%.4f", ALPHA / n_tests), "\n")

    # Check with Bonferroni
    bonf_sig_temp <- sum(results_summary$pval_temp < (ALPHA / n_tests))
    bonf_sig_precip <- sum(results_summary$pval_precip < (ALPHA / n_tests))

    if (bonf_sig_temp == 0 && bonf_sig_precip == 0) {
      cat("   After Bonferroni correction: No significant effects. PASS.\n")
    }
  } else {
    cat(">> WARNING: ", n_significant, " of ", n_tests, " tests show significance!\n", sep = "")
    cat("   Current shocks appear to 'predict' past outcomes.\n")
    cat("   This indicates potential problems:\n")
    cat("     - Correlated trends between climate and outcomes\n")
    cat("     - Measurement timing issues\n")
    cat("     - Omitted time-varying confounders\n")
    cat("   Consider adding region-year fixed effects or investigating further.\n")
  }

  # Joint test across all lags
  if (length(results) >= 2) {
    cat("\nJoint Wald Test (H0: All lagged outcome coefficients = 0):\n")

    # Collect all temperature coefficients and their variance
    temp_coefs <- sapply(results, function(m) coef(m)[shock_var])
    temp_vars <- sapply(results, function(m) se(m)[shock_var]^2)

    # Simple chi-square test (assuming independence across lags for simplicity)
    chi_sq <- sum(temp_coefs^2 / temp_vars)
    df <- length(temp_coefs)
    joint_pval <- pchisq(chi_sq, df = df, lower.tail = FALSE)

    cat("  Chi-square statistic:", sprintf("%.3f", chi_sq), "\n")
    cat("  Degrees of freedom:  ", df, "\n")
    cat("  P-value:             ", sprintf("%.4f", joint_pval), "\n")

    if (joint_pval < ALPHA) {
      cat("\n  >> Joint test REJECTS H0: Evidence of systematic reverse correlation.\n")
    } else {
      cat("\n  >> Joint test FAILS TO REJECT H0: No systematic reverse causality.\n")
    }
  }

  return(list(
    models = results,
    summary = results_summary,
    dep_var = dep_var
  ))
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================

cat("\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("RUNNING PRE-TREND AND PLACEBO TESTS\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

# Store all results
all_results <- list()

# ============================================================================
# EMPLOYMENT TESTS
# ============================================================================

if (!is.null(data_emp)) {

  cat("\n")
  cat("#" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("# EMPLOYMENT OUTCOMES\n")
  cat("#" %>% rep(70) %>% paste(collapse = ""), "\n")

  # Rename variables if needed
  if ("total_vinc" %in% names(data_emp)) {
    setnames(data_emp, old = c("total_vinc", "total_vinc_verde"),
             new = c("total_jobs", "green_jobs"), skip_absent = TRUE)
  }

  if (!"prop_verde" %in% names(data_emp) && all(c("total_jobs", "green_jobs") %in% names(data_emp))) {
    data_emp[, prop_verde := green_jobs / total_jobs]
  }

  # Filter data
  data_emp_clean <- data_emp[year >= 2000 & year <= 2020]
  data_emp_clean <- data_emp_clean[!is.na(cont_shock_temp) & !is.na(cont_shock_precip)]

  # Test 1: Pre-trends with leads
  all_results$emp_pretrend_leads <- run_pretrend_leads_test(
    data_emp_clean, "green_jobs", leads = c(1, 2, 3), label = "Employment"
  )

  # Test 2: Low variability periods
  all_results$emp_low_var <- run_low_variability_test(
    data_emp_clean, "green_jobs", label = "Employment"
  )

  # Test 3: Distant lag placebo
  all_results$emp_distant_lag <- run_distant_lag_placebo(
    data_emp_clean, "green_jobs", placebo_lags = c(15, 20), label = "Employment"
  )

  # Test 4: Permutation placebo (reduced for speed)
  all_results$emp_permutation <- run_permutation_placebo(
    data_emp_clean, "green_jobs", n_perms = min(N_PERMUTATIONS, 100), label = "Employment"
  )

  # Test 5: Event study
  all_results$emp_event_study <- run_event_study(
    data_emp_clean, "green_jobs", leads = c(1, 2, 3), lags = c(0, 1, 2, 3, 4, 5),
    label = "Employment"
  )

  # Test 6: Lagged outcome placebo (reverse causality)
  all_results$emp_lagged_outcome <- run_lagged_outcome_placebo(
    data_emp_clean, "green_jobs", outcome_lags = c(1, 2, 3, 5), label = "Employment"
  )
}

# ============================================================================
# PATENT TESTS
# ============================================================================

if (!is.null(data_pat)) {

  cat("\n")
  cat("#" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("# PATENT OUTCOMES\n")
  cat("#" %>% rep(70) %>% paste(collapse = ""), "\n")

  # Prepare proportion variable
  if (!"prop_verde" %in% names(data_pat) && all(c("qtd_pat", "qtd_pat_verde") %in% names(data_pat))) {
    data_pat[, prop_verde := ifelse(qtd_pat > 0, qtd_pat_verde / qtd_pat, 0)]
  }

  # Filter data
  data_pat_clean <- data_pat[year >= 2000 & year <= 2020]
  data_pat_clean <- data_pat_clean[!is.na(cont_shock_temp) & !is.na(cont_shock_precip)]

  # Test 1: Pre-trends with leads
  all_results$pat_pretrend_leads <- run_pretrend_leads_test(
    data_pat_clean, "qtd_pat_verde", leads = c(1, 2, 3), label = "Patents"
  )

  # Test 2: Low variability periods
  all_results$pat_low_var <- run_low_variability_test(
    data_pat_clean, "qtd_pat_verde", label = "Patents"
  )

  # Test 3: Distant lag placebo
  all_results$pat_distant_lag <- run_distant_lag_placebo(
    data_pat_clean, "qtd_pat_verde", placebo_lags = c(15, 20), label = "Patents"
  )

  # Test 5: Event study
  all_results$pat_event_study <- run_event_study(
    data_pat_clean, "qtd_pat_verde", leads = c(1, 2, 3), lags = c(0, 1, 2, 3, 4, 5),
    label = "Patents"
  )

  # Test 6: Lagged outcome placebo (reverse causality)
  all_results$pat_lagged_outcome <- run_lagged_outcome_placebo(
    data_pat_clean, "qtd_pat_verde", outcome_lags = c(1, 2, 3, 5), label = "Patents"
  )
}

# ============================================================================
# SUMMARY AND RECOMMENDATIONS
# ============================================================================

cat("\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("SUMMARY OF PRE-TREND AND PLACEBO TESTS\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

cat("Tests Implemented:\n")
cat("  1. Pre-trend test with leads (future shocks)\n")
cat("  2. Low climate variability period analysis\n")
cat("  3. Distant lag placebo (15-20 year lags)\n")
cat("  4. Permutation inference placebo\n")
cat("  5. Event study specification\n")
cat("  6. Lagged outcome placebo (reverse causality)\n\n")

cat("Key Interpretations:\n")
cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("  - Lead coefficients should be zero (no anticipation effects)\n")
cat("  - Distant lags should be insignificant (no spurious correlation)\n")
cat("  - Permutation p-value < alpha confirms true effect\n")
cat("  - Event study should show flat pre-trends, effect at t=0\n")
cat("  - Lagged outcomes should NOT be predicted by current shocks\n")
cat("-" %>% rep(70) %>% paste(collapse = ""), "\n\n")

cat("If tests fail:\n")
cat("  - Consider adding state-year or region-year fixed effects\n")
cat("  - Check for measurement error in shock variables\n")
cat("  - Investigate potential reverse causality\n")
cat("  - Consider alternative identification strategies\n\n")

cat("Script completed.\n")

# ============================================================================
# SAVE RESULTS
# ============================================================================

# Save results object for later use
if (length(all_results) > 0) {
  save(all_results, file = "./output/pretrend_placebo_results.RData")
  cat("Results saved to: ./output/pretrend_placebo_results.RData\n")
}
