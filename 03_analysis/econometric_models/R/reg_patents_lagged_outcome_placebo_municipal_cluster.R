# ============================================================================
# LAGGED OUTCOME PLACEBO TEST - PATENT OUTCOMES
# Reverse Causality Test: Current Shocks Predicting Past Outcomes
# Based on reg_patents_municipal_cluster.R specifications
# ============================================================================
#
# OBJETIVO: Testar se choques climaticos ATUAIS predizem outcomes PASSADOS.
# Se sim, indica correlacao espuria (futuro nao pode causar passado).
#
# MODELO:
#   Y_{t-k} = beta_1 * Shock_t + beta_2 * Precip_t (+ trend) + FE + epsilon
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
library(sandwich)     # For robust standard errors
library(lmtest)       # For coefficient testing
library(data.table)   # For efficient data manipulation
library(lfe)          # For fast two-way demeaning

# ============================================================================
# OPTIONAL POPULATION WEIGHTS
# ============================================================================

weight_var <- if (exists("population_weight_var", inherits = TRUE)) {
  population_weight_var
} else {
  NULL
}

resolve_weight_formula <- function(df) {
  if (!is.null(weight_var) && weight_var %in% names(df)) {
    return(as.formula(paste0("~", weight_var)))
  }
  NULL
}

resolve_weight_vector <- function(df) {
  if (!is.null(weight_var) && weight_var %in% names(df)) {
    return(df[[weight_var]])
  }
  NULL
}

# ============================================================================
# CONFIGURATION
# ============================================================================

# Outcome lags to test (in years)
OUTCOME_LAGS <- c(1, 5, 10, 20)

# ============================================================================
# STEP ONE: LOAD AND PREPARE DATA
# ============================================================================

cat("Loading patent data...\n")

# Load data
data <- read_dta("./output/final_base/weather_patent.dta")

# Filter for years 2000-2020
data <- data %>%
  filter(year >= 2000 & year <= 2020)

if (!is.null(weight_var) && weight_var %in% names(data)) {
  data <- data %>%
    filter(!is.na(.data[[weight_var]]), .data[[weight_var]] > 0)
}

cat("Data loaded. Observations:", nrow(data), "\n")

# ============================================================================
# STEP TWO: PREPARE VARIABLES
# ============================================================================

cat("Preparing variables...\n")

# Ensure prop_verde exists
if(!"prop_verde" %in% names(data) && "qtd_pat" %in% names(data) && "qtd_pat_verde" %in% names(data)) {
  data <- data %>%
    mutate(prop_verde = ifelse(qtd_pat > 0, qtd_pat_verde / qtd_pat, 0))
}

# Create state-year interaction variable for fixed effects
# Create municipality-year interaction for fixed effects
# Create state-year trend (year * code_state)
data <- data %>%
  mutate(
    state_year = interaction(code_state, year, drop = TRUE),
    mun_year = interaction(mun_code, year, drop = TRUE),
    year_state_trend = year * code_state
  )

cat("Variables prepared.\n")
cat("States:", length(unique(data$code_state)), "\n")
cat("Years:", length(unique(data$year)), "\n")
cat("State-year combinations:", length(unique(data$state_year)), "\n\n")

# Define dependent variables
base_dep_vars <- c("qtd_pat", "qtd_pat_verde", "prop_verde")
base_dep_labels <- c("Total Patents", "Green Patents", "Prop. Green Patents")

# Create lagged outcomes
cat("Creating lagged outcome variables...\n")

setDT(data)
setorderv(data, c("mun_code", "year"))

for (dep_var in base_dep_vars) {
  for (k in OUTCOME_LAGS) {
    lag_name <- paste0(dep_var, "_lag", k)
    data[, (lag_name) := shift(get(dep_var), n = k, type = "lag"), by = mun_code]
    cat("  Created:", lag_name, "\n")
  }
}

cat("\n")

# ============================================================================
# STEP THREE: PREPARE CLEAN DATA FOR SPATIAL HAC
# ============================================================================

cat("Preparing clean data for spatial HAC...\n")

data_clean <- as.data.table(data)
data_clean <- data_clean[!is.na(lat) & !is.na(lon) &
                           !is.na(cont_shock_temp) & !is.na(cont_shock_precip) &
                           !is.na(year_state_trend)]

if (!is.null(weight_var) && weight_var %in% names(data_clean)) {
  data_clean <- data_clean[!is.na(get(weight_var)) & get(weight_var) > 0]
}

cat("Clean data observations:", nrow(data_clean), "\n\n")

data_clean_df <- as.data.frame(data_clean)

# ============================================================================
# STEP FOUR: RUN REGRESSIONS
# ============================================================================

cat("Running placebo regressions...\n\n")

demean_two_way <- function(df, fe_var1, fe_var2, vars, weights = NULL, max_iter = 200, tol = 1e-8) {
  df_out <- df
  fe1 <- factor(df[[fe_var1]])
  fe2 <- factor(df[[fe_var2]])
  if (is.null(weights) && !is.null(weight_var) && weight_var %in% names(df)) {
    weights <- df[[weight_var]]
  }

  for (var in vars) {
    if (all(is.na(df[[var]]))) {
      next
    }

    if (is.null(weights)) {
      temp_df <- data.frame(
        y = df[[var]],
        fe1 = fe1,
        fe2 = fe2
      )

      demeaned <- lfe::demeanlist(temp_df[, "y", drop = FALSE],
                                  fl = list(fe1 = fe1, fe2 = fe2),
                                  na.rm = TRUE)

      df_out[[paste0(var, "_dm")]] <- demeaned[[1]]
    } else {
      x <- df[[var]]
      x_demean <- x
      w <- weights

      for (iter in seq_len(max_iter)) {
        x_old <- x_demean
        dt1 <- data.table(x = x_demean, w = w, fe1 = fe1)
        dt1[, mean1 := ifelse(
          sum(w[!is.na(x)]) == 0,
          NA_real_,
          sum(w * x, na.rm = TRUE) / sum(w[!is.na(x)])
        ), by = fe1]
        x_demean <- dt1$x - dt1$mean1

        dt2 <- data.table(x = x_demean, w = w, fe2 = fe2)
        dt2[, mean2 := ifelse(
          sum(w[!is.na(x)]) == 0,
          NA_real_,
          sum(w * x, na.rm = TRUE) / sum(w[!is.na(x)])
        ), by = fe2]
        x_demean <- dt2$x - dt2$mean2

        if (max(abs(x_demean - x_old), na.rm = TRUE) < tol) {
          break
        }
      }

      df_out[[paste0(var, "_dm")]] <- x_demean
    }
  }

  df_out
}

compute_spatial_xeeX <- function(X, resid, lat, lon, time, dist_cutoff, bartlett = FALSE) {
  k <- ncol(X)
  xeeX <- matrix(0, k, k)
  time_unique <- unique(time)

  for (ti in time_unique) {
    idx <- which(time == ti)
    X1 <- X[idx, , drop = FALSE]
    e1 <- resid[idx]
    lat1 <- lat[idx]
    lon1 <- lon[idx]

    n1 <- length(e1)
    if (n1 == 0) next

    for (i in seq_len(n1)) {
      lon_scale <- cos(lat1[i] * pi / 180) * 111
      lat_scale <- 111

      distance <- sqrt((lat_scale * (lat1[i] - lat1))^2 +
                         (lon_scale * (lon1[i] - lon1))^2)

      window <- as.numeric(distance <= dist_cutoff)

      if (bartlett) {
        weight <- 1 - distance / dist_cutoff
        window <- window * weight
      }

      weighted_resid <- e1 * window
      xeeXh <- (t(X1[i, , drop = FALSE]) * e1[i]) %*%
        (t(weighted_resid) %*% X1)
      xeeX <- xeeX + xeeXh
    }
  }

  xeeX
}

compute_serial_xeeX <- function(X, resid, time, panel, lag_cutoff) {
  if (lag_cutoff <= 0) {
    return(matrix(0, ncol(X), ncol(X)))
  }

  k <- ncol(X)
  xeeX <- matrix(0, k, k)
  panel_unique <- unique(panel)

  for (p in panel_unique) {
    idx <- which(panel == p)
    X1 <- X[idx, , drop = FALSE]
    e1 <- resid[idx]
    t1 <- time[idx]

    n1 <- length(e1)
    if (n1 == 0) next

    for (i in seq_len(n1)) {
      time_diff <- abs(t1[i] - t1)
      weight <- 1 - (time_diff / (lag_cutoff + 1))
      window <- (time_diff <= lag_cutoff) * weight

      weighted_resid <- e1 * window
      xeeXh <- (t(X1[i, , drop = FALSE]) * e1[i]) %*%
        (t(weighted_resid) %*% X1)
      xeeX <- xeeX + xeeXh
    }
  }

  xeeX
}

compute_spatial_hac_vcov <- function(data, y_var, x_vars, lat_var, lon_var,
                                     time_var, panel_var, dist_cutoff,
                                     lag_cutoff, bartlett = FALSE,
                                     weights = NULL) {
  y <- data[[y_var]]
  X <- as.matrix(data[, x_vars])
  lat <- data[[lat_var]]
  lon <- data[[lon_var]]
  time <- data[[time_var]]
  panel <- data[[panel_var]]

  if (!is.null(weights)) {
    w <- weights
    X <- X * sqrt(w)
    y <- y * sqrt(w)
  }

  fit <- lm.fit(x = X, y = y)
  resid <- fit$residuals
  n <- length(resid)

  inv_xx <- solve(crossprod(X))

  xeeX_spatial <- compute_spatial_xeeX(X, resid, lat, lon, time, dist_cutoff, bartlett)
  xeeX_serial <- compute_serial_xeeX(X, resid, time, panel, lag_cutoff)
  xeeX_spatial_hac <- xeeX_spatial + xeeX_serial

  xeeX_spatial_scaled <- xeeX_spatial * (n / (n - 1))
  xeeX_spatial_hac_scaled <- xeeX_spatial_hac * (n / (n - 1))

  v_spatial <- inv_xx %*% xeeX_spatial_scaled %*% inv_xx / n
  v_spatial_hac <- inv_xx %*% xeeX_spatial_hac_scaled %*% inv_xx / n

  v_spatial <- (v_spatial + t(v_spatial)) / 2
  v_spatial_hac <- (v_spatial_hac + t(v_spatial_hac)) / 2

  list(
    coefficients = fit$coefficients,
    vcov_spatial = v_spatial,
    vcov_spatial_hac = v_spatial_hac
  )
}

results_clustered <- data.frame()
results_hac <- data.frame()

for (lag in OUTCOME_LAGS) {
  dep_vars <- paste0(base_dep_vars, "_lag", lag)
  dep_labels <- paste0(base_dep_labels, " (t-", lag, ")")

  for (i in seq_along(dep_vars)) {
    dv <- dep_vars[i]
    dep_label <- dep_labels[i]

    if (all(is.na(data[[dv]]))) {
      next
    }

    vars_to_demean <- c(dv, "cont_shock_temp", "cont_shock_precip", "year_state_trend")

    cat("============================================\n")
    cat("Lag", lag, "-", dep_label, "\n")
    cat("============================================\n")

    # -----------------------------------------------------------------------
    # Model 1: Municipal + Year Fixed Effects (Baseline)
    # -----------------------------------------------------------------------
    formula_basic <- as.formula(paste0(dv, " ~ cont_shock_temp + cont_shock_precip | mun_code + year"))

    model_basic <- feols(
      formula_basic,
      data = data,
      weights = resolve_weight_formula(data),
      cluster = ~mun_code
    )

    results_clustered <- rbind(results_clustered, data.frame(
      outcome = dep_label,
      lag = lag,
      model = "baseline",
      se_type = "clustered",
      coef_temp = coef(model_basic)["cont_shock_temp"],
      se_temp = se(model_basic)["cont_shock_temp"],
      pval_temp = pvalue(model_basic)["cont_shock_temp"],
      coef_precip = coef(model_basic)["cont_shock_precip"],
      se_precip = se(model_basic)["cont_shock_precip"],
      pval_precip = pvalue(model_basic)["cont_shock_precip"],
      coef_trend = NA_real_,
      se_trend = NA_real_,
      pval_trend = NA_real_,
      n_obs = nobs(model_basic),
      r2 = r2(model_basic, type = "r2"),
      r2_adj = r2(model_basic, type = "ar2")
    ))

    tryCatch({
      data_model_df <- demean_two_way(data_clean_df, "mun_code", "year", vars_to_demean,
                                      weights = resolve_weight_vector(data_clean_df))
      data_model_df <- data_model_df[!is.na(data_model_df[[paste0(dv, "_dm")]]), ]

      hac_results <- compute_spatial_hac_vcov(
        data = data_model_df,
        y_var = paste0(dv, "_dm"),
        x_vars = c("cont_shock_temp_dm", "cont_shock_precip_dm"),
        lat_var = "lat",
        lon_var = "lon",
        time_var = "year",
        panel_var = "mun_code",
        dist_cutoff = 250,
        lag_cutoff = 6,
        bartlett = TRUE,
        weights = resolve_weight_vector(data_model_df)
      )

      coefs_hac <- hac_results$coefficients
      se_spatial_hac <- sqrt(diag(hac_results$vcov_spatial_hac))
      tstat_hac <- coefs_hac / se_spatial_hac
      pval_hac <- 2 * pt(abs(tstat_hac),
                         df = nrow(data_model_df) - length(coefs_hac),
                         lower.tail = FALSE)

      results_hac <- rbind(results_hac, data.frame(
        outcome = dep_label,
        lag = lag,
        model = "baseline",
        se_type = "spatial_hac",
        coef_temp = coefs_hac["cont_shock_temp_dm"],
        se_temp = se_spatial_hac["cont_shock_temp_dm"],
        pval_temp = pval_hac["cont_shock_temp_dm"],
        coef_precip = coefs_hac["cont_shock_precip_dm"],
        se_precip = se_spatial_hac["cont_shock_precip_dm"],
        pval_precip = pval_hac["cont_shock_precip_dm"],
        coef_trend = NA_real_,
        se_trend = NA_real_,
        pval_trend = NA_real_,
        n_obs = nrow(data_model_df),
        r2 = NA_real_,
        r2_adj = NA_real_
      ))
    }, error = function(e) {
      warning(paste("Spatial + serial HAC (baseline) failed for", dep_label, ":", e$message))
    })

    # -----------------------------------------------------------------------
    # Model 2: Municipal + State-Year Fixed Effects
    # -----------------------------------------------------------------------
    formula_state_year <- as.formula(paste0(dv, " ~ cont_shock_temp + cont_shock_precip | mun_code + state_year"))

    model_state_year <- feols(
      formula_state_year,
      data = data,
      weights = resolve_weight_formula(data),
      cluster = ~mun_code
    )

    results_clustered <- rbind(results_clustered, data.frame(
      outcome = dep_label,
      lag = lag,
      model = "state_year",
      se_type = "clustered",
      coef_temp = coef(model_state_year)["cont_shock_temp"],
      se_temp = se(model_state_year)["cont_shock_temp"],
      pval_temp = pvalue(model_state_year)["cont_shock_temp"],
      coef_precip = coef(model_state_year)["cont_shock_precip"],
      se_precip = se(model_state_year)["cont_shock_precip"],
      pval_precip = pvalue(model_state_year)["cont_shock_precip"],
      coef_trend = NA_real_,
      se_trend = NA_real_,
      pval_trend = NA_real_,
      n_obs = nobs(model_state_year),
      r2 = r2(model_state_year, type = "r2"),
      r2_adj = r2(model_state_year, type = "ar2")
    ))

    tryCatch({
      data_model_df <- demean_two_way(data_clean_df, "mun_code", "state_year", vars_to_demean,
                                      weights = resolve_weight_vector(data_clean_df))
      data_model_df <- data_model_df[!is.na(data_model_df[[paste0(dv, "_dm")]]), ]

      hac_results <- compute_spatial_hac_vcov(
        data = data_model_df,
        y_var = paste0(dv, "_dm"),
        x_vars = c("cont_shock_temp_dm", "cont_shock_precip_dm"),
        lat_var = "lat",
        lon_var = "lon",
        time_var = "year",
        panel_var = "mun_code",
        dist_cutoff = 250,
        lag_cutoff = 6,
        bartlett = TRUE,
        weights = resolve_weight_vector(data_model_df)
      )

      coefs_hac <- hac_results$coefficients
      se_spatial_hac <- sqrt(diag(hac_results$vcov_spatial_hac))
      tstat_hac <- coefs_hac / se_spatial_hac
      pval_hac <- 2 * pt(abs(tstat_hac),
                         df = nrow(data_model_df) - length(coefs_hac),
                         lower.tail = FALSE)

      results_hac <- rbind(results_hac, data.frame(
        outcome = dep_label,
        lag = lag,
        model = "state_year",
        se_type = "spatial_hac",
        coef_temp = coefs_hac["cont_shock_temp_dm"],
        se_temp = se_spatial_hac["cont_shock_temp_dm"],
        pval_temp = pval_hac["cont_shock_temp_dm"],
        coef_precip = coefs_hac["cont_shock_precip_dm"],
        se_precip = se_spatial_hac["cont_shock_precip_dm"],
        pval_precip = pval_hac["cont_shock_precip_dm"],
        coef_trend = NA_real_,
        se_trend = NA_real_,
        pval_trend = NA_real_,
        n_obs = nrow(data_model_df),
        r2 = NA_real_,
        r2_adj = NA_real_
      ))
    }, error = function(e) {
      warning(paste("Spatial + serial HAC (state-year FE) failed for", dep_label, ":", e$message))
    })

    # -----------------------------------------------------------------------
    # Model 3: Municipality-Year FE + State-Year Trends
    # -----------------------------------------------------------------------
    formula_mun_year_trend <- as.formula(paste0(dv, " ~ cont_shock_temp + cont_shock_precip + year_state_trend | mun_code + year"))

    model_mun_year_trend <- feols(
      formula_mun_year_trend,
      data = data,
      weights = resolve_weight_formula(data),
      cluster = ~mun_code
    )

    results_clustered <- rbind(results_clustered, data.frame(
      outcome = dep_label,
      lag = lag,
      model = "mun_year_trend",
      se_type = "clustered",
      coef_temp = coef(model_mun_year_trend)["cont_shock_temp"],
      se_temp = se(model_mun_year_trend)["cont_shock_temp"],
      pval_temp = pvalue(model_mun_year_trend)["cont_shock_temp"],
      coef_precip = coef(model_mun_year_trend)["cont_shock_precip"],
      se_precip = se(model_mun_year_trend)["cont_shock_precip"],
      pval_precip = pvalue(model_mun_year_trend)["cont_shock_precip"],
      coef_trend = coef(model_mun_year_trend)["year_state_trend"],
      se_trend = se(model_mun_year_trend)["year_state_trend"],
      pval_trend = pvalue(model_mun_year_trend)["year_state_trend"],
      n_obs = nobs(model_mun_year_trend),
      r2 = r2(model_mun_year_trend, type = "r2"),
      r2_adj = r2(model_mun_year_trend, type = "ar2")
    ))

    tryCatch({
      data_model_df <- demean_two_way(data_clean_df, "mun_code", "year", vars_to_demean,
                                      weights = resolve_weight_vector(data_clean_df))
      data_model_df <- data_model_df[!is.na(data_model_df[[paste0(dv, "_dm")]]), ]

      hac_results <- compute_spatial_hac_vcov(
        data = data_model_df,
        y_var = paste0(dv, "_dm"),
        x_vars = c("cont_shock_temp_dm", "cont_shock_precip_dm", "year_state_trend_dm"),
        lat_var = "lat",
        lon_var = "lon",
        time_var = "year",
        panel_var = "mun_code",
        dist_cutoff = 250,
        lag_cutoff = 6,
        bartlett = TRUE,
        weights = resolve_weight_vector(data_model_df)
      )

      coefs_hac <- hac_results$coefficients
      se_spatial_hac <- sqrt(diag(hac_results$vcov_spatial_hac))
      tstat_hac <- coefs_hac / se_spatial_hac
      pval_hac <- 2 * pt(abs(tstat_hac),
                         df = nrow(data_model_df) - length(coefs_hac),
                         lower.tail = FALSE)

      results_hac <- rbind(results_hac, data.frame(
        outcome = dep_label,
        lag = lag,
        model = "mun_year_trend",
        se_type = "spatial_hac",
        coef_temp = coefs_hac["cont_shock_temp_dm"],
        se_temp = se_spatial_hac["cont_shock_temp_dm"],
        pval_temp = pval_hac["cont_shock_temp_dm"],
        coef_precip = coefs_hac["cont_shock_precip_dm"],
        se_precip = se_spatial_hac["cont_shock_precip_dm"],
        pval_precip = pval_hac["cont_shock_precip_dm"],
        coef_trend = coefs_hac["year_state_trend_dm"],
        se_trend = se_spatial_hac["year_state_trend_dm"],
        pval_trend = pval_hac["year_state_trend_dm"],
        n_obs = nrow(data_model_df),
        r2 = NA_real_,
        r2_adj = NA_real_
      ))
    }, error = function(e) {
      warning(paste("Spatial + serial HAC (state-trend) failed for", dep_label, ":", e$message))
    })
  }
}

# ============================================================================
# STEP FIVE: SAVE RESULTS
# ============================================================================

if (!dir.exists("./output/placebo_tests")) {
  dir.create("./output/placebo_tests", recursive = TRUE)
}

save(results_clustered, results_hac,
     file = "./output/placebo_tests/patents_lagged_outcome_placebo_municipal_cluster.RData")

write.csv(results_clustered,
          file = "./output/placebo_tests/patents_lagged_outcome_placebo_municipal_cluster_clustered.csv",
          row.names = FALSE)

write.csv(results_hac,
          file = "./output/placebo_tests/patents_lagged_outcome_placebo_municipal_cluster_hac.csv",
          row.names = FALSE)

cat("\nResults saved to:\n")
cat("  - ./output/placebo_tests/patents_lagged_outcome_placebo_municipal_cluster.RData\n")
cat("  - ./output/placebo_tests/patents_lagged_outcome_placebo_municipal_cluster_clustered.csv\n")
cat("  - ./output/placebo_tests/patents_lagged_outcome_placebo_municipal_cluster_hac.csv\n")
cat("\nScript completed.\n")
