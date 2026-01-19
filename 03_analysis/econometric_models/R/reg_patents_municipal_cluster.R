# ============================================================================
# SHORT-RUN PANEL ESTIMATES - PATENT OUTCOMES (2000-2020)
# Municipality clustering, with spatial + serial HAC for state-trend models
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
# STEP ONE: LOAD AND PREPARE DATA
# ============================================================================

cat("Loading patent data...\n")

# Load data
data <- read_dta("./output/final_base/weather_patent.dta")

# Filter for years 2000-2020
data <- data %>%
  filter(year >= 2000 & year <= 2020)

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
dep_vars <- c("qtd_pat", "qtd_pat_verde", "prop_verde")
dep_labels <- c("Total Patents", "Green Patents", "Prop. Green Patents")

# ============================================================================
# STEP THREE: PREPARE CLEAN DATA FOR SPATIAL HAC
# ============================================================================

cat("Preparing clean data for spatial HAC...\n")

data_clean <- as.data.table(data)
data_clean <- data_clean[!is.na(lat) & !is.na(lon) &
                           !is.na(cont_shock_temp) & !is.na(cont_shock_precip) &
                           !is.na(year_state_trend)]

data_clean <- data_clean[!is.na(qtd_pat) | !is.na(qtd_pat_verde) | !is.na(prop_verde)]

cat("Clean data observations:", nrow(data_clean), "\n\n")

data_clean_df <- as.data.frame(data_clean)

vars_to_demean <- c(dep_vars, "cont_shock_temp", "cont_shock_precip", "year_state_trend")

demean_two_way <- function(df, fe_var1, fe_var2, vars) {
  df_out <- df
  fe_list <- list(
    fe1 = factor(df[[fe_var1]]),
    fe2 = factor(df[[fe_var2]])
  )

  for (var in vars) {
    if (all(is.na(df[[var]]))) {
      next
    }

    temp_df <- data.frame(
      y = df[[var]],
      fe1 = fe_list$fe1,
      fe2 = fe_list$fe2
    )

    demeaned <- lfe::demeanlist(temp_df[, "y", drop = FALSE],
                                fl = fe_list,
                                na.rm = TRUE)

    df_out[[paste0(var, "_dm")]] <- demeaned[[1]]
  }

  df_out
}

# ============================================================================
# STEP FOUR: RUN REGRESSIONS
# ============================================================================

cat("Running regressions...\n\n")

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
  k <- ncol(X)
  xeeX <- matrix(0, k, k)

  if (lag_cutoff <= 0) {
    return(xeeX)
  }

  panel_unique <- unique(panel)

  for (pi in panel_unique) {
    idx <- which(panel == pi)
    X1 <- X[idx, , drop = FALSE]
    e1 <- resid[idx]
    time1 <- time[idx]

    n1 <- length(e1)
    if (n1 == 0) next

    for (t in seq_len(n1)) {
      time_diff <- abs(time1[t] - time1)
      weight <- 1 - (time_diff / (lag_cutoff + 1))
      window <- (time_diff <= lag_cutoff) * weight
      window <- window * (time1[t] != time1)

      weighted_resid <- e1 * window
      xeeXh <- (t(X1[t, , drop = FALSE]) * e1[t]) %*%
        (t(weighted_resid) %*% X1)
      xeeX <- xeeX + xeeXh
    }
  }

  xeeX
}

compute_spatial_hac_vcov <- function(data, y_var, x_vars, lat_var, lon_var,
                                     time_var, panel_var, dist_cutoff,
                                     lag_cutoff, bartlett = FALSE) {
  y <- data[[y_var]]
  X <- as.matrix(data[, x_vars, drop = FALSE])
  n <- nrow(X)

  fit <- lm.fit(x = X, y = y)
  resid <- fit$residuals

  inv_xx <- solve(crossprod(X)) * n

  xeeX_spatial <- compute_spatial_xeeX(
    X = X,
    resid = resid,
    lat = data[[lat_var]],
    lon = data[[lon_var]],
    time = data[[time_var]],
    dist_cutoff = dist_cutoff,
    bartlett = bartlett
  )

  xeeX_serial <- compute_serial_xeeX(
    X = X,
    resid = resid,
    time = data[[time_var]],
    panel = data[[panel_var]],
    lag_cutoff = lag_cutoff
  )

  xeeX_spatial_scaled <- xeeX_spatial / n
  xeeX_spatial_hac_scaled <- (xeeX_spatial + xeeX_serial) / n

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

# Initialize results storage
results_basic <- list()
results_basic_hac <- list()
results_state_year <- list()
results_state_year_hac <- list()
results_mun_year_trend <- list()
results_mun_year_trend_hac <- list()

# Loop through each dependent variable
for(i in 1:length(dep_vars)) {

  dv <- dep_vars[i]
  cat("============================================\n")
  cat("Model", i, "-", dep_labels[i], "\n")
  cat("============================================\n")

  # -------------------------------------------------------------------------
  # Model 1: Municipal + Year Fixed Effects (Baseline)
  # -------------------------------------------------------------------------

  cat("\n[1] Baseline: Municipality FE + Year FE + Clustering Municipality\n")

  formula_basic <- as.formula(paste0(dv, " ~ cont_shock_temp + cont_shock_precip | mun_code + year"))

  model_basic <- feols(formula_basic, data = data, cluster = ~mun_code)

  results_basic[[i]] <- list(
    model = model_basic,
    coef_temp = coef(model_basic)["cont_shock_temp"],
    se_temp = se(model_basic)["cont_shock_temp"],
    pval_temp = pvalue(model_basic)["cont_shock_temp"],
    coef_precip = coef(model_basic)["cont_shock_precip"],
    se_precip = se(model_basic)["cont_shock_precip"],
    pval_precip = pvalue(model_basic)["cont_shock_precip"],
    n_obs = nobs(model_basic),
    r2 = r2(model_basic, type = "r2"),
    r2_adj = r2(model_basic, type = "ar2")
  )

  tryCatch({
    data_model_df <- demean_two_way(data_clean_df, "mun_code", "year", vars_to_demean)
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
      bartlett = TRUE
    )

    coefs_hac <- hac_results$coefficients
    se_spatial_hac <- sqrt(diag(hac_results$vcov_spatial_hac))
    tstat_hac <- coefs_hac / se_spatial_hac
    pval_hac <- 2 * pt(abs(tstat_hac),
                       df = nrow(data_model_df) - length(coefs_hac),
                       lower.tail = FALSE)

    results_basic_hac[[i]] <- list(
      coef_temp = coefs_hac["cont_shock_temp_dm"],
      se_temp = se_spatial_hac["cont_shock_temp_dm"],
      pval_temp = pval_hac["cont_shock_temp_dm"],
      coef_precip = coefs_hac["cont_shock_precip_dm"],
      se_precip = se_spatial_hac["cont_shock_precip_dm"],
      pval_precip = pval_hac["cont_shock_precip_dm"]
    )
  }, error = function(e) {
    warning(paste("Spatial + serial HAC (baseline) failed for", dep_labels[i], ":", e$message))
    results_basic_hac[[i]] <<- NULL
  })

  cat("  Temperature coef:   ", sprintf("%.4f", results_basic[[i]]$coef_temp), "\n")
  cat("  Temperature SE:     ", sprintf("%.4f", results_basic[[i]]$se_temp), "\n")
  cat("  Temperature p-value:", sprintf("%.4f", results_basic[[i]]$pval_temp), "\n")
  cat("  Precipitation coef: ", sprintf("%.4f", results_basic[[i]]$coef_precip), "\n")
  cat("  Precipitation SE:   ", sprintf("%.4f", results_basic[[i]]$se_precip), "\n")
  cat("  Precipitation p-val:", sprintf("%.4f", results_basic[[i]]$pval_precip), "\n")
  cat("  Observations:       ", results_basic[[i]]$n_obs, "\n")
  cat("  R²:                 ", sprintf("%.3f", results_basic[[i]]$r2), "\n")

  # -------------------------------------------------------------------------
  # Model 2: Municipal + State-Year Fixed Effects
  # -------------------------------------------------------------------------

  cat("\n[2] State-Year FE: Municipality FE + State×Year FE + Clustering Municipality\n")

  formula_state_year <- as.formula(paste0(dv, " ~ cont_shock_temp + cont_shock_precip | mun_code + state_year"))

  model_state_year <- feols(formula_state_year, data = data, cluster = ~mun_code)

  results_state_year[[i]] <- list(
    model = model_state_year,
    coef_temp = coef(model_state_year)["cont_shock_temp"],
    se_temp = se(model_state_year)["cont_shock_temp"],
    pval_temp = pvalue(model_state_year)["cont_shock_temp"],
    coef_precip = coef(model_state_year)["cont_shock_precip"],
    se_precip = se(model_state_year)["cont_shock_precip"],
    pval_precip = pvalue(model_state_year)["cont_shock_precip"],
    n_obs = nobs(model_state_year),
    r2 = r2(model_state_year, type = "r2"),
    r2_adj = r2(model_state_year, type = "ar2")
  )

  tryCatch({
    data_model_df <- demean_two_way(data_clean_df, "mun_code", "state_year", vars_to_demean)
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
      bartlett = TRUE
    )

    coefs_hac <- hac_results$coefficients
    se_spatial_hac <- sqrt(diag(hac_results$vcov_spatial_hac))
    tstat_hac <- coefs_hac / se_spatial_hac
    pval_hac <- 2 * pt(abs(tstat_hac),
                       df = nrow(data_model_df) - length(coefs_hac),
                       lower.tail = FALSE)

    results_state_year_hac[[i]] <- list(
      coef_temp = coefs_hac["cont_shock_temp_dm"],
      se_temp = se_spatial_hac["cont_shock_temp_dm"],
      pval_temp = pval_hac["cont_shock_temp_dm"],
      coef_precip = coefs_hac["cont_shock_precip_dm"],
      se_precip = se_spatial_hac["cont_shock_precip_dm"],
      pval_precip = pval_hac["cont_shock_precip_dm"]
    )
  }, error = function(e) {
    warning(paste("Spatial + serial HAC (state-year FE) failed for", dep_labels[i], ":", e$message))
    results_state_year_hac[[i]] <<- NULL
  })

  cat("  Temperature coef:   ", sprintf("%.4f", results_state_year[[i]]$coef_temp), "\n")
  cat("  Temperature SE:     ", sprintf("%.4f", results_state_year[[i]]$se_temp), "\n")
  cat("  Temperature p-value:", sprintf("%.4f", results_state_year[[i]]$pval_temp), "\n")
  cat("  Precipitation coef: ", sprintf("%.4f", results_state_year[[i]]$coef_precip), "\n")
  cat("  Precipitation SE:   ", sprintf("%.4f", results_state_year[[i]]$se_precip), "\n")
  cat("  Precipitation p-val:", sprintf("%.4f", results_state_year[[i]]$pval_precip), "\n")
  cat("  Observations:       ", results_state_year[[i]]$n_obs, "\n")
  cat("  R²:                 ", sprintf("%.3f", results_state_year[[i]]$r2), "\n\n")

  # -------------------------------------------------------------------------
  # Model 3: Municipality-Year FE + State-Year Trends
  # -------------------------------------------------------------------------

  cat("\n[3] Mun-Year FE + State-Year Trends: Municipality×Year FE + State×Year Trends + Clustering Municipality\n")

  formula_mun_year_trend <- as.formula(paste0(dv, " ~ cont_shock_temp + cont_shock_precip + year_state_trend | mun_code + year"))

  model_mun_year_trend <- feols(formula_mun_year_trend, data = data, cluster = ~mun_code)

  results_mun_year_trend[[i]] <- list(
    model = model_mun_year_trend,
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
  )

  tryCatch({
    data_model_df <- demean_two_way(data_clean_df, "mun_code", "year", vars_to_demean)
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
      bartlett = TRUE
    )

    coefs_hac <- hac_results$coefficients
    se_spatial_hac <- sqrt(diag(hac_results$vcov_spatial_hac))
    tstat_hac <- coefs_hac / se_spatial_hac
    pval_hac <- 2 * pt(abs(tstat_hac),
                       df = nrow(data_model_df) - length(coefs_hac),
                       lower.tail = FALSE)

    results_mun_year_trend_hac[[i]] <- list(
      model = model_mun_year_trend,
      coef_temp = coefs_hac["cont_shock_temp_dm"],
      se_temp = se_spatial_hac["cont_shock_temp_dm"],
      pval_temp = pval_hac["cont_shock_temp_dm"],
      coef_precip = coefs_hac["cont_shock_precip_dm"],
      se_precip = se_spatial_hac["cont_shock_precip_dm"],
      pval_precip = pval_hac["cont_shock_precip_dm"],
      coef_trend = coefs_hac["year_state_trend_dm"],
      se_trend = se_spatial_hac["year_state_trend_dm"],
      pval_trend = pval_hac["year_state_trend_dm"],
      n_obs = nobs(model_mun_year_trend),
      r2 = r2(model_mun_year_trend, type = "r2"),
      r2_adj = r2(model_mun_year_trend, type = "ar2")
    )
  }, error = function(e) {
    warning(paste("Spatial + serial HAC calculation failed for", dep_labels[i], ":", e$message))
    warning("Using clustered standard errors instead.")

    results_mun_year_trend_hac[[i]] <<- list(
      model = model_mun_year_trend,
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
    )
  })

  cat("  Temperature coef:   ", sprintf("%.4f", results_mun_year_trend[[i]]$coef_temp), "\n")
  cat("  Temperature SE:     ", sprintf("%.4f", results_mun_year_trend[[i]]$se_temp), "\n")
  cat("  Temperature p-value:", sprintf("%.4f", results_mun_year_trend[[i]]$pval_temp), "\n")
  cat("  Precipitation coef: ", sprintf("%.4f", results_mun_year_trend[[i]]$coef_precip), "\n")
  cat("  Precipitation SE:   ", sprintf("%.4f", results_mun_year_trend[[i]]$se_precip), "\n")
  cat("  Precipitation p-val:", sprintf("%.4f", results_mun_year_trend[[i]]$pval_precip), "\n")
  cat("  State-Year Trend:   ", sprintf("%.4f", results_mun_year_trend[[i]]$coef_trend), "\n")
  cat("  Trend SE:           ", sprintf("%.4f", results_mun_year_trend[[i]]$se_trend), "\n")
  cat("  Observations:       ", results_mun_year_trend[[i]]$n_obs, "\n")
  cat("  R²:                 ", sprintf("%.3f", results_mun_year_trend[[i]]$r2), "\n\n")
}

# ============================================================================
# STEP FOUR: GENERATE LATEX TABLES
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("Generating LaTeX tables...\n")
cat(rep("=", 70), "\n\n", sep = "")

# Helper functions
add_stars <- function(pval) {
  if(is.na(pval)) return("")
  if(pval < 0.01) return("***")
  if(pval < 0.05) return("**")
  if(pval < 0.10) return("*")
  return("")
}

format_coef <- function(x, digits = 3) {
  if(is.na(x)) return("NA")
  sprintf(paste0("%.", digits, "f"), x)
}

extract_or_na <- function(result_list, i, field) {
  if (is.null(result_list[[i]])) {
    return(NA_real_)
  }
  result_list[[i]][[field]]
}

# -------------------------------------------------------------------------
# TABLE 1: Comparison Table (Both Specifications)
# -------------------------------------------------------------------------

latex_comparison <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{threeparttable}",
  "\\caption{Weather Shocks and Patents: Municipal Clustering}",
  "\\label{tab:patents_comparison}",
  "\\begin{tabular}{lcccccc}",
  "\\toprule",
  "\\multicolumn{7}{c}{\\textbf{Weather Shocks and Patents (2000-2020)}}\\\\",
  "\\midrule",
  " & \\multicolumn{3}{c}{\\textbf{Year FE}} & \\multicolumn{3}{c}{\\textbf{State×Year FE}} \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
  "Dependent Variable:",
  paste0("  & (1) ", dep_labels[1]),
  paste0("  & (2) ", dep_labels[2]),
  paste0("  & (3) ", dep_labels[3]),
  paste0("  & (4) ", dep_labels[1]),
  paste0("  & (5) ", dep_labels[2]),
  paste0("  & (6) ", dep_labels[3], " \\\\"),
  "\\midrule"
)

# Temperature coefficient
temp_row <- "$T_{mt}$"
temp_se_row <- ""
for(i in 1:3) {
  coef_val <- results_basic[[i]]$coef_temp
  se_val <- results_basic[[i]]$se_temp
  pval <- results_basic[[i]]$pval_temp
  stars <- add_stars(pval)
  digits <- ifelse(abs(coef_val) < 0.01, 4, 3)
  temp_row <- paste0(temp_row, "  &   ", format_coef(coef_val, digits), stars)
  temp_se_row <- paste0(temp_se_row, "  & (", format_coef(se_val, digits), ")")
}
for(i in 1:3) {
  coef_val <- results_state_year[[i]]$coef_temp
  se_val <- results_state_year[[i]]$se_temp
  pval <- results_state_year[[i]]$pval_temp
  stars <- add_stars(pval)
  digits <- ifelse(abs(coef_val) < 0.01, 4, 3)
  temp_row <- paste0(temp_row, "  &   ", format_coef(coef_val, digits), stars)
  temp_se_row <- paste0(temp_se_row, "  & (", format_coef(se_val, digits), ")")
}
temp_row <- paste0(temp_row, " \\\\")
temp_se_row <- paste0(temp_se_row, "  \\\\[0.5em]")

latex_comparison <- c(latex_comparison, temp_row, temp_se_row)

# Precipitation coefficient
precip_row <- "$P_{mt}$"
precip_se_row <- ""
for(i in 1:3) {
  coef_val <- results_basic[[i]]$coef_precip
  se_val <- results_basic[[i]]$se_precip
  pval <- results_basic[[i]]$pval_precip
  stars <- add_stars(pval)
  digits <- ifelse(abs(coef_val) < 0.01, 4, 3)
  precip_row <- paste0(precip_row, "  &   ", format_coef(coef_val, digits), stars)
  precip_se_row <- paste0(precip_se_row, "  & (", format_coef(se_val, digits), ")")
}
for(i in 1:3) {
  coef_val <- results_state_year[[i]]$coef_precip
  se_val <- results_state_year[[i]]$se_precip
  pval <- results_state_year[[i]]$pval_precip
  stars <- add_stars(pval)
  digits <- ifelse(abs(coef_val) < 0.01, 4, 3)
  precip_row <- paste0(precip_row, "  &   ", format_coef(coef_val, digits), stars)
  precip_se_row <- paste0(precip_se_row, "  & (", format_coef(se_val, digits), ")")
}
precip_row <- paste0(precip_row, " \\\\")
precip_se_row <- paste0(precip_se_row, "  \\\\")

latex_comparison <- c(latex_comparison, precip_row, precip_se_row)

# Footer
latex_comparison <- c(
  latex_comparison,
  "\\midrule",
  paste0("Observations & ",
         format(results_basic[[1]]$n_obs, big.mark = ","), " &",
         format(results_basic[[2]]$n_obs, big.mark = ","), " &",
         format(results_basic[[3]]$n_obs, big.mark = ","), " &",
         format(results_state_year[[1]]$n_obs, big.mark = ","), " &",
         format(results_state_year[[2]]$n_obs, big.mark = ","), " &",
         format(results_state_year[[3]]$n_obs, big.mark = ","), " \\\\"),
  paste0("$R^2$ & ",
         format_coef(results_basic[[1]]$r2, 3), " & ",
         format_coef(results_basic[[2]]$r2, 3), " & ",
         format_coef(results_basic[[3]]$r2, 3), " & ",
         format_coef(results_state_year[[1]]$r2, 3), " & ",
         format_coef(results_state_year[[2]]$r2, 3), " & ",
         format_coef(results_state_year[[3]]$r2, 3), " \\\\"),
  "Municipality FE & Yes & Yes & Yes & Yes & Yes & Yes \\\\",
  "Year FE         & Yes & Yes & Yes & No  & No  & No  \\\\",
  "State×Year FE   & No  & No  & No  & Yes & Yes & Yes \\\\",
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}",
  "\\footnotesize",
  "\\item \\textit{Note:} Dependent variables are Total Patents (columns 1,4), Green Patents (columns 2,5), and Proportion of Green Patents (columns 3,6). Temperature ($T_{mt}$) and precipitation ($P_{mt}$) are measured as standard deviations from historical means. Columns (1)-(3) include municipality and year fixed effects. Columns (4)-(6) include municipality and state×year fixed effects. All standard errors are clustered at the municipality level and shown in parentheses.",
  "\\item \\textsuperscript{*} p < 0.10, \\textsuperscript{**} p < 0.05, \\textsuperscript{***} p < 0.01",
  "\\end{tablenotes}",
  "\\end{threeparttable}%",
  "}",
  "\\end{table}"
)

# Write table
output_file <- "table_patents_municipal_cluster_comparison.tex"
writeLines(latex_comparison, output_file)
cat("LaTeX table saved to:", output_file, "\n")

# -------------------------------------------------------------------------
# TABLE 1B: Comparison Table (HAC Standard Errors)
# -------------------------------------------------------------------------

latex_comparison_hac <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{threeparttable}",
  "\\caption{Weather Shocks and Patents: Spatial + Serial HAC}",
  "\\label{tab:patents_comparison_hac}",
  "\\begin{tabular}{lcccccc}",
  "\\toprule",
  "\\multicolumn{7}{c}{\\textbf{Weather Shocks and Patents (2000-2020)}}\\\\",
  "\\midrule",
  " & \\multicolumn{3}{c}{\\textbf{Year FE}} & \\multicolumn{3}{c}{\\textbf{State×Year FE}} \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
  "Dependent Variable:",
  paste0("  & (1) ", dep_labels[1]),
  paste0("  & (2) ", dep_labels[2]),
  paste0("  & (3) ", dep_labels[3]),
  paste0("  & (4) ", dep_labels[1]),
  paste0("  & (5) ", dep_labels[2]),
  paste0("  & (6) ", dep_labels[3], " \\\\"),
  "\\midrule"
)

temp_row <- "$T_{mt}$"
temp_se_row <- ""
for(i in 1:3) {
  coef_val <- extract_or_na(results_basic_hac, i, "coef_temp")
  se_val <- extract_or_na(results_basic_hac, i, "se_temp")
  pval <- extract_or_na(results_basic_hac, i, "pval_temp")
  stars <- add_stars(pval)
  digits <- ifelse(is.na(coef_val), 3, ifelse(abs(coef_val) < 0.01, 4, 3))
  temp_row <- paste0(temp_row, "  &   ", format_coef(coef_val, digits), stars)
  temp_se_row <- paste0(temp_se_row, "  & (", format_coef(se_val, digits), ")")
}
for(i in 1:3) {
  coef_val <- extract_or_na(results_state_year_hac, i, "coef_temp")
  se_val <- extract_or_na(results_state_year_hac, i, "se_temp")
  pval <- extract_or_na(results_state_year_hac, i, "pval_temp")
  stars <- add_stars(pval)
  digits <- ifelse(is.na(coef_val), 3, ifelse(abs(coef_val) < 0.01, 4, 3))
  temp_row <- paste0(temp_row, "  &   ", format_coef(coef_val, digits), stars)
  temp_se_row <- paste0(temp_se_row, "  & (", format_coef(se_val, digits), ")")
}
temp_row <- paste0(temp_row, " \\\\")
temp_se_row <- paste0(temp_se_row, "  \\\\[0.5em]")

latex_comparison_hac <- c(latex_comparison_hac, temp_row, temp_se_row)

precip_row <- "$P_{mt}$"
precip_se_row <- ""
for(i in 1:3) {
  coef_val <- extract_or_na(results_basic_hac, i, "coef_precip")
  se_val <- extract_or_na(results_basic_hac, i, "se_precip")
  pval <- extract_or_na(results_basic_hac, i, "pval_precip")
  stars <- add_stars(pval)
  digits <- ifelse(is.na(coef_val), 3, ifelse(abs(coef_val) < 0.01, 4, 3))
  precip_row <- paste0(precip_row, "  &   ", format_coef(coef_val, digits), stars)
  precip_se_row <- paste0(precip_se_row, "  & (", format_coef(se_val, digits), ")")
}
for(i in 1:3) {
  coef_val <- extract_or_na(results_state_year_hac, i, "coef_precip")
  se_val <- extract_or_na(results_state_year_hac, i, "se_precip")
  pval <- extract_or_na(results_state_year_hac, i, "pval_precip")
  stars <- add_stars(pval)
  digits <- ifelse(is.na(coef_val), 3, ifelse(abs(coef_val) < 0.01, 4, 3))
  precip_row <- paste0(precip_row, "  &   ", format_coef(coef_val, digits), stars)
  precip_se_row <- paste0(precip_se_row, "  & (", format_coef(se_val, digits), ")")
}
precip_row <- paste0(precip_row, " \\\\")
precip_se_row <- paste0(precip_se_row, "  \\\\")

latex_comparison_hac <- c(latex_comparison_hac, precip_row, precip_se_row)

latex_comparison_hac <- c(
  latex_comparison_hac,
  "\\midrule",
  paste0("Observations & ",
         format(results_basic[[1]]$n_obs, big.mark = ","), " &",
         format(results_basic[[2]]$n_obs, big.mark = ","), " &",
         format(results_basic[[3]]$n_obs, big.mark = ","), " &",
         format(results_state_year[[1]]$n_obs, big.mark = ","), " &",
         format(results_state_year[[2]]$n_obs, big.mark = ","), " &",
         format(results_state_year[[3]]$n_obs, big.mark = ","), " \\\\"),
  "Municipality FE & Yes & Yes & Yes & Yes & Yes & Yes \\\\",
  "Year FE         & Yes & Yes & Yes & No  & No  & No  \\\\",
  "State×Year FE   & No  & No  & No  & Yes & Yes & Yes \\\\",
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}",
  "\\footnotesize",
  "\\item \\textit{Note:} Standard errors are spatial (Conley 1999, 250 km) and serial (Newey-West 1987, 6-year lags) HAC.",
  "\\item \\textsuperscript{*} p < 0.10, \\textsuperscript{**} p < 0.05, \\textsuperscript{***} p < 0.01",
  "\\end{tablenotes}",
  "\\end{threeparttable}%",
  "}",
  "\\end{table}"
)

output_file_hac <- "table_patents_municipal_cluster_comparison_hac.tex"
writeLines(latex_comparison_hac, output_file_hac)
cat("LaTeX HAC table saved to:", output_file_hac, "\n")

# -------------------------------------------------------------------------
# TABLE 2: Municipality-Year Fixed Effects + State-Year Trends
# -------------------------------------------------------------------------

latex_mun_year_trend <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\resizebox{0.75\\textwidth}{!}{%",
  "\\begin{threeparttable}",
  "\\caption{Municipality-Year Fixed Effects with State-Year Trends - Patents}",
  "\\label{tab:patents_mun_year_trend}",
  "\\begin{tabular}{lccc}",
  "\\toprule",
  "\\multicolumn{4}{c}{\\textbf{Weather Shocks and Patents - Municipality×Year FE + State Trends}}\\\\",
  "\\midrule",
  "Dependent Variable:",
  paste0("  & (1) ", dep_labels[1]),
  paste0("  & (2) ", dep_labels[2]),
  paste0("  & (3) ", dep_labels[3], " \\\\"),
  "\\midrule"
)

# Temperature coefficient
temp_row <- "$T_{mt}$ (Temperature)"
temp_se_row <- ""
for(i in 1:3) {
  coef_val <- results_mun_year_trend[[i]]$coef_temp
  se_val <- results_mun_year_trend[[i]]$se_temp
  pval <- results_mun_year_trend[[i]]$pval_temp
  stars <- add_stars(pval)
  digits <- ifelse(abs(coef_val) < 0.01, 4, 3)
  temp_row <- paste0(temp_row, "  &   ", format_coef(coef_val, digits), stars)
  temp_se_row <- paste0(temp_se_row, "  & (", format_coef(se_val, digits), ")")
}
temp_row <- paste0(temp_row, " \\\\")
temp_se_row <- paste0(temp_se_row, "  \\\\[0.5em]")

latex_mun_year_trend <- c(latex_mun_year_trend, temp_row, temp_se_row)

# Precipitation coefficient
precip_row <- "$P_{mt}$ (Precipitation)"
precip_se_row <- ""
for(i in 1:3) {
  coef_val <- results_mun_year_trend[[i]]$coef_precip
  se_val <- results_mun_year_trend[[i]]$se_precip
  pval <- results_mun_year_trend[[i]]$pval_precip
  stars <- add_stars(pval)
  digits <- ifelse(abs(coef_val) < 0.01, 4, 3)
  precip_row <- paste0(precip_row, "  &   ", format_coef(coef_val, digits), stars)
  precip_se_row <- paste0(precip_se_row, "  & (", format_coef(se_val, digits), ")")
}
precip_row <- paste0(precip_row, " \\\\")
precip_se_row <- paste0(precip_se_row, "  \\\\[0.5em]")

latex_mun_year_trend <- c(latex_mun_year_trend, precip_row, precip_se_row)

# State-Year Trend coefficient
trend_row <- "$year \\times state$ (State-Year Trend)"
trend_se_row <- ""
for(i in 1:3) {
  coef_val <- results_mun_year_trend[[i]]$coef_trend
  se_val <- results_mun_year_trend[[i]]$se_trend
  pval <- results_mun_year_trend[[i]]$pval_trend
  stars <- add_stars(pval)
  digits <- ifelse(abs(coef_val) < 0.01, 4, 3)
  trend_row <- paste0(trend_row, "  &   ", format_coef(coef_val, digits), stars)
  trend_se_row <- paste0(trend_se_row, "  & (", format_coef(se_val, digits), ")")
}
trend_row <- paste0(trend_row, " \\\\")
trend_se_row <- paste0(trend_se_row, "  \\\\")

latex_mun_year_trend <- c(latex_mun_year_trend, trend_row, trend_se_row)

# Footer
latex_mun_year_trend <- c(
  latex_mun_year_trend,
  "\\midrule",
  paste0("Observations        & ",
         format(results_mun_year_trend[[1]]$n_obs, big.mark = ","), " &",
         format(results_mun_year_trend[[2]]$n_obs, big.mark = ","), " &",
         format(results_mun_year_trend[[3]]$n_obs, big.mark = ","), " \\\\"),
  paste0("$R^2$                &  ",
         format_coef(results_mun_year_trend[[1]]$r2, 3), " & ",
         format_coef(results_mun_year_trend[[2]]$r2, 3), " & ",
         format_coef(results_mun_year_trend[[3]]$r2, 3), " \\\\"),
  paste0("Adjusted $R^2$       &  ",
         format_coef(results_mun_year_trend[[1]]$r2_adj, 3), " & ",
         format_coef(results_mun_year_trend[[2]]$r2_adj, 3), " & ",
         format_coef(results_mun_year_trend[[3]]$r2_adj, 3), " \\\\"),
  "Municipality×Year FE &  Yes & Yes & Yes \\\\",
  "State-Year Trends    &  Yes & Yes & Yes \\\\",
  "Clustering           &  Municipality & Municipality & Municipality \\\\",
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}",
  "\\footnotesize",
  "\\item \\textit{Note:} Dependent variables are Total Patents (1), Green Patents (2), and Proportion of Green Patents (3). Temperature and precipitation are measured as standard deviations from historical means. All regressions include municipality×year fixed effects and state-specific linear time trends (year×state). Standard errors use spatial (Conley 1999, 250 km) and serial (Newey-West 1987, 6-year lags) HAC adjustments following the translated Stata ols\\_spatial\\_HAC logic, shown in parentheses.",
  "\\item \\textsuperscript{*} p < 0.10, \\textsuperscript{**} p < 0.05, \\textsuperscript{***} p < 0.01",
  "\\end{tablenotes}",
  "\\end{threeparttable}%",
  "}",
  "\\end{table}"
)

# Write table 2
output_file_mun_year_trend <- "table_patents_municipal_cluster_mun_year_trend.tex"
writeLines(latex_mun_year_trend, output_file_mun_year_trend)
cat("Table 2 saved to:", output_file_mun_year_trend, "\n")

# -------------------------------------------------------------------------
# TABLE 2B: Municipality-Year FE + State-Year Trends (HAC Standard Errors)
# -------------------------------------------------------------------------

latex_mun_year_trend_hac <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\resizebox{0.75\\textwidth}{!}{%",
  "\\begin{threeparttable}",
  "\\caption{Municipality-Year Fixed Effects with State-Year Trends - Patents (HAC)}",
  "\\label{tab:patents_mun_year_trend_hac}",
  "\\begin{tabular}{lccc}",
  "\\toprule",
  "\\multicolumn{4}{c}{\\textbf{Weather Shocks and Patents - Municipality×Year FE + State Trends}}\\\\",
  "\\midrule",
  "Dependent Variable:",
  paste0("  & (1) ", dep_labels[1]),
  paste0("  & (2) ", dep_labels[2]),
  paste0("  & (3) ", dep_labels[3], " \\\\"),
  "\\midrule"
)

temp_row <- "$T_{mt}$ (Temperature)"
temp_se_row <- ""
for(i in 1:3) {
  coef_val <- extract_or_na(results_mun_year_trend_hac, i, "coef_temp")
  se_val <- extract_or_na(results_mun_year_trend_hac, i, "se_temp")
  pval <- extract_or_na(results_mun_year_trend_hac, i, "pval_temp")
  stars <- add_stars(pval)
  digits <- ifelse(is.na(coef_val), 3, ifelse(abs(coef_val) < 0.01, 4, 3))
  temp_row <- paste0(temp_row, "  &   ", format_coef(coef_val, digits), stars)
  temp_se_row <- paste0(temp_se_row, "  & (", format_coef(se_val, digits), ")")
}
temp_row <- paste0(temp_row, " \\\\")
temp_se_row <- paste0(temp_se_row, "  \\\\[0.5em]")

latex_mun_year_trend_hac <- c(latex_mun_year_trend_hac, temp_row, temp_se_row)

precip_row <- "$P_{mt}$ (Precipitation)"
precip_se_row <- ""
for(i in 1:3) {
  coef_val <- extract_or_na(results_mun_year_trend_hac, i, "coef_precip")
  se_val <- extract_or_na(results_mun_year_trend_hac, i, "se_precip")
  pval <- extract_or_na(results_mun_year_trend_hac, i, "pval_precip")
  stars <- add_stars(pval)
  digits <- ifelse(is.na(coef_val), 3, ifelse(abs(coef_val) < 0.01, 4, 3))
  precip_row <- paste0(precip_row, "  &   ", format_coef(coef_val, digits), stars)
  precip_se_row <- paste0(precip_se_row, "  & (", format_coef(se_val, digits), ")")
}
precip_row <- paste0(precip_row, " \\\\")
precip_se_row <- paste0(precip_se_row, "  \\\\[0.5em]")

latex_mun_year_trend_hac <- c(latex_mun_year_trend_hac, precip_row, precip_se_row)

trend_row <- "$year \\times state$ (State-Year Trend)"
trend_se_row <- ""
for(i in 1:3) {
  coef_val <- extract_or_na(results_mun_year_trend_hac, i, "coef_trend")
  se_val <- extract_or_na(results_mun_year_trend_hac, i, "se_trend")
  pval <- extract_or_na(results_mun_year_trend_hac, i, "pval_trend")
  stars <- add_stars(pval)
  digits <- ifelse(is.na(coef_val), 3, ifelse(abs(coef_val) < 0.01, 4, 3))
  trend_row <- paste0(trend_row, "  &   ", format_coef(coef_val, digits), stars)
  trend_se_row <- paste0(trend_se_row, "  & (", format_coef(se_val, digits), ")")
}
trend_row <- paste0(trend_row, " \\\\")
trend_se_row <- paste0(trend_se_row, "  \\\\")

latex_mun_year_trend_hac <- c(latex_mun_year_trend_hac, trend_row, trend_se_row)

latex_mun_year_trend_hac <- c(
  latex_mun_year_trend_hac,
  "\\midrule",
  paste0("Observations        & ",
         format(results_mun_year_trend[[1]]$n_obs, big.mark = ","), " &",
         format(results_mun_year_trend[[2]]$n_obs, big.mark = ","), " &",
         format(results_mun_year_trend[[3]]$n_obs, big.mark = ","), " \\\\"),
  "Municipality×Year FE &  Yes & Yes & Yes \\\\",
  "State-Year Trends    &  Yes & Yes & Yes \\\\",
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}",
  "\\footnotesize",
  "\\item \\textit{Note:} Standard errors are spatial (Conley 1999, 250 km) and serial (Newey-West 1987, 6-year lags) HAC.",
  "\\item \\textsuperscript{*} p < 0.10, \\textsuperscript{**} p < 0.05, \\textsuperscript{***} p < 0.01",
  "\\end{tablenotes}",
  "\\end{threeparttable}%",
  "}",
  "\\end{table}"
)

output_file_mun_year_trend_hac <- "table_patents_municipal_cluster_mun_year_trend_hac.tex"
writeLines(latex_mun_year_trend_hac, output_file_mun_year_trend_hac)
cat("Table 2 HAC saved to:", output_file_mun_year_trend_hac, "\n")

# ============================================================================
# STEP FIVE: PRINT RESULTS SUMMARY
# ============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("RESULTS SUMMARY - PATENTS\n")
cat(rep("=", 80), "\n\n", sep = "")

for(i in 1:3) {
  cat(rep("-", 80), "\n", sep = "")
  cat(dep_labels[i], "\n")
  cat(rep("-", 80), "\n", sep = "")

  cat("\n[BASELINE: Municipality FE + Year FE | Clustered]\n")
  cat("  Temperature (β):    ", format_coef(results_basic[[i]]$coef_temp, 4),
      " (", format_coef(results_basic[[i]]$se_temp, 4), ")",
      add_stars(results_basic[[i]]$pval_temp), "\n", sep = "")
  cat("  Precipitation (η):  ", format_coef(results_basic[[i]]$coef_precip, 4),
      " (", format_coef(results_basic[[i]]$se_precip, 4), ")",
      add_stars(results_basic[[i]]$pval_precip), "\n", sep = "")
  cat("  HAC Temperature:    ", format_coef(extract_or_na(results_basic_hac, i, "coef_temp"), 4),
      " (", format_coef(extract_or_na(results_basic_hac, i, "se_temp"), 4), ")",
      add_stars(extract_or_na(results_basic_hac, i, "pval_temp")), "\n", sep = "")
  cat("  HAC Precipitation:  ", format_coef(extract_or_na(results_basic_hac, i, "coef_precip"), 4),
      " (", format_coef(extract_or_na(results_basic_hac, i, "se_precip"), 4), ")",
      add_stars(extract_or_na(results_basic_hac, i, "pval_precip")), "\n", sep = "")
  cat("  Observations:       ", results_basic[[i]]$n_obs, "\n", sep = "")
  cat("  R²:                 ", format_coef(results_basic[[i]]$r2, 3), "\n", sep = "")

  cat("\n[STATE×YEAR FE: Municipality FE + State×Year FE | Clustered]\n")
  cat("  Temperature (β):    ", format_coef(results_state_year[[i]]$coef_temp, 4),
      " (", format_coef(results_state_year[[i]]$se_temp, 4), ")",
      add_stars(results_state_year[[i]]$pval_temp), "\n", sep = "")
  cat("  Precipitation (η):  ", format_coef(results_state_year[[i]]$coef_precip, 4),
      " (", format_coef(results_state_year[[i]]$se_precip, 4), ")",
      add_stars(results_state_year[[i]]$pval_precip), "\n", sep = "")
  cat("  HAC Temperature:    ", format_coef(extract_or_na(results_state_year_hac, i, "coef_temp"), 4),
      " (", format_coef(extract_or_na(results_state_year_hac, i, "se_temp"), 4), ")",
      add_stars(extract_or_na(results_state_year_hac, i, "pval_temp")), "\n", sep = "")
  cat("  HAC Precipitation:  ", format_coef(extract_or_na(results_state_year_hac, i, "coef_precip"), 4),
      " (", format_coef(extract_or_na(results_state_year_hac, i, "se_precip"), 4), ")",
      add_stars(extract_or_na(results_state_year_hac, i, "pval_precip")), "\n", sep = "")
  cat("  Observations:       ", results_state_year[[i]]$n_obs, "\n", sep = "")
  cat("  R²:                 ", format_coef(results_state_year[[i]]$r2, 3), "\n", sep = "")

  cat("\n[MUN×YEAR FE + STATE-YEAR TRENDS: Municipality×Year FE + State-Year Trends | Clustered]\n")
  cat("  Temperature (β):    ", format_coef(results_mun_year_trend[[i]]$coef_temp, 4),
      " (", format_coef(results_mun_year_trend[[i]]$se_temp, 4), ")",
      add_stars(results_mun_year_trend[[i]]$pval_temp), "\n", sep = "")
  cat("  Precipitation (η):  ", format_coef(results_mun_year_trend[[i]]$coef_precip, 4),
      " (", format_coef(results_mun_year_trend[[i]]$se_precip, 4), ")",
      add_stars(results_mun_year_trend[[i]]$pval_precip), "\n", sep = "")
  cat("  State-Year Trend:   ", format_coef(results_mun_year_trend[[i]]$coef_trend, 4),
      " (", format_coef(results_mun_year_trend[[i]]$se_trend, 4), ")",
      add_stars(results_mun_year_trend[[i]]$pval_trend), "\n", sep = "")
  cat("  HAC Temperature:    ", format_coef(extract_or_na(results_mun_year_trend_hac, i, "coef_temp"), 4),
      " (", format_coef(extract_or_na(results_mun_year_trend_hac, i, "se_temp"), 4), ")",
      add_stars(extract_or_na(results_mun_year_trend_hac, i, "pval_temp")), "\n", sep = "")
  cat("  HAC Precipitation:  ", format_coef(extract_or_na(results_mun_year_trend_hac, i, "coef_precip"), 4),
      " (", format_coef(extract_or_na(results_mun_year_trend_hac, i, "se_precip"), 4), ")",
      add_stars(extract_or_na(results_mun_year_trend_hac, i, "pval_precip")), "\n", sep = "")
  cat("  HAC State Trend:    ", format_coef(extract_or_na(results_mun_year_trend_hac, i, "coef_trend"), 4),
      " (", format_coef(extract_or_na(results_mun_year_trend_hac, i, "se_trend"), 4), ")",
      add_stars(extract_or_na(results_mun_year_trend_hac, i, "pval_trend")), "\n", sep = "")
  cat("  Observations:       ", results_mun_year_trend[[i]]$n_obs, "\n", sep = "")
  cat("  R²:                 ", format_coef(results_mun_year_trend[[i]]$r2, 3), "\n\n", sep = "")
}

cat(rep("=", 80), "\n", sep = "")
cat("*** p<0.01, ** p<0.05, * p<0.10\n")
cat("Clustered SEs and spatial + serial HAC (Conley 1999; Newey-West 1987) are reported for all models\n")
cat(rep("=", 80), "\n\n", sep = "")

cat("Analysis complete!\n")
