# ============================================================================
# LAGGED OUTCOMES ANALYSIS - EMPLOYMENT (2000-2020)
# Regressoes com outcomes defasados: Y_{t+k} = beta * Shock_t + alpha_i + gamma_t + e
# Replicacao completa da estrutura de reg_employment_municipal_cluster.R
# ============================================================================

# Load required packages
library(haven)        # For reading Stata files
library(dplyr)        # For data manipulation
library(fixest)       # For high-dimensional fixed effects
library(sandwich)     # For robust standard errors
library(lmtest)       # For coefficient testing
library(data.table)   # For efficient data manipulation

# ============================================================================
# CONFIGURATION - LAG PERIODS TO TEST
# ============================================================================

# Lags to test (k years into the future)
LAGS_TO_TEST <- c(1, 5, 10, 20)

cat("=" , rep("=", 79), "\n", sep = "")
cat("LAGGED OUTCOMES ANALYSIS - EMPLOYMENT\n")
cat("Testing effect of current shocks on future outcomes\n")
cat("Lags tested:", paste(LAGS_TO_TEST, collapse = ", "), "years\n")
cat(rep("=", 80), "\n\n", sep = "")

# ============================================================================
# STEP ONE: LOAD AND PREPARE DATA
# ============================================================================

cat("Loading data...\n")

# Load data
data <- read_dta("./output/final_base/weather_rais.dta")

# Use full panel to allow for lagged outcomes
# Data should span enough years to create leads
cat("Original data years:", min(data$year, na.rm = TRUE), "-", max(data$year, na.rm = TRUE), "\n")
cat("Data loaded. Observations:", nrow(data), "\n")

# ============================================================================
# STEP TWO: PREPARE VARIABLES
# ============================================================================

cat("Preparing variables...\n")

# Rename variables to match expected names
if("total_vinc" %in% names(data)) {
  data <- data %>%
    rename(total_jobs = total_vinc,
           green_jobs = total_vinc_verde)
}

# Ensure prop_verde exists
if(!"prop_verde" %in% names(data) && "total_jobs" %in% names(data) && "green_jobs" %in% names(data)) {
  data <- data %>%
    mutate(prop_verde = ifelse(total_jobs > 0, green_jobs / total_jobs, 0))
}

# Create state-year interaction variable for fixed effects
data <- data %>%
  mutate(
    state_year = interaction(code_state, year, drop = TRUE),
    mun_year = interaction(mun_code, year, drop = TRUE),
    year_state_trend = year * code_state
  )

cat("Variables prepared.\n")
cat("Municipalities:", length(unique(data$mun_code)), "\n")
cat("States:", length(unique(data$code_state)), "\n")
cat("Years:", length(unique(data$year)), "\n\n")

# ============================================================================
# STEP THREE: CREATE LAGGED OUTCOME VARIABLES (LEADS)
# ============================================================================

cat("Creating lagged outcome variables (leads)...\n")

# Convert to data.table for efficient operations
data <- as.data.table(data)

# Sort by municipality and year
setkey(data, mun_code, year)

# Define dependent variables
dep_vars <- c("total_jobs", "green_jobs", "prop_verde")
dep_labels <- c("Total Jobs", "Green Jobs", "Prop. Green Jobs")

# Create lead variables for each dependent variable and lag
# Lead k means Y_{t+k} - the outcome k years in the future
for(dv in dep_vars) {
  for(k in LAGS_TO_TEST) {
    lead_var_name <- paste0(dv, "_lead", k)
    # Create lead: Y_{t+k} aligned with shocks at time t
    data[, (lead_var_name) := shift(get(dv), n = k, type = "lead"), by = mun_code]
    cat("  Created", lead_var_name, "- observations:", sum(!is.na(data[[lead_var_name]])), "\n")
  }
}

cat("\nLead variables created.\n\n")

# Convert back to data.frame
data <- as.data.frame(data)

# ============================================================================
# STEP FOUR: RUN REGRESSIONS FOR EACH LAG
# ============================================================================

cat("Running regressions for lagged outcomes...\n\n")

# Initialize results storage
# Structure: results[[lag]][[dep_var]][[model_type]]
results_all <- list()

for(k in LAGS_TO_TEST) {
  cat(rep("=", 80), "\n", sep = "")
  cat("LAG k =", k, "years (Effect of Shock_t on Y_{t+", k, "})\n", sep = "")
  cat(rep("=", 80), "\n\n", sep = "")

  results_lag <- list()

  # Filter data for this lag (need observations where lead exists)
  # Restrict to years where we can observe the outcome k years later
  max_shock_year <- max(data$year, na.rm = TRUE) - k
  data_lag <- data %>%
    filter(year >= 2000, year <= max_shock_year)

  cat("Analysis period for shocks: 2000-", max_shock_year, "\n", sep = "")
  cat("Observations for this lag:", nrow(data_lag), "\n\n")

  for(i in 1:length(dep_vars)) {

    dv <- dep_vars[i]
    dv_lead <- paste0(dv, "_lead", k)

    cat("------------------------------------------------------------\n")
    cat("Outcome:", dep_labels[i], "(", dv_lead, ")\n")
    cat("------------------------------------------------------------\n")

    results_var <- list()

    # -------------------------------------------------------------------------
    # Model 1: Municipal + Year Fixed Effects (Baseline)
    # -------------------------------------------------------------------------

    cat("\n[1] Baseline: Municipality FE + Year FE + Clustering Municipality\n")

    # Formula: Y_{t+k} = beta*T_t + eta*P_t + alpha_m + gamma_t + e
    formula_basic <- as.formula(paste0(dv_lead, " ~ cont_shock_temp + cont_shock_precip | mun_code + year"))

    # Run fixed effects regression with municipal clustering
    model_basic <- tryCatch({
      feols(formula_basic, data = data_lag, cluster = ~mun_code)
    }, error = function(e) {
      cat("  Error in baseline model:", e$message, "\n")
      NULL
    })

    if(!is.null(model_basic)) {
      results_var$basic <- list(
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

      cat("  Temperature coef:   ", sprintf("%.4f", results_var$basic$coef_temp), "\n")
      cat("  Temperature SE:     ", sprintf("%.4f", results_var$basic$se_temp), "\n")
      cat("  Temperature p-value:", sprintf("%.4f", results_var$basic$pval_temp), "\n")
      cat("  Precipitation coef: ", sprintf("%.4f", results_var$basic$coef_precip), "\n")
      cat("  Observations:       ", results_var$basic$n_obs, "\n")
      cat("  R^2:                ", sprintf("%.3f", results_var$basic$r2), "\n")
    }

    # -------------------------------------------------------------------------
    # Model 2: Municipal + State-Year Fixed Effects
    # -------------------------------------------------------------------------

    cat("\n[2] State-Year FE: Municipality FE + State x Year FE + Clustering Municipality\n")

    formula_state_year <- as.formula(paste0(dv_lead, " ~ cont_shock_temp + cont_shock_precip | mun_code + state_year"))

    model_state_year <- tryCatch({
      feols(formula_state_year, data = data_lag, cluster = ~mun_code)
    }, error = function(e) {
      cat("  Error in state-year model:", e$message, "\n")
      NULL
    })

    if(!is.null(model_state_year)) {
      results_var$state_year <- list(
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

      cat("  Temperature coef:   ", sprintf("%.4f", results_var$state_year$coef_temp), "\n")
      cat("  Temperature SE:     ", sprintf("%.4f", results_var$state_year$se_temp), "\n")
      cat("  Temperature p-value:", sprintf("%.4f", results_var$state_year$pval_temp), "\n")
      cat("  Precipitation coef: ", sprintf("%.4f", results_var$state_year$coef_precip), "\n")
      cat("  Observations:       ", results_var$state_year$n_obs, "\n")
      cat("  R^2:                ", sprintf("%.3f", results_var$state_year$r2), "\n")
    }

    # -------------------------------------------------------------------------
    # Model 3: Municipality + Year FE + State-Year Trends
    # -------------------------------------------------------------------------

    cat("\n[3] State Trends: Municipality FE + Year FE + State x Year Trends + Clustering Municipality\n")

    formula_mun_year_trend <- as.formula(paste0(dv_lead, " ~ cont_shock_temp + cont_shock_precip + year_state_trend | mun_code + year"))

    model_mun_year_trend <- tryCatch({
      feols(formula_mun_year_trend, data = data_lag, cluster = ~mun_code)
    }, error = function(e) {
      cat("  Error in state trend model:", e$message, "\n")
      NULL
    })

    if(!is.null(model_mun_year_trend)) {
      results_var$mun_year_trend <- list(
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

      cat("  Temperature coef:   ", sprintf("%.4f", results_var$mun_year_trend$coef_temp), "\n")
      cat("  Temperature SE:     ", sprintf("%.4f", results_var$mun_year_trend$se_temp), "\n")
      cat("  Temperature p-value:", sprintf("%.4f", results_var$mun_year_trend$pval_temp), "\n")
      cat("  Precipitation coef: ", sprintf("%.4f", results_var$mun_year_trend$coef_precip), "\n")
      cat("  Observations:       ", results_var$mun_year_trend$n_obs, "\n")
      cat("  R^2:                ", sprintf("%.3f", results_var$mun_year_trend$r2), "\n")
    }

    cat("\n")
    results_lag[[dv]] <- results_var
  }

  results_all[[as.character(k)]] <- results_lag
}

# ============================================================================
# STEP FIVE: GENERATE LATEX TABLES
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("Generating LaTeX tables...\n")
cat(rep("=", 70), "\n\n", sep = "")

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
  if(is.na(x)) return("NA")
  sprintf(paste0("%.", digits, "f"), x)
}

# -------------------------------------------------------------------------
# TABLE 1: Lagged Outcomes - Baseline Models (All Lags Side by Side)
# -------------------------------------------------------------------------

for(model_type in c("basic", "state_year", "mun_year_trend")) {

  model_label <- switch(model_type,
                        "basic" = "Baseline (Mun + Year FE)",
                        "state_year" = "State x Year FE",
                        "mun_year_trend" = "State-Year Trends")

  model_suffix <- switch(model_type,
                         "basic" = "baseline",
                         "state_year" = "state_year",
                         "mun_year_trend" = "state_trend")

  for(i in 1:length(dep_vars)) {

    dv <- dep_vars[i]
    dv_label <- dep_labels[i]

    n_lags <- length(LAGS_TO_TEST)
    col_spec <- paste0("l", paste(rep("c", n_lags), collapse = ""))

    latex_table <- c(
      "\\begin{table}[H]",
      "\\centering",
      "\\resizebox{0.9\\textwidth}{!}{%",
      "\\begin{threeparttable}",
      paste0("\\caption{Lagged Outcomes: ", dv_label, " - ", model_label, "}"),
      paste0("\\label{tab:employment_lagged_", dv, "_", model_suffix, "}"),
      paste0("\\begin{tabular}{", col_spec, "}"),
      "\\toprule",
      paste0("\\multicolumn{", n_lags + 1, "}{c}{\\textbf{Effect of Current Shocks on Future ", dv_label, "}}\\\\"),
      "\\midrule"
    )

    # Header row with lag labels
    header_row <- "Outcome at $t+k$:"
    for(k in LAGS_TO_TEST) {
      header_row <- paste0(header_row, " & $Y_{t+", k, "}$")
    }
    header_row <- paste0(header_row, " \\\\")
    latex_table <- c(latex_table, header_row, "\\midrule")

    # Temperature coefficient row
    temp_row <- "$T_{mt}$ (Temperature)"
    temp_se_row <- ""
    for(k in LAGS_TO_TEST) {
      res <- results_all[[as.character(k)]][[dv]][[model_type]]
      if(!is.null(res)) {
        coef_val <- res$coef_temp
        se_val <- res$se_temp
        pval <- res$pval_temp
        stars <- add_stars(pval)
        digits <- ifelse(abs(coef_val) < 0.01, 4, 3)
        temp_row <- paste0(temp_row, " & ", format_coef(coef_val, digits), stars)
        temp_se_row <- paste0(temp_se_row, " & (", format_coef(se_val, digits), ")")
      } else {
        temp_row <- paste0(temp_row, " & --")
        temp_se_row <- paste0(temp_se_row, " & --")
      }
    }
    temp_row <- paste0(temp_row, " \\\\")
    temp_se_row <- paste0(temp_se_row, " \\\\[0.5em]")
    latex_table <- c(latex_table, temp_row, temp_se_row)

    # Precipitation coefficient row
    precip_row <- "$P_{mt}$ (Precipitation)"
    precip_se_row <- ""
    for(k in LAGS_TO_TEST) {
      res <- results_all[[as.character(k)]][[dv]][[model_type]]
      if(!is.null(res)) {
        coef_val <- res$coef_precip
        se_val <- res$se_precip
        pval <- res$pval_precip
        stars <- add_stars(pval)
        digits <- ifelse(abs(coef_val) < 0.01, 4, 3)
        precip_row <- paste0(precip_row, " & ", format_coef(coef_val, digits), stars)
        precip_se_row <- paste0(precip_se_row, " & (", format_coef(se_val, digits), ")")
      } else {
        precip_row <- paste0(precip_row, " & --")
        precip_se_row <- paste0(precip_se_row, " & --")
      }
    }
    precip_row <- paste0(precip_row, " \\\\")
    precip_se_row <- paste0(precip_se_row, " \\\\")
    latex_table <- c(latex_table, precip_row, precip_se_row)

    # Footer
    obs_row <- "Observations"
    r2_row <- "$R^2$"
    for(k in LAGS_TO_TEST) {
      res <- results_all[[as.character(k)]][[dv]][[model_type]]
      if(!is.null(res)) {
        obs_row <- paste0(obs_row, " & ", format(res$n_obs, big.mark = ","))
        r2_row <- paste0(r2_row, " & ", format_coef(res$r2, 3))
      } else {
        obs_row <- paste0(obs_row, " & --")
        r2_row <- paste0(r2_row, " & --")
      }
    }
    obs_row <- paste0(obs_row, " \\\\")
    r2_row <- paste0(r2_row, " \\\\")

    fe_row1 <- "Municipality FE"
    fe_row2 <- switch(model_type,
                      "basic" = "Year FE",
                      "state_year" = "State $\\times$ Year FE",
                      "mun_year_trend" = "Year FE + State Trends")
    for(k in LAGS_TO_TEST) {
      fe_row1 <- paste0(fe_row1, " & Yes")
      fe_row2 <- paste0(fe_row2, " & Yes")
    }
    fe_row1 <- paste0(fe_row1, " \\\\")
    fe_row2 <- paste0(fe_row2, " \\\\")

    latex_table <- c(
      latex_table,
      "\\midrule",
      obs_row,
      r2_row,
      fe_row1,
      fe_row2,
      "\\bottomrule",
      "\\end{tabular}",
      "\\begin{tablenotes}",
      "\\footnotesize",
      paste0("\\item \\textit{Note:} This table shows the effect of climate shocks at time $t$ on ", dv_label, " at time $t+k$. ",
             "Temperature and precipitation are measured as standard deviations from historical means. ",
             "Standard errors are clustered at the municipality level and shown in parentheses."),
      "\\item \\textsuperscript{*} p < 0.10, \\textsuperscript{**} p < 0.05, \\textsuperscript{***} p < 0.01",
      "\\end{tablenotes}",
      "\\end{threeparttable}%",
      "}",
      "\\end{table}"
    )

    output_file <- paste0("table_employment_lagged_outcomes_", dv, "_", model_suffix, ".tex")
    writeLines(latex_table, output_file)
    cat("Table saved:", output_file, "\n")
  }
}

# -------------------------------------------------------------------------
# TABLE 2: Comparison Table - All Outcomes, All Lags (Baseline Model)
# -------------------------------------------------------------------------

latex_comparison <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{threeparttable}",
  "\\caption{Lagged Employment Outcomes: Effect of Current Shocks on Future Outcomes}",
  "\\label{tab:employment_lagged_comparison}",
  paste0("\\begin{tabular}{l", paste(rep("c", length(LAGS_TO_TEST) * 3), collapse = ""), "}"),
  "\\toprule"
)

# Header row 1: Outcome types
header1 <- " & \\multicolumn{"
for(i in 1:length(dep_labels)) {
  header1 <- paste0(header1, length(LAGS_TO_TEST), "}{c}{\\textbf{", dep_labels[i], "}}")
  if(i < length(dep_labels)) header1 <- paste0(header1, " & \\multicolumn{")
}
header1 <- paste0(header1, " \\\\")

# Header row 2: Lag values
header2 <- "Lag (years):"
for(i in 1:length(dep_vars)) {
  for(k in LAGS_TO_TEST) {
    header2 <- paste0(header2, " & $k=", k, "$")
  }
}
header2 <- paste0(header2, " \\\\")

latex_comparison <- c(latex_comparison, header1)
# Add cmidrule separators
cmidrule <- ""
col_start <- 2
for(i in 1:length(dep_vars)) {
  col_end <- col_start + length(LAGS_TO_TEST) - 1
  cmidrule <- paste0(cmidrule, "\\cmidrule(lr){", col_start, "-", col_end, "} ")
  col_start <- col_end + 1
}
latex_comparison <- c(latex_comparison, cmidrule, header2, "\\midrule")

# Temperature row
temp_row <- "$T_{mt}$ (Temp.)"
temp_se_row <- ""
for(dv in dep_vars) {
  for(k in LAGS_TO_TEST) {
    res <- results_all[[as.character(k)]][[dv]]$basic
    if(!is.null(res)) {
      coef_val <- res$coef_temp
      se_val <- res$se_temp
      pval <- res$pval_temp
      stars <- add_stars(pval)
      digits <- ifelse(abs(coef_val) < 0.01, 4, 3)
      temp_row <- paste0(temp_row, " & ", format_coef(coef_val, digits), stars)
      temp_se_row <- paste0(temp_se_row, " & (", format_coef(se_val, digits), ")")
    } else {
      temp_row <- paste0(temp_row, " & --")
      temp_se_row <- paste0(temp_se_row, " & --")
    }
  }
}
temp_row <- paste0(temp_row, " \\\\")
temp_se_row <- paste0(temp_se_row, " \\\\[0.5em]")
latex_comparison <- c(latex_comparison, temp_row, temp_se_row)

# Precipitation row
precip_row <- "$P_{mt}$ (Precip.)"
precip_se_row <- ""
for(dv in dep_vars) {
  for(k in LAGS_TO_TEST) {
    res <- results_all[[as.character(k)]][[dv]]$basic
    if(!is.null(res)) {
      coef_val <- res$coef_precip
      se_val <- res$se_precip
      pval <- res$pval_precip
      stars <- add_stars(pval)
      digits <- ifelse(abs(coef_val) < 0.01, 4, 3)
      precip_row <- paste0(precip_row, " & ", format_coef(coef_val, digits), stars)
      precip_se_row <- paste0(precip_se_row, " & (", format_coef(se_val, digits), ")")
    } else {
      precip_row <- paste0(precip_row, " & --")
      precip_se_row <- paste0(precip_se_row, " & --")
    }
  }
}
precip_row <- paste0(precip_row, " \\\\")
precip_se_row <- paste0(precip_se_row, " \\\\")
latex_comparison <- c(latex_comparison, precip_row, precip_se_row)

# Footer
obs_row <- "Observations"
r2_row <- "$R^2$"
for(dv in dep_vars) {
  for(k in LAGS_TO_TEST) {
    res <- results_all[[as.character(k)]][[dv]]$basic
    if(!is.null(res)) {
      obs_row <- paste0(obs_row, " & ", format(res$n_obs, big.mark = ","))
      r2_row <- paste0(r2_row, " & ", format_coef(res$r2, 3))
    } else {
      obs_row <- paste0(obs_row, " & --")
      r2_row <- paste0(r2_row, " & --")
    }
  }
}
obs_row <- paste0(obs_row, " \\\\")
r2_row <- paste0(r2_row, " \\\\")

latex_comparison <- c(
  latex_comparison,
  "\\midrule",
  obs_row,
  r2_row,
  "Municipality FE & \\multicolumn{" , length(LAGS_TO_TEST) * 3, "}{c}{Yes} \\\\",
  "Year FE & \\multicolumn{" , length(LAGS_TO_TEST) * 3, "}{c}{Yes} \\\\",
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}",
  "\\footnotesize",
  "\\item \\textit{Note:} This table shows the effect of climate shocks at time $t$ on employment outcomes at time $t+k$. ",
  "Each column represents a different lag horizon ($k$ years). Temperature and precipitation are measured as standard deviations from historical means. ",
  "All regressions include municipality and year fixed effects. Standard errors are clustered at the municipality level and shown in parentheses.",
  "\\item \\textsuperscript{*} p < 0.10, \\textsuperscript{**} p < 0.05, \\textsuperscript{***} p < 0.01",
  "\\end{tablenotes}",
  "\\end{threeparttable}%",
  "}",
  "\\end{table}"
)

output_file_comparison <- "table_employment_lagged_outcomes_comparison.tex"
writeLines(latex_comparison, output_file_comparison)
cat("Comparison table saved:", output_file_comparison, "\n")

# ============================================================================
# STEP SIX: PRINT RESULTS SUMMARY
# ============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("RESULTS SUMMARY - LAGGED EMPLOYMENT OUTCOMES\n")
cat(rep("=", 80), "\n\n", sep = "")

for(k in LAGS_TO_TEST) {
  cat(rep("-", 80), "\n", sep = "")
  cat("LAG k =", k, "years (Shock_t -> Y_{t+", k, "})\n", sep = "")
  cat(rep("-", 80), "\n\n", sep = "")

  for(i in 1:length(dep_vars)) {
    dv <- dep_vars[i]
    cat(dep_labels[i], ":\n")

    res_basic <- results_all[[as.character(k)]][[dv]]$basic
    res_sy <- results_all[[as.character(k)]][[dv]]$state_year

    if(!is.null(res_basic)) {
      cat("  [Baseline] Temp: ", format_coef(res_basic$coef_temp, 4),
          " (", format_coef(res_basic$se_temp, 4), ")",
          add_stars(res_basic$pval_temp),
          " | Precip: ", format_coef(res_basic$coef_precip, 4),
          " (", format_coef(res_basic$se_precip, 4), ")",
          add_stars(res_basic$pval_precip),
          " | N=", res_basic$n_obs, "\n", sep = "")
    }

    if(!is.null(res_sy)) {
      cat("  [State-Year FE] Temp: ", format_coef(res_sy$coef_temp, 4),
          " (", format_coef(res_sy$se_temp, 4), ")",
          add_stars(res_sy$pval_temp),
          " | Precip: ", format_coef(res_sy$coef_precip, 4),
          " (", format_coef(res_sy$se_precip, 4), ")",
          add_stars(res_sy$pval_precip),
          " | N=", res_sy$n_obs, "\n", sep = "")
    }
    cat("\n")
  }
}

cat(rep("=", 80), "\n", sep = "")
cat("*** p<0.01, ** p<0.05, * p<0.10\n")
cat("Standard errors clustered at municipality level\n")
cat(rep("=", 80), "\n\n", sep = "")

cat("Analysis complete!\n")
cat("Tables generated in current directory.\n")
