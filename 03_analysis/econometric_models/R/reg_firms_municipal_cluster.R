# ============================================================================
# SHORT-RUN PANEL ESTIMATES - FIRM OUTCOMES (2000-2020)
# Regressões com clustering no município e efeitos fixos estado x ano
# SEM Conley spatial standard errors
# ============================================================================

# Load required packages
library(haven)        # For reading Stata files
library(dplyr)        # For data manipulation
library(fixest)       # For high-dimensional fixed effects
library(sandwich)     # For robust standard errors
library(lmtest)       # For coefficient testing
library(data.table)   # For efficient data manipulation

# ============================================================================
# STEP ONE: LOAD AND PREPARE DATA
# ============================================================================

cat("Loading firm data...\n")

# Load data
data <- read_dta("./output/final_base/weather_quadro_societario.dta")

# Filter for years 2000-2020
data <- data %>%
  filter(year >= 2000 & year <= 2020)

cat("Data loaded. Observations:", nrow(data), "\n")

# ============================================================================
# STEP TWO: PREPARE VARIABLES
# ============================================================================

cat("Preparing variables...\n")

# Ensure prop_verde exists
if(!"prop_verde" %in% names(data) && "firmas_ativas" %in% names(data) && "firmas_verde" %in% names(data)) {
  data <- data %>%
    mutate(prop_verde = ifelse(firmas_ativas > 0, firmas_verde / firmas_ativas, 0))
}

# Create state-year interaction variable for fixed effects
data <- data %>%
  mutate(state_year = interaction(code_state, year, drop = TRUE))

cat("Variables prepared.\n")
cat("States:", length(unique(data$code_state)), "\n")
cat("Years:", length(unique(data$year)), "\n")
cat("State-year combinations:", length(unique(data$state_year)), "\n\n")

# ============================================================================
# STEP THREE: RUN REGRESSIONS
# ============================================================================

cat("Running regressions...\n\n")

# Define dependent variables
dep_vars <- c("firmas_ativas", "firmas_verde", "prop_verde")
dep_labels <- c("Total Firms", "Green Firms", "Prop. Green Firms")

# Initialize results storage
results_basic <- list()
results_state_year <- list()

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

  cat("  Temperature coef:   ", sprintf("%.4f", results_state_year[[i]]$coef_temp), "\n")
  cat("  Temperature SE:     ", sprintf("%.4f", results_state_year[[i]]$se_temp), "\n")
  cat("  Temperature p-value:", sprintf("%.4f", results_state_year[[i]]$pval_temp), "\n")
  cat("  Precipitation coef: ", sprintf("%.4f", results_state_year[[i]]$coef_precip), "\n")
  cat("  Precipitation SE:   ", sprintf("%.4f", results_state_year[[i]]$se_precip), "\n")
  cat("  Precipitation p-val:", sprintf("%.4f", results_state_year[[i]]$pval_precip), "\n")
  cat("  Observations:       ", results_state_year[[i]]$n_obs, "\n")
  cat("  R²:                 ", sprintf("%.3f", results_state_year[[i]]$r2), "\n\n")
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

# -------------------------------------------------------------------------
# TABLE 1: Comparison Table (Both Specifications)
# -------------------------------------------------------------------------

latex_comparison <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{threeparttable}",
  "\\caption{Weather Shocks and Firms: Municipal Clustering}",
  "\\label{tab:firms_comparison}",
  "\\begin{tabular}{lcccccc}",
  "\\toprule",
  "\\multicolumn{7}{c}{\\textbf{Weather Shocks and Active Firms (2000-2020)}}\\\\",
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
  "\\item \\textit{Note:} Dependent variables are Total Active Firms (columns 1,4), Green Firms (columns 2,5), and Proportion of Green Firms (columns 3,6). Temperature ($T_{mt}$) and precipitation ($P_{mt}$) are measured as standard deviations from historical means. Columns (1)-(3) include municipality and year fixed effects. Columns (4)-(6) include municipality and state×year fixed effects. All standard errors are clustered at the municipality level and shown in parentheses.",
  "\\item \\textsuperscript{*} p < 0.10, \\textsuperscript{**} p < 0.05, \\textsuperscript{***} p < 0.01",
  "\\end{tablenotes}",
  "\\end{threeparttable}%",
  "}",
  "\\end{table}"
)

# Write table
output_file <- "table_firms_municipal_cluster_comparison.tex"
writeLines(latex_comparison, output_file)
cat("LaTeX table saved to:", output_file, "\n")

# ============================================================================
# STEP FIVE: PRINT RESULTS SUMMARY
# ============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("RESULTS SUMMARY - FIRMS\n")
cat(rep("=", 80), "\n\n", sep = "")

for(i in 1:3) {
  cat(rep("-", 80), "\n", sep = "")
  cat(dep_labels[i], "\n")
  cat(rep("-", 80), "\n", sep = "")

  cat("\n[BASELINE: Municipality FE + Year FE]\n")
  cat("  Temperature (β):    ", format_coef(results_basic[[i]]$coef_temp, 4),
      " (", format_coef(results_basic[[i]]$se_temp, 4), ")",
      add_stars(results_basic[[i]]$pval_temp), "\n", sep = "")
  cat("  Precipitation (η):  ", format_coef(results_basic[[i]]$coef_precip, 4),
      " (", format_coef(results_basic[[i]]$se_precip, 4), ")",
      add_stars(results_basic[[i]]$pval_precip), "\n", sep = "")
  cat("  Observations:       ", results_basic[[i]]$n_obs, "\n", sep = "")
  cat("  R²:                 ", format_coef(results_basic[[i]]$r2, 3), "\n", sep = "")

  cat("\n[STATE×YEAR FE: Municipality FE + State×Year FE]\n")
  cat("  Temperature (β):    ", format_coef(results_state_year[[i]]$coef_temp, 4),
      " (", format_coef(results_state_year[[i]]$se_temp, 4), ")",
      add_stars(results_state_year[[i]]$pval_temp), "\n", sep = "")
  cat("  Precipitation (η):  ", format_coef(results_state_year[[i]]$coef_precip, 4),
      " (", format_coef(results_state_year[[i]]$se_precip, 4), ")",
      add_stars(results_state_year[[i]]$pval_precip), "\n", sep = "")
  cat("  Observations:       ", results_state_year[[i]]$n_obs, "\n", sep = "")
  cat("  R²:                 ", format_coef(results_state_year[[i]]$r2, 3), "\n\n", sep = "")
}

cat(rep("=", 80), "\n", sep = "")
cat("*** p<0.01, ** p<0.05, * p<0.10\n")
cat("Standard errors clustered at municipality level (no Conley spatial correction)\n")
cat(rep("=", 80), "\n\n", sep = "")

cat("Analysis complete!\n")
