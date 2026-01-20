# ============================================================================
# POPULATION-WEIGHTED SHORT-RUN PANEL ESTIMATES (2000-2020)
# Employment, Firms, and Patents (population-weighted versions of existing models)
# ============================================================================

# Load required packages
library(haven)        # For reading Stata files
library(dplyr)        # For data manipulation
library(fixest)       # For high-dimensional fixed effects

# ============================================================================
# HELPERS
# ============================================================================

weight_var <- "population"

validate_weights <- function(df, dataset_label) {
  if (!weight_var %in% names(df)) {
    stop(sprintf("Population variable '%s' not found for %s.", weight_var, dataset_label))
  }

  df <- df %>%
    mutate(
      !!weight_var := as.numeric(.data[[weight_var]])
    )

  invalid <- is.na(df[[weight_var]]) | df[[weight_var]] <= 0
  if (any(invalid)) {
    warning(sprintf(
      "Removing %d rows with missing/non-positive population weights for %s.",
      sum(invalid),
      dataset_label
    ))
    df <- df[!invalid, , drop = FALSE]
  }

  df
}

run_weighted_models <- function(df, dep_vars, dep_labels, dataset_label, include_state_trend = TRUE) {
  cat("\n============================================\n")
  cat("Population-weighted regressions:", dataset_label, "\n")
  cat("============================================\n\n")

  results <- list()

  for (i in seq_along(dep_vars)) {
    dv <- dep_vars[i]
    cat("Model", i, "-", dep_labels[i], "\n")

    formula_basic <- as.formula(paste0(dv, " ~ cont_shock_temp + cont_shock_precip | mun_code + year"))
    model_basic <- feols(
      formula_basic,
      data = df,
      weights = as.formula(paste0("~", weight_var)),
      cluster = ~mun_code
    )

    formula_state_year <- as.formula(paste0(dv, " ~ cont_shock_temp + cont_shock_precip | mun_code + state_year"))
    model_state_year <- feols(
      formula_state_year,
      data = df,
      weights = as.formula(paste0("~", weight_var)),
      cluster = ~mun_code
    )

    model_state_trend <- NULL
    if (include_state_trend) {
      formula_state_trend <- as.formula(paste0(
        dv,
        " ~ cont_shock_temp + cont_shock_precip + year_state_trend | mun_code + year"
      ))
      model_state_trend <- feols(
        formula_state_trend,
        data = df,
        weights = as.formula(paste0("~", weight_var)),
        cluster = ~mun_code
      )
    }

    results[[dv]] <- list(
      basic = model_basic,
      state_year = model_state_year,
      state_trend = model_state_trend
    )

    cat("  Baseline temp coef:", sprintf("%.4f", coef(model_basic)["cont_shock_temp"]), "\n")
    cat("  Baseline precip coef:", sprintf("%.4f", coef(model_basic)["cont_shock_precip"]), "\n")
    cat("  State-year temp coef:", sprintf("%.4f", coef(model_state_year)["cont_shock_temp"]), "\n")
    cat("  State-year precip coef:", sprintf("%.4f", coef(model_state_year)["cont_shock_precip"]), "\n")
    if (!is.null(model_state_trend)) {
      cat("  State-trend temp coef:", sprintf("%.4f", coef(model_state_trend)["cont_shock_temp"]), "\n")
      cat("  State-trend precip coef:", sprintf("%.4f", coef(model_state_trend)["cont_shock_precip"]), "\n")
    }
    cat("\n")
  }

  invisible(results)
}

# ============================================================================
# EMPLOYMENT MODELS (weather_rais.dta)
# ============================================================================

cat("Loading employment data...\n")

employment_data <- read_dta("./output/final_base/weather_rais.dta") %>%
  filter(year >= 2000 & year <= 2020)

if ("total_vinc" %in% names(employment_data)) {
  employment_data <- employment_data %>%
    rename(
      total_jobs = total_vinc,
      green_jobs = total_vinc_verde
    )
}

if (!"prop_verde" %in% names(employment_data) &&
    all(c("total_jobs", "green_jobs") %in% names(employment_data))) {
  employment_data <- employment_data %>%
    mutate(prop_verde = green_jobs / total_jobs)
}

employment_data <- employment_data %>%
  mutate(
    state_year = interaction(code_state, year, drop = TRUE),
    year_state_trend = year * code_state
  )

employment_data <- validate_weights(employment_data, "Employment")

run_weighted_models(
  df = employment_data,
  dep_vars = c("total_jobs", "green_jobs", "prop_verde"),
  dep_labels = c("Total Jobs", "Green Jobs", "Prop. Green Jobs"),
  dataset_label = "Employment"
)

# ============================================================================
# FIRM MODELS (weather_quadro_societario.dta)
# ============================================================================

cat("Loading firm data...\n")

firm_data <- read_dta("./output/final_base/weather_quadro_societario.dta") %>%
  filter(year >= 2000 & year <= 2020)

if (!"prop_verde" %in% names(firm_data) &&
    all(c("firmas_ativas", "firmas_verde") %in% names(firm_data))) {
  firm_data <- firm_data %>%
    mutate(prop_verde = ifelse(firmas_ativas > 0, firmas_verde / firmas_ativas, 0))
}

firm_data <- firm_data %>%
  mutate(
    state_year = interaction(code_state, year, drop = TRUE),
    year_state_trend = year * code_state
  )

firm_data <- validate_weights(firm_data, "Firms")

run_weighted_models(
  df = firm_data,
  dep_vars = c("firmas_ativas", "firmas_verde", "prop_verde"),
  dep_labels = c("Total Firms", "Green Firms", "Prop. Green Firms"),
  dataset_label = "Firms"
)

# ============================================================================
# PATENT MODELS (weather_patent.dta)
# ============================================================================

cat("Loading patent data...\n")

patent_data <- read_dta("./output/final_base/weather_patent.dta") %>%
  filter(year >= 2000 & year <= 2020)

if (!"prop_verde" %in% names(patent_data) &&
    all(c("qtd_pat", "qtd_pat_verde") %in% names(patent_data))) {
  patent_data <- patent_data %>%
    mutate(prop_verde = ifelse(qtd_pat > 0, qtd_pat_verde / qtd_pat, 0))
}

patent_data <- patent_data %>%
  mutate(
    state_year = interaction(code_state, year, drop = TRUE),
    year_state_trend = year * code_state
  )

patent_data <- validate_weights(patent_data, "Patents")

run_weighted_models(
  df = patent_data,
  dep_vars = c("qtd_pat", "qtd_pat_verde", "prop_verde"),
  dep_labels = c("Total Patents", "Green Patents", "Prop. Green Patents"),
  dataset_label = "Patents"
)
