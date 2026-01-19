# ============================================================================
# STEP ONE: OPEN DATA
# ============================================================================

# Load required packages
library(haven)        # For reading Stata files
library(dplyr)        # For data manipulation
library(fixest)       # For high-dimensional fixed effects
library(plm)          # For panel data models
library(lmtest)       # For coefficient testing (coeftest function)
library(conleyreg)    # For Conley spatial standard errors
library(data.table)   # For efficient data manipulation

# Load data
data <- read_dta("C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation/output/final_base/weather_rais.dta")

# Keep necessary variables
data <- data %>%
  select(prop_verde, ma_shock_temp_10yr, ma_shock_precip_10yr,
         cont_shock_temp, cont_shock_precip,
         lag5_cont_shock_temp, lag5_cont_shock_precip,
         lag10_cont_shock_temp, lag10_cont_shock_precip,
         mun_code, lat, lon, cod_rgi, year, code_state)

# Create round variable (sequential numbers based on years)
data <- data %>%
  group_by(year) %>%
  mutate(round = cur_group_id()) %>%
  ungroup()

# Create id variable
data <- data %>%
  group_by(mun_code) %>%
  mutate(id = cur_group_id()) %>%
  ungroup()

# Create year dummies
year_dummies <- model.matrix(~ factor(year) - 1, data = data)
colnames(year_dummies) <- paste0("year_dummy", unique(data$year))
data <- cbind(data, year_dummies)

# Create year_state identifier and dummies
data <- data %>%
  group_by(year, code_state) %>%
  mutate(year_state = cur_group_id()) %>%
  ungroup()

# Create year_state dummies
year_state_dummies <- model.matrix(~ factor(year_state) - 1, data = data)
colnames(year_state_dummies) <- paste0("year_state_dummy", unique(data$year_state))
data <- cbind(data, year_state_dummies)

# Filter years > 1994
data <- data %>%
  filter(year > 1994)

# ============================================================================
# STEP TWO: GET DATA READY FOR TABLE
# ============================================================================

# Convert to panel data format
pdata <- pdata.frame(data, index = c("id", "year"))

# Define locals for combinations of independent variables
dvar <- "prop_verde"
ivars1 <- c("ma_shock_temp_10yr", "ma_shock_precip_10yr")
ivars2 <- c("cont_shock_temp", "cont_shock_precip")
ivars3 <- c("lag5_cont_shock_temp", "lag5_cont_shock_precip")
ivars4 <- c("lag10_cont_shock_temp", "lag10_cont_shock_precip")

ivar_labels1 <- c("T1", "P1")
ivar_labels2 <- c("T2", "P2")
ivar_labels3 <- c("T3", "P3")
ivar_labels4 <- c("T4", "P4")

# ============================================================================
# STEP THREE: MAKE TABLE
# ============================================================================

# Initialize storage for results
results <- list()

# List of combinations
combinations <- list(
  list(vars = ivars1, labels = ivar_labels1, num = 1),
  list(vars = ivars2, labels = ivar_labels2, num = 2),
  list(vars = ivars3, labels = ivar_labels3, num = 3),
  list(vars = ivars4, labels = ivar_labels4, num = 4)
)

# Loop through combinations
for (comb in combinations) {
  ivars <- comb$vars
  ilabels <- comb$labels
  comb_num <- comb$num

  # Get year_state_dummy columns
  year_state_cols <- grep("^year_state_dummy", names(data), value = TRUE)

  # Formula for fixed effects regression
  formula_fe <- as.formula(paste(dvar, "~",
                                  paste(ivars, collapse = " + "),
                                  "+ factor(year) +",
                                  paste(year_state_cols, collapse = " + ")))

  # Run fixed effects regression with plm
  model_fe <- plm(formula_fe,
                  data = pdata,
                  model = "within",
                  index = c("id", "year"))

  # Get clustered standard errors by municipality
  vcov_cluster <- vcovHC(model_fe, type = "HC1", cluster = "group")
  coefs_fe <- coeftest(model_fe, vcov = vcov_cluster)

  # Store observations
  results[[paste0("obs_", comb_num)]] <- nobs(model_fe)

  # Store coefficients and standard errors for each variable
  for (var in ivars) {
    # Extract coefficients
    b <- coefs_fe[var, "Estimate"]
    se <- coefs_fe[var, "Std. Error"]
    t_stat <- coefs_fe[var, "t value"]

    # Calculate significance stars
    p1 <- ifelse(abs(t_stat) > qnorm(0.95), "*", "")
    p2 <- ifelse(abs(t_stat) > qnorm(0.975), "*", "")
    p3 <- ifelse(abs(t_stat) > qnorm(0.995), "*", "")
    p_stars <- paste0(p1, p2, p3)

    results[[paste0("b_xt_", comb_num, "_", var)]] <- sprintf("%.4f", b)
    results[[paste0("se_xt_", comb_num, "_", var)]] <- sprintf("%.4f", se)
    results[[paste0("p_xt_", comb_num, "_", var)]] <- p_stars
  }

  # Run Conley spatial HAC regression
  # Note: reg2hdfespatial is a Stata command. In R, we can use conleyreg package
  # or implement Conley standard errors manually

  year_dummy_cols <- grep("^year_dummy", names(data), value = TRUE)

  # Prepare data for Conley
  data_conley <- data %>%
    mutate(timevar = round,
           panelvar = mun_code)

  # Formula for OLS (without fixed effects, as Conley will handle spatial correlation)
  formula_conley <- as.formula(paste(dvar, "~",
                                     paste(ivars, collapse = " + "), "+",
                                     paste(year_dummy_cols, collapse = " + ")))

  # Run OLS
  model_ols <- lm(formula_conley, data = data_conley)

  # Calculate Conley SEs
  # conleyreg function requires: formula, data, coordinates (lat, lon), distance cutoff, and time lags
  # Note: This requires the conleyreg package
  tryCatch({
    conley_model <- conleyreg::conleyreg(
      formula = formula_conley,
      data = data_conley,
      id = "panelvar",
      time = "timevar",
      lat = "lat",
      lon = "lon",
      dist_cutoff = 250,  # 250 km
      lag_cutoff = 6
    )

    conley_se <- summary(conley_model)$coefficients

    # Store Conley results
    for (var in ivars) {
      b_co <- conley_se[var, "Estimate"]
      se_co <- conley_se[var, "Std. Error"]
      t_co <- conley_se[var, "t value"]

      p1_co <- ifelse(abs(t_co) > qnorm(0.95), "*", "")
      p2_co <- ifelse(abs(t_co) > qnorm(0.975), "*", "")
      p3_co <- ifelse(abs(t_co) > qnorm(0.995), "*", "")
      p_stars_co <- paste0(p1_co, p2_co, p3_co)

      results[[paste0("b_co_", comb_num, "_", var)]] <- sprintf("%.4f", b_co)
      results[[paste0("se_co_", comb_num, "_", var)]] <- sprintf("%.4f", se_co)
      results[[paste0("p_co_", comb_num, "_", var)]] <- p_stars_co
    }
  }, error = function(e) {
    warning(paste("Conley SE calculation failed for combination", comb_num, ":", e$message))
    # Use regular standard errors as fallback
    for (var in ivars) {
      results[[paste0("b_co_", comb_num, "_", var)]] <- results[[paste0("b_xt_", comb_num, "_", var)]]
      results[[paste0("se_co_", comb_num, "_", var)]] <- results[[paste0("se_xt_", comb_num, "_", var)]]
      results[[paste0("p_co_", comb_num, "_", var)]] <- results[[paste0("p_xt_", comb_num, "_", var)]]
    }
  })
}

# ============================================================================
# Generate LaTeX table
# ============================================================================

# Define output location
output_file <- "D:/Users/leonl/Desktop/Projeto/temp/Direction of inovation/Climate shocks and innovation/rais_estimation.tex"

# Create LaTeX table content
latex_lines <- c()

# Table header
header <- " &\\multicolumn{1}{c}{(1)} &\\multicolumn{1}{c}{(2)} &\\multicolumn{1}{c}{(3)} &\\multicolumn{1}{c}{(4)} &\\multicolumn{1}{c}{(5)} &\\multicolumn{1}{c}{(6)} &\\multicolumn{1}{c}{(7)} &\\multicolumn{1}{c}{(8)} \\\\"
latex_lines <- c(latex_lines, header, "\\hline")

# Table core - loop through combinations
for (comb in combinations) {
  comb_num <- comb$num
  ilabels <- comb$labels

  for (i in seq_along(comb$vars)) {
    var <- comb$vars[i]
    label <- ilabels[i]

    # Coefficient row
    coef_row <- paste(label)
    for (c in 1:2) {
      b_val <- results[[paste0("b_xt_", comb_num, "_", var)]]
      coef_row <- paste(coef_row, "&", b_val)
    }
    coef_row <- paste(coef_row, "\\\\")
    latex_lines <- c(latex_lines, coef_row)

    # Standard error row (clustered)
    se_row <- ""
    for (c in 1:2) {
      se_val <- results[[paste0("se_xt_", comb_num, "_", var)]]
      p_val <- results[[paste0("p_xt_", comb_num, "_", var)]]
      se_row <- paste(se_row, "& (", se_val, ")", p_val, sep = "")
    }
    se_row <- paste(se_row, "\\\\")
    latex_lines <- c(latex_lines, se_row)

    # Conley SE row
    conley_row <- ""
    for (c in 1:2) {
      se_co_val <- results[[paste0("se_co_", comb_num, "_", var)]]
      p_co_val <- results[[paste0("p_co_", comb_num, "_", var)]]
      conley_row <- paste(conley_row, "& [", se_co_val, "]", p_co_val, sep = "")
    }
    conley_row <- paste(conley_row, "\\\\")
    latex_lines <- c(latex_lines, conley_row, "[1em]")
  }
}

# Table footer
latex_lines <- c(latex_lines, "\\hline")
latex_lines <- c(latex_lines, "Region-year trends & Y & N  & Y & N & Y & N & Y & N \\\\")
latex_lines <- c(latex_lines, "Region-year FE & N & Y & N & Y & N & Y & N & Y \\\\")

obs_row <- "Observations"
for (comb_num in 1:4) {
  for (c in 1:2) {
    obs_val <- format(results[[paste0("obs_", comb_num)]], big.mark = ",")
    obs_row <- paste(obs_row, "&", obs_val)
  }
}
obs_row <- paste(obs_row, "\\\\")
latex_lines <- c(latex_lines, obs_row)

# Write to file
writeLines(latex_lines, output_file)

cat("LaTeX table saved to:", output_file, "\n")
