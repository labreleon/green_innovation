# ============================================================================
# STEP ONE: OPEN DATA
# ============================================================================

# Load required packages
library(haven)        # For reading Stata files
library(dplyr)        # For data manipulation
library(conleyreg)    # For Conley spatial standard errors
library(lmtest)       # For coefficient testing
library(data.table)   # For efficient data manipulation

# Load data
data <- read_dta("C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation/output/final_base/weather_quadro_societario.dta")

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

# Filter years > 1999
data <- data %>%
  filter(year > 1999)

# Create year-state trend
data <- data %>%
  mutate(year_state_trend = year * code_state)

# Create lagged prop_verde (lag 1)
data <- data %>%
  arrange(id, year) %>%
  group_by(id) %>%
  mutate(prop_verde_lag1 = lag(prop_verde, 1)) %>%
  ungroup()

# ============================================================================
# SHORT-TERM ANALYSIS
# ============================================================================

cat("\n=== SHORT-TERM ANALYSIS ===\n\n")

# Temperature
cat("Model 1: Temperature shock\n")
formula1 <- prop_verde ~ cont_shock_temp + year_state_trend + urban +
            log_renda + alphabetization_rate
model1 <- lm(formula1, data = data)

# Note: For Conley spatial standard errors, you would use the conleyreg package:
# See: https://cran.r-project.org/package=conleyreg
# Example:
# conley_model <- conleyreg::conleyreg(
#   formula = formula1,
#   data = data,
#   id = "mun_code",
#   time = "year",
#   lat = "lat",
#   lon = "lon",
#   dist_cutoff = 250,
#   lag_cutoff = 6
# )
print(summary(model1))

# Precipitation
cat("\nModel 2: Precipitation shock\n")
formula2 <- prop_verde ~ cont_shock_precip + year_state_trend + urban +
            log_renda + alphabetization_rate
model2 <- lm(formula2, data = data)
print(summary(model2))

# Temperature + Precipitation
cat("\nModel 3: Temperature + Precipitation shock\n")
formula3 <- prop_verde ~ cont_shock_temp + cont_shock_precip +
            year_state_trend + urban + log_renda + alphabetization_rate
model3 <- lm(formula3, data = data)
print(summary(model3))

# Linear combination (equivalent to lincom in Stata)
cat("\nLinear combination: cont_shock_temp + cont_shock_precip\n")
coef_temp <- coef(model3)["cont_shock_temp"]
coef_precip <- coef(model3)["cont_shock_precip"]
lincom_result <- coef_temp + coef_precip
cat("Combined effect:", lincom_result, "\n")

# ============================================================================
# MEDIUM-TERM ANALYSIS
# ============================================================================

cat("\n=== MEDIUM-TERM ANALYSIS ===\n\n")

# Create lags for precipitation shocks
data <- data %>%
  arrange(id, year) %>%
  group_by(id) %>%
  mutate(
    cont_shock_precip_lag1 = lag(cont_shock_precip, 1),
    cont_shock_precip_lag2 = lag(cont_shock_precip, 2),
    cont_shock_precip_lag3 = lag(cont_shock_precip, 3),
    cont_shock_precip_lag4 = lag(cont_shock_precip, 4),
    cont_shock_precip_lag5 = lag(cont_shock_precip, 5)
  ) %>%
  ungroup()

# Create lags for temperature shocks
data <- data %>%
  arrange(id, year) %>%
  group_by(id) %>%
  mutate(
    cont_shock_temp_lag1 = lag(cont_shock_temp, 1),
    cont_shock_temp_lag2 = lag(cont_shock_temp, 2),
    cont_shock_temp_lag3 = lag(cont_shock_temp, 3),
    cont_shock_temp_lag4 = lag(cont_shock_temp, 4),
    cont_shock_temp_lag5 = lag(cont_shock_temp, 5)
  ) %>%
  ungroup()

# Create interaction terms for precipitation
data <- data %>%
  mutate(
    cont_shock_precip_int_lag1 = cont_shock_precip * cont_shock_precip_lag1,
    cont_shock_precip_int_lag2 = cont_shock_precip * cont_shock_precip_lag2,
    cont_shock_precip_int_lag3 = cont_shock_precip * cont_shock_precip_lag3,
    cont_shock_precip_int_lag4 = cont_shock_precip * cont_shock_precip_lag4,
    cont_shock_precip_int_lag5 = cont_shock_precip * cont_shock_precip_lag5
  )

# Create interaction terms for temperature
data <- data %>%
  mutate(
    cont_shock_temp_int_lag1 = cont_shock_temp * cont_shock_temp_lag1,
    cont_shock_temp_int_lag2 = cont_shock_temp * cont_shock_temp_lag2,
    cont_shock_temp_int_lag3 = cont_shock_temp * cont_shock_temp_lag3,
    cont_shock_temp_int_lag4 = cont_shock_temp * cont_shock_temp_lag4,
    cont_shock_temp_int_lag5 = cont_shock_temp * cont_shock_temp_lag5
  )

# Calculate means for temperature lags
mean_temp_lag1 <- mean(data$cont_shock_temp_lag1, na.rm = TRUE)
mean_temp_lag2 <- mean(data$cont_shock_temp_lag2, na.rm = TRUE)
mean_temp_lag3 <- mean(data$cont_shock_temp_lag3, na.rm = TRUE)
mean_temp_lag4 <- mean(data$cont_shock_temp_lag4, na.rm = TRUE)
mean_temp_lag5 <- mean(data$cont_shock_temp_lag5, na.rm = TRUE)

cat("Mean temperature lags:\n")
cat("Lag 1:", mean_temp_lag1, "\n")
cat("Lag 2:", mean_temp_lag2, "\n")
cat("Lag 3:", mean_temp_lag3, "\n")
cat("Lag 4:", mean_temp_lag4, "\n")
cat("Lag 5:", mean_temp_lag5, "\n\n")

# Temperature model with lags and interactions
cat("Model 4: Temperature with lags and interactions\n")
formula_temp_lags <- prop_verde ~ prop_verde_lag1 +
                     cont_shock_temp + cont_shock_temp_lag1 + cont_shock_temp_lag2 +
                     cont_shock_temp_lag3 + cont_shock_temp_lag4 + cont_shock_temp_lag5 +
                     cont_shock_temp_int_lag1 + cont_shock_temp_int_lag2 +
                     cont_shock_temp_int_lag3 + cont_shock_temp_int_lag4 +
                     cont_shock_temp_int_lag5 + year_state_trend + urban +
                     log_renda + alphabetization_rate

model_temp_lags <- lm(formula_temp_lags, data = data)
print(summary(model_temp_lags))

# Linear combination for temperature (equivalent to lincom in Stata)
cat("\nLinear combination for temperature:\n")
coefs <- coef(model_temp_lags)
lincom_temp <- coefs["cont_shock_temp"] +
               mean_temp_lag1 * coefs["cont_shock_temp_int_lag1"] +
               mean_temp_lag2 * coefs["cont_shock_temp_int_lag2"] +
               mean_temp_lag3 * coefs["cont_shock_temp_int_lag3"] +
               mean_temp_lag4 * coefs["cont_shock_temp_int_lag4"] +
               mean_temp_lag5 * coefs["cont_shock_temp_int_lag5"]
cat("Combined temperature effect:", lincom_temp, "\n\n")

# Precipitation model with lags and interactions
cat("Model 5: Precipitation with lags and interactions\n")

# Calculate means for precipitation lags
mean_precip_lag1 <- mean(data$cont_shock_precip_lag1, na.rm = TRUE)
mean_precip_lag2 <- mean(data$cont_shock_precip_lag2, na.rm = TRUE)
mean_precip_lag3 <- mean(data$cont_shock_precip_lag3, na.rm = TRUE)
mean_precip_lag4 <- mean(data$cont_shock_precip_lag4, na.rm = TRUE)
mean_precip_lag5 <- mean(data$cont_shock_precip_lag5, na.rm = TRUE)

cat("Mean precipitation lags:\n")
cat("Lag 1:", mean_precip_lag1, "\n")
cat("Lag 2:", mean_precip_lag2, "\n")
cat("Lag 3:", mean_precip_lag3, "\n")
cat("Lag 4:", mean_precip_lag4, "\n")
cat("Lag 5:", mean_precip_lag5, "\n\n")

formula_precip_lags <- prop_verde ~ prop_verde_lag1 +
                       cont_shock_precip + cont_shock_precip_lag1 + cont_shock_precip_lag2 +
                       cont_shock_precip_lag3 + cont_shock_precip_lag4 + cont_shock_precip_lag5 +
                       cont_shock_precip_int_lag1 + cont_shock_precip_int_lag2 +
                       cont_shock_precip_int_lag3 + cont_shock_precip_int_lag4 +
                       cont_shock_precip_int_lag5 + year_state_trend + urban +
                       log_renda + alphabetization_rate

model_precip_lags <- lm(formula_precip_lags, data = data)
print(summary(model_precip_lags))

# Linear combination for precipitation
cat("\nLinear combination for precipitation:\n")
coefs_precip <- coef(model_precip_lags)
lincom_precip <- coefs_precip["cont_shock_precip"] +
                 mean_precip_lag1 * coefs_precip["cont_shock_precip_int_lag1"] +
                 mean_precip_lag2 * coefs_precip["cont_shock_precip_int_lag2"] +
                 mean_precip_lag3 * coefs_precip["cont_shock_precip_int_lag3"] +
                 mean_precip_lag4 * coefs_precip["cont_shock_precip_int_lag4"] +
                 mean_precip_lag5 * coefs_precip["cont_shock_precip_int_lag5"]
cat("Combined precipitation effect:", lincom_precip, "\n\n")

# Combined Temperature + Precipitation model
cat("Model 6: Combined Temperature + Precipitation with lags and interactions\n")

# Recalculate means for combined model (using same labels as Stata code)
mean_lag1 <- mean(data$cont_shock_temp_lag1, na.rm = TRUE)
mean_lag2 <- mean(data$cont_shock_temp_lag2, na.rm = TRUE)
mean_lag3 <- mean(data$cont_shock_temp_lag3, na.rm = TRUE)
mean_lag4 <- mean(data$cont_shock_temp_lag4, na.rm = TRUE)
mean_lag5 <- mean(data$cont_shock_temp_lag5, na.rm = TRUE)

mean_lag6 <- mean(data$cont_shock_precip_lag1, na.rm = TRUE)
mean_lag7 <- mean(data$cont_shock_precip_lag2, na.rm = TRUE)
mean_lag8 <- mean(data$cont_shock_precip_lag3, na.rm = TRUE)
mean_lag9 <- mean(data$cont_shock_precip_lag4, na.rm = TRUE)
mean_lag10 <- mean(data$cont_shock_precip_lag5, na.rm = TRUE)

formula_combined <- prop_verde ~ prop_verde_lag1 +
                    cont_shock_temp + cont_shock_temp_lag1 + cont_shock_temp_lag2 +
                    cont_shock_temp_lag3 + cont_shock_temp_lag4 + cont_shock_temp_lag5 +
                    cont_shock_temp_int_lag1 + cont_shock_temp_int_lag2 +
                    cont_shock_temp_int_lag3 + cont_shock_temp_int_lag4 +
                    cont_shock_temp_int_lag5 +
                    cont_shock_precip + cont_shock_precip_lag1 + cont_shock_precip_lag2 +
                    cont_shock_precip_lag3 + cont_shock_precip_lag4 + cont_shock_precip_lag5 +
                    cont_shock_precip_int_lag1 + cont_shock_precip_int_lag2 +
                    cont_shock_precip_int_lag3 + cont_shock_precip_int_lag4 +
                    cont_shock_precip_int_lag5 + year_state_trend + urban +
                    log_renda + alphabetization_rate

model_combined <- lm(formula_combined, data = data)
print(summary(model_combined))

# Linear combination for combined model
cat("\nLinear combination for combined model:\n")
coefs_comb <- coef(model_combined)
lincom_combined <- coefs_comb["cont_shock_temp"] +
                   mean_lag1 * coefs_comb["cont_shock_temp_int_lag1"] +
                   mean_lag2 * coefs_comb["cont_shock_temp_int_lag2"] +
                   mean_lag3 * coefs_comb["cont_shock_temp_int_lag3"] +
                   mean_lag4 * coefs_comb["cont_shock_temp_int_lag4"] +
                   mean_lag5 * coefs_comb["cont_shock_temp_int_lag5"] +
                   coefs_comb["cont_shock_precip"] +
                   mean_lag6 * coefs_comb["cont_shock_precip_int_lag1"] +
                   mean_lag7 * coefs_comb["cont_shock_precip_int_lag2"] +
                   mean_lag8 * coefs_comb["cont_shock_precip_int_lag3"] +
                   mean_lag9 * coefs_comb["cont_shock_precip_int_lag4"] +
                   mean_lag10 * coefs_comb["cont_shock_precip_int_lag5"]
cat("Combined effect (temp + precip):", lincom_combined, "\n")

# ============================================================================
# Note on Conley Spatial Standard Errors
# ============================================================================

cat("\n=== NOTE ===\n")
cat("To implement Conley spatial standard errors (equivalent to reg2hdfespatial in Stata),\n")
cat("uncomment the conleyreg function calls in the code above.\n")
cat("This requires the conleyreg package to be installed:\n")
cat("install.packages('conleyreg')\n\n")
cat("Alternative packages for spatial standard errors:\n")
cat("- plm package with vcovHC for panel-robust SEs\n")
cat("- fixest package with conley() function\n")
cat("- Or use the acreg package for Conley HAC standard errors\n")
