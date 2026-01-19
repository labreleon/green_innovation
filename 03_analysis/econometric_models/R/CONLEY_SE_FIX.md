# Fix for Conley Standard Errors in Panel Employment Estimates

## Problem

The original code was trying to use parameters (`id`, `time`, `fe`) that don't exist in the `conleyreg::conleyreg()` function, causing the error:

```
argumentos não utilizados (id = "mun_code", fe = "both")
```

(unused arguments: id = "mun_code", fe = "both")

## Root Cause

The `conleyreg` package in R has a simpler interface than what was being used:

```r
conleyreg(formula, data, lat, lon, dist_cutoff, lag_cutoff)
```

It does **not** have built-in support for panel fixed effects through `id`, `time`, or `fe` parameters.

## Solution

The fix implements a proper two-way fixed effects panel regression with Conley spatial HAC standard errors using a **within-transformation** approach:

### Step 1: Fixed Effects Regression
Uses `fixest::feols()` (already working correctly) to estimate the model with municipality and year fixed effects:

```r
Y_mt = β*T_mt + η*P_mt + α_m + γ_t + ε_mt
```

Where:
- `Y_mt` = outcome variable (total jobs, green jobs, or proportion)
- `T_mt` = temperature shock
- `P_mt` = precipitation shock
- `α_m` = municipality fixed effects
- `γ_t` = year fixed effects

### Step 2: Conley SE Calculation

To apply Conley SEs with two-way fixed effects:

1. **Demean variables by municipality** (within-transformation):
   - This absorbs the municipality fixed effects `α_m`
   - Variables: dependent variable, temperature, precipitation

2. **Include year dummies** (also demeaned):
   - Year fixed effects are included as dummy variables in the formula
   - These are also demeaned by municipality to maintain the within-transformation

3. **Apply Conley correction**:
   - Run `conleyreg()` on the demeaned data
   - Spatial correlation: 250 km cutoff (Conley 1999)
   - Temporal correlation: 7-year lag (Newey-West 1987)

### Code Changes

**Before:**
```r
conley_model <- conleyreg::conleyreg(
  formula = formula_conley,
  data = data_clean,
  id = "mun_code",        # ❌ Doesn't exist
  time = "year",          # ❌ Doesn't exist
  lat = "lat",
  lon = "lon",
  dist_cutoff = 250,
  lag_cutoff = 7,
  fe = "both"             # ❌ Doesn't exist
)
```

**After:**
```r
# Demean by municipality (within-transformation)
data_conley <- data_clean %>%
  group_by(mun_code) %>%
  mutate(
    Y_dm = Y - mean(Y),
    temp_dm = temp - mean(temp),
    precip_dm = precip - mean(precip)
  )

# Run Conley with demeaned data + year FEs
conley_model <- conleyreg::conleyreg(
  formula = Y_dm ~ temp_dm + precip_dm + year_2001_dm + year_2002_dm + ...,
  data = data_conley,
  lat = "lat",
  lon = "lon",
  dist_cutoff = 250,
  lag_cutoff = 7
)
```

## Additional Fixes

1. **Data path**: Changed from `"weather_rais.dta"` to `"./output/final_base/weather_rais.dta"`

2. **Variable renaming**: Added automatic renaming from `total_vinc` and `total_vinc_verde` to `total_jobs` and `green_jobs`

## Fallback Behavior

If Conley SE calculation fails for any reason, the code automatically falls back to using clustered standard errors (clustered by municipality) from the fixest regression.

## References

- Conley, T. G. (1999). GMM estimation with cross sectional dependence. *Journal of Econometrics*, 92(1), 1-45.
- Newey, W. K., & West, K. D. (1987). A simple, positive semi-definite, heteroskedasticity and autocorrelation consistent covariance matrix. *Econometrica*, 55(3), 703-708.

## Testing

To test the fixed code, run:

```r
source("03_analysis/econometric_models/R/reg_employment_shortrun.R")
```

The code should now:
1. Load and prepare the data correctly
2. Run three regressions (total jobs, green jobs, proportion)
3. Calculate Conley SEs successfully
4. Generate a LaTeX table with results
