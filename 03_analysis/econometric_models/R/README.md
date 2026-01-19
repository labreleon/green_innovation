# R Scripts - Econometric Models

This directory contains R scripts converted from Stata code for econometric analysis of climate shocks and green innovation.

## Files

### 1. `reg.R`
Main econometric analysis with panel data regression models.

**Purpose:**
- Runs panel data regressions with fixed effects
- Estimates models with Conley spatial standard errors
- Generates LaTeX tables with regression results

**Key Features:**
- Panel data analysis with municipality and year fixed effects
- Multiple specifications with different shock measures (moving averages, contemporaneous, lags)
- Spatial standard errors accounting for spatial correlation (Conley HAC)
- Clustered standard errors by municipality
- Automated LaTeX table generation

**Data Required:**
- `weather_rais.dta` - Panel data with climate shocks and innovation outcomes

### 2. `reg_micro.R`
Micro-level analysis of short-term and medium-term effects.

**Purpose:**
- Analyzes short-term effects of climate shocks
- Examines medium-term effects with lag structures
- Tests interaction effects between current and past shocks

**Key Features:**
- Short-term analysis: Direct effects of temperature and precipitation shocks
- Medium-term analysis: Effects with 5-year lag structures
- Interaction terms between current and lagged shocks
- Linear combinations to assess cumulative effects
- Controls for socioeconomic variables (urbanization, income, literacy)

**Data Required:**
- `weather_quadro_societario.dta` - Panel data with firm-level information

### 3. `utility_functions.R`
Helper functions for file and directory management.

**Purpose:**
- Provides R equivalents to Stata utility functions
- Simplifies temporary file and directory management

**Functions:**
- `get_temp_dir()` - Get system temporary directory
- `create_temp_file()` - Create temporary file path
- `confirm_dir()` - Check if directory exists
- `ensure_dir_exists()` - Create directory if needed
- Path manipulation utilities

## Required R Packages

Install all required packages:

```r
# Core packages
install.packages("haven")        # Read Stata files
install.packages("dplyr")        # Data manipulation
install.packages("data.table")   # Efficient data handling

# Panel data and fixed effects
install.packages("plm")          # Panel data models
install.packages("fixest")       # High-dimensional fixed effects
install.packages("lfe")          # Linear fixed effects

# Standard errors
install.packages("sandwich")     # Robust standard errors
install.packages("lmtest")       # Coefficient testing

# Spatial standard errors (choose one)
install.packages("ConleySEs")    # Conley spatial HAC (recommended)
# OR
install.packages("acreg")        # Alternative for Conley SEs

# Tables and output
install.packages("stargazer")    # LaTeX/HTML tables
install.packages("texreg")       # Alternative table package
```

## Quick Start

### Load a single script:
```r
source("reg.R")
```

### Load utility functions:
```r
source("utility_functions.R")

# Examples
temp_dir <- get_temp_dir()
ensure_dir_exists("output/tables")
```

## Data Paths

**Important:** Update the data paths in each script to match your local directory structure:

```r
# Original paths (from Stata code):
# "C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation/output/final_base/..."

# Update to your paths:
data <- read_dta("path/to/your/data/weather_rais.dta")
```

## Key Differences from Stata Code

### 1. Panel Data Setup
**Stata:**
```stata
xtset id year
xtreg y x1 x2, fe cluster(mun_code)
```

**R:**
```r
pdata <- pdata.frame(data, index = c("id", "year"))
model <- plm(y ~ x1 + x2, data = pdata, model = "within")
vcov_cluster <- vcovHC(model, cluster = "group")
```

### 2. Creating Lags
**Stata:**
```stata
gen x_lag1 = x[_n-1]
```

**R:**
```r
data <- data %>%
  group_by(id) %>%
  mutate(x_lag1 = lag(x, 1)) %>%
  ungroup()
```

### 3. Conley Spatial Standard Errors
**Stata:**
```stata
reg2hdfespatial y x1 x2, timevar(round) panelvar(id) lat(lat) lon(lon) distcutoff(250) lagcutoff(6)
```

**R:**
```r
model <- lm(y ~ x1 + x2, data = data)
conley_se <- ConleySEs::ConleySEs(
  reg = model,
  unit = data$id,
  time = data$round,
  lat = data$lat,
  lon = data$lon,
  dist_cutoff = 250,
  lag_cutoff = 6
)
```

### 4. Linear Combinations (lincom)
**Stata:**
```stata
lincom x1 + x2
```

**R:**
```r
coefs <- coef(model)
combined_effect <- coefs["x1"] + coefs["x2"]
```

### 5. Generating Dummies
**Stata:**
```stata
tabulate year, generate(year_dummy)
```

**R:**
```r
year_dummies <- model.matrix(~ factor(year) - 1, data = data)
```

## Notes on Spatial Standard Errors

The original Stata code uses `reg2hdfespatial`, which implements Conley (1999) spatial HAC standard errors. In R, there are several options:

1. **ConleySEs package** (Recommended)
   - Direct implementation of Conley spatial HAC
   - Handles panel data structure
   - Accounts for both spatial and temporal correlation

2. **fixest package with conley()**
   - Fast implementation
   - Integrated with fixed effects estimation
   - Example: `feols(y ~ x1 + x2 | id + year, data = data, vcov = conley(250))`

3. **acreg package**
   - Alternative implementation
   - May be slower but more flexible

4. **Manual implementation**
   - Can be done using spatial weight matrices
   - More control but more complex

## Running the Analysis

### Complete workflow:
```r
# 1. Load packages
library(haven)
library(dplyr)
library(fixest)
library(ConleySEs)

# 2. Source utility functions (optional)
source("utility_functions.R")

# 3. Run main analysis
source("reg.R")

# 4. Run micro-level analysis
source("reg_micro.R")
```

### Running specific models:
```r
# After sourcing reg.R, the results are stored in the 'results' list
print(results$obs_1)  # Number of observations for combination 1
```

## Output

### reg.R outputs:
- LaTeX table file: `rais_estimation.tex`
- Results stored in `results` list

### reg_micro.R outputs:
- Console output with model summaries
- Linear combination effects printed to console

## Troubleshooting

### Issue: "Error: package 'ConleySEs' not available"
**Solution:**
```r
# ConleySEs may need to be installed from GitHub
# install.packages("devtools")
devtools::install_github("darinchristensen/conley-se")
```

### Issue: "Error in read_dta(): file not found"
**Solution:** Update the file paths in the scripts to match your directory structure.

### Issue: High-dimensional fixed effects too slow
**Solution:** Use `fixest::feols()` instead of `plm::plm()`:
```r
model <- feols(y ~ x1 + x2 | id + year, data = data, vcov = "cluster")
```

### Issue: Missing values in lagged variables
**Solution:** This is expected when creating lags. The analysis automatically handles NA values.

## References

- Conley, T.G. (1999). "GMM estimation with cross sectional dependence." *Journal of Econometrics*, 92(1), 1-45.
- Croissant, Y., & Millo, G. (2008). "Panel data econometrics in R: The plm package." *Journal of Statistical Software*, 27(2), 1-43.

## Contact

For questions about the R code conversion, please refer to the original Stata code in the `../stata/` directory.
