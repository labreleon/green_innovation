# Analysis Workflow Guide

## Table of Contents

1. [Overview](#overview)
2. [Prerequisites](#prerequisites)
3. [Step-by-Step Execution](#step-by-step-execution)
4. [Script Execution Order](#script-execution-order)
5. [Expected Outputs](#expected-outputs)
6. [Quality Checks](#quality-checks)
7. [Troubleshooting](#troubleshooting)

---

## Overview

This document provides a detailed guide for executing the complete analysis pipeline from raw data to final publication-ready outputs.

**Total estimated time**: 2-4 hours (depending on hardware and data size)

**Recommended approach**: Run scripts step-by-step initially to understand the workflow, then use `run_all.R` for subsequent runs.

---

## Prerequisites

### 1. Software Installation

**R (version 4.0+)**
- Download from: https://cran.r-project.org/
- Verify installation: `R --version`

**RStudio (recommended)**
- Download from: https://posit.co/download/rstudio-desktop/

**Stata (version 15+)**
- Required for econometric models
- Ensure you have a valid license

### 2. Project Setup

```r
# 1. Open R/RStudio
# 2. Set working directory to project root
setwd("/path/to/green_innovation")

# 3. Run setup script
source("00_setup.R")
```

The setup script will:
- Install required R packages (if missing)
- Create output directories
- Load common functions
- Set global parameters

### 3. Data Preparation

Place your raw data files in the appropriate location:

```
green_innovation/
└── data/
    └── raw/
        ├── climate/              # Temperature and precipitation files
        ├── patents/              # Patent data from INPI
        ├── rais/                 # Employment data
        ├── annel/                # Renewable energy projects
        ├── geography/            # Municipality shapefiles/data
        ├── firms/                # Corporate registry data
        └── controls/             # Education, disasters, etc.
```

**Note**: Exact file names and formats should match those expected by the processing scripts.

---

## Step-by-Step Execution

### STEP 0: Environment Setup

**Script**: `00_setup.R`

**Purpose**: Prepare R environment

**Actions**:
1. Install missing packages
2. Load all required libraries
3. Create output directories
4. Define helper functions
5. Set global parameters

**Run**:
```r
source("00_setup.R")
```

**Expected output**:
```
Setting up R environment for Green Innovation project...

Installing missing packages...
Loading packages...
  ✓ data.table
  ✓ fixest
  ✓ geobr
  ...

Setup complete! Ready to run analysis scripts.
```

**Duration**: 1-5 minutes (longer if packages need installation)

---

### STEP 1: Data Processing

**Location**: `01_data_processing/`

**Purpose**: Clean and standardize raw data from various sources

#### 1.1 Climate Data Processing

**Scripts** (run in order):

```r
# Temperature data
source("01_data_processing/climate/processamento_dados_temperatura_maxima.R")
source("01_data_processing/climate/processamento_dados_temperatura_maxima_micro.R")

# Precipitation data
source("01_data_processing/climate/processamento_dados_precipitacao.R")
source("01_data_processing/climate/processamento_dados_precipitacao_micro.R")

# Combined climate data
source("01_data_processing/climate/processamento_dados_climaticos.R")
source("01_data_processing/climate/processamento_dados_climaticos_combinados.R")
source("01_data_processing/climate/processamento_dados_climaticos_combinados_micro.R")

# Climate data with education controls
source("01_data_processing/climate/processamento_dados_climaticos_educacionais.R")
source("01_data_processing/climate/processamento_dados_climaticos_educacionais_micro.R")
```

**What each script does**:
- Reads raw climate data (NetCDF, CSV, or other formats)
- Calculates municipality-level aggregates
- Computes climate shock indicators (percentiles)
- Saves processed data as `.rds` files

**Expected outputs**:
- `data/processed/temperatura_maxima.rds`
- `data/processed/temperatura_maxima_micro.rds`
- `data/processed/precipitacao.rds`
- `data/processed/precipitacao_micro.rds`
- `data/processed/clima_combinado.rds`

**Duration**: 15-45 minutes (depends on raw data size)

#### 1.2 Green Economy Data Processing

**Scripts**:

```r
# Employment in green sectors (RAIS)
source("01_data_processing/green_economy/processamento_dados_rais_economia_verde.R")
source("01_data_processing/green_economy/dados_rais.R")

# Patent data
source("01_data_processing/green_economy/processamento_dados_patentes.R")

# Green sector classifications
source("01_data_processing/green_economy/processamento_legenda_economia_verde.R")
source("01_data_processing/green_economy/processamento_taxonomia_verde_febrabran.R")
source("01_data_processing/green_economy/tabela_cnae_ambiental.R")

# Renewable energy projects
source("01_data_processing/green_economy/processamento_annel.R")
```

**What these scripts do**:
- Process employment data (filter green sectors by CNAE codes)
- Clean patent data (identify green technology classifications)
- Create green sector taxonomies
- Process renewable energy project registrations

**Expected outputs**:
- `data/processed/rais_economia_verde.rds`
- `data/processed/patentes_verdes.rds`
- `data/processed/taxonomia_verde.rds`
- `data/processed/annel_renovaveis.rds`

**Duration**: 10-30 minutes

#### 1.3 Geographic Data Processing

**Scripts**:

```r
source("01_data_processing/geography/processamento_dados_municipios.R")
source("01_data_processing/geography/processamento_dados_mun_lat_lon.R")
source("01_data_processing/geography/processamento_dados_regiao_imediata.R")
```

**What these scripts do**:
- Load municipality boundaries and identifiers
- Extract geographic coordinates (centroids)
- Assign regional classifications

**Expected outputs**:
- `data/processed/municipios.rds`
- `data/processed/municipios_coordenadas.rds`
- `data/processed/regioes_imediatas.rds`

**Duration**: 5-10 minutes

#### 1.4 Firm Data Processing

**Scripts**:

```r
source("01_data_processing/firms/processamento_dados_quadro_societario.R")
source("01_data_processing/firms/dados_quadro_societario.R")
```

**What these scripts do**:
- Process corporate registry data
- Extract firm characteristics and ownership

**Expected outputs**:
- `data/processed/quadro_societario.rds`

**Duration**: 5-15 minutes

#### 1.5 Control Variables Processing

**Scripts**:

```r
source("01_data_processing/controls/processamento_dados_censo_educacao.R")
source("01_data_processing/controls/processamento_dados_censo_educacao_micro.R")
source("01_data_processing/controls/processamento_desastres.R")
source("01_data_processing/controls/prepare_control_variables.R")
```

**What these scripts do**:
- Process education census data (literacy rates, school enrollment)
- Process natural disaster records
- Prepare all control variables for regressions

**Expected outputs**:
- `data/processed/educacao_municipio.rds`
- `data/processed/desastres.rds`
- `data/processed/controles.rds`

**Duration**: 5-15 minutes

---

### STEP 2: Data Merge

**Location**: `02_data_merge/`

**Purpose**: Combine processed datasets into analysis-ready files

**Scripts**:

```r
# Aggregated (municipality-year) level
source("02_data_merge/merge_weather_green_outcomes.R")

# Micro (firm/individual-year) level
source("02_data_merge/merge_weather_green_outcomes_micro.R")
```

**What these scripts do**:
- Merge climate data with green economy outcomes
- Add geographic identifiers and control variables
- Create lagged variables for dynamic analysis
- Validate merges (check for missing values, duplicates)

**Expected outputs**:
- `data/merged/analysis_data_agregado.rds`
- `data/merged/analysis_data_micro.rds`

**Quality checks performed**:
- Number of observations before/after merge
- Missing value patterns
- Temporal coverage (year range)
- Geographic coverage (municipality count)

**Duration**: 5-15 minutes

---

### STEP 3: Statistical Analysis

**Location**: `03_analysis/`

**Purpose**: Estimate models and generate statistical results

#### 3.1 Descriptive Statistics

**Script**:

```r
source("03_analysis/descriptive/descriptive_statistics.R")
```

**What this script does**:
- Calculate summary statistics for all variables
- Generate correlation matrices
- Create balance tables
- Export tables to LaTeX format

**Expected outputs**:
- `output/tables/descriptive_stats.tex`
- `output/tables/correlations.tex`
- Console output with key statistics

**Duration**: 2-5 minutes

#### 3.2 Climate Shock Analysis (R)

**Scripts** (run all or select specific ones):

```r
# Main specification
source("03_analysis/climate_shocks/long_shock.R")

# Alternative specifications
source("03_analysis/climate_shocks/long_shock_absolut.R")      # Absolute values
source("03_analysis/climate_shocks/long_shock_per_capita.R")   # Per capita outcomes

# Robustness checks
source("03_analysis/climate_shocks/long_shock_robustness_1.R")
source("03_analysis/climate_shocks/long_shock_robustness_2.R")

# Climate shock definitions and summary
source("03_analysis/climate_shocks/climate_shocks_analysis.R")
```

**What these scripts do**:
- Estimate distributed lag models
- Calculate cumulative effects of climate shocks
- Test for heterogeneous effects (by region, size, etc.)
- Generate coefficient plots and event studies

**Expected outputs**:
- `output/tables/long_shock_results.tex`
- `output/tables/robustness_checks.tex`
- `output/figures/event_study_plots.png`

**Duration**: 20-60 minutes (depends on model complexity)

#### 3.3 Econometric Models (Stata)

**Location**: `03_analysis/econometric_models/stata/`

**Important**: These scripts must be run in Stata, not R.

**Step 1**: Add custom Stata commands to your ado path

Open Stata and run:

```stata
* Add custom ado files
adopath + "03_analysis/econometric_models/stata_commands"

* Verify installation
which ols_spatial_HAC
which reg2hdfespatial
which DCdensity
```

**Step 2**: Run regression scripts

```stata
* Main specification
do "03_analysis/econometric_models/stata/reg.do"

* Micro-level regressions
do "03_analysis/econometric_models/stata/reg_micro.do"

* Robustness checks
do "03_analysis/econometric_models/stata/reg_robustness.do"
```

**What these scripts do**:
- Estimate fixed effects models with spatial HAC standard errors
- Run specification tests
- Export regression tables

**Expected outputs**:
- `output/tables/stata_main_results.tex`
- `output/tables/stata_micro_results.tex`
- Stata log files in `output/logs/`

**Duration**: 10-30 minutes

**Custom Stata commands included**:

| Command | Purpose |
|---------|---------|
| `ols_spatial_HAC.ado` | OLS with spatial HAC-robust standard errors |
| `reg2hdfespatial.ado` | High-dimensional FE with spatial corrections |
| `DCdensity.ado` | McCrary density discontinuity test |

---

### STEP 4: Visualization

**Location**: `04_visualization/`

**Purpose**: Generate publication-quality figures

**Scripts**:

```r
# Weather maps (spatial distribution)
source("04_visualization/weather_maps.R")

# Time series of climate variables
source("04_visualization/weather_time_series.R")

# Patent trends
source("04_visualization/grafico_patent.R")

# Employment trends (RAIS)
source("04_visualization/grafico_rais.R")

# Socioeconomic indicators
source("04_visualization/grafico_socio.R")

# Patent data figures
source("04_visualization/figura_data_patente.R")
```

**What these scripts do**:
- Create maps showing geographic variation
- Plot time series with trends
- Generate scatter plots and correlations
- Export high-resolution figures (300 dpi PNG)

**Expected outputs**:
- `output/figures/weather_map_temperature.png`
- `output/figures/weather_map_precipitation.png`
- `output/figures/timeseries_climate.png`
- `output/figures/patent_trends.png`
- `output/figures/rais_employment.png`
- `output/figures/socioeconomic_trends.png`

**Duration**: 5-15 minutes

---

## Script Execution Order

### Quick Reference Table

| Step | Script | Type | Duration | Output Location |
|------|--------|------|----------|----------------|
| 0 | `00_setup.R` | Setup | 1-5 min | N/A |
| 1.1 | `01_data_processing/climate/*.R` | Data | 15-45 min | `data/processed/` |
| 1.2 | `01_data_processing/green_economy/*.R` | Data | 10-30 min | `data/processed/` |
| 1.3 | `01_data_processing/geography/*.R` | Data | 5-10 min | `data/processed/` |
| 1.4 | `01_data_processing/firms/*.R` | Data | 5-15 min | `data/processed/` |
| 1.5 | `01_data_processing/controls/*.R` | Data | 5-15 min | `data/processed/` |
| 2 | `02_data_merge/*.R` | Data | 5-15 min | `data/merged/` |
| 3.1 | `03_analysis/descriptive/*.R` | Analysis | 2-5 min | `output/tables/` |
| 3.2 | `03_analysis/climate_shocks/*.R` | Analysis | 20-60 min | `output/tables/`, `output/figures/` |
| 3.3 | `03_analysis/econometric_models/stata/*.do` | Analysis (Stata) | 10-30 min | `output/tables/` |
| 4 | `04_visualization/*.R` | Viz | 5-15 min | `output/figures/` |

**Total time**: 2-4 hours

---

## Expected Outputs

### Data Files

**Processed data** (`data/processed/`):
- Climate: `temperatura_maxima.rds`, `precipitacao.rds`, etc.
- Green economy: `patentes_verdes.rds`, `rais_economia_verde.rds`, `annel_renovaveis.rds`
- Geography: `municipios.rds`, `municipios_coordenadas.rds`
- Controls: `educacao_municipio.rds`, `desastres.rds`

**Merged data** (`data/merged/`):
- `analysis_data_agregado.rds` - Municipality-year panel
- `analysis_data_micro.rds` - Firm/individual-year panel

### Regression Tables

**Location**: `output/tables/`

**Files**:
- `descriptive_stats.tex` - Summary statistics
- `long_shock_results.tex` - Main climate shock results
- `robustness_checks.tex` - Robustness specifications
- `stata_main_results.tex` - Stata regression outputs
- `stata_micro_results.tex` - Micro-level results

**Format**: LaTeX (can be included in manuscripts)

### Figures

**Location**: `output/figures/`

**Files**:
- `weather_map_*.png` - Spatial maps of climate variables
- `timeseries_*.png` - Time series plots
- `patent_trends.png` - Patent application trends
- `rais_employment.png` - Green employment trends
- `event_study_*.png` - Event study plots

**Format**: PNG, 300 dpi, publication-ready

### Logs

**Location**: `output/logs/`

**Files**:
- `execution_log.txt` - Timestamp of each script execution
- `*.log` - Stata log files

---

## Quality Checks

### After Each Step

Perform the following checks to ensure data quality:

#### 1. Data Processing (Step 1)

```r
# Check a processed file
data <- readRDS("data/processed/temperatura_maxima.rds")

# Verify structure
str(data)
summary(data)

# Check for missing values
colSums(is.na(data))

# Check time coverage
range(data$year)

# Check geographic coverage
length(unique(data$cod_municipio))
```

**Expected**:
- No unexpected missing values
- Time range: 1940-2020
- ~5,570 Brazilian municipalities

#### 2. Data Merge (Step 2)

```r
# Load merged data
data_merged <- readRDS("data/merged/analysis_data_agregado.rds")

# Check merge success
cat("Total observations:", nrow(data_merged), "\n")
cat("Municipalities:", length(unique(data_merged$cod_municipio)), "\n")
cat("Years:", range(data_merged$year), "\n")

# Check for duplicates
duplicates <- data_merged[duplicated(data_merged[, .(cod_municipio, year)]), ]
if (nrow(duplicates) > 0) {
  warning("Duplicates found!")
}

# Check variable completeness
missing_pct <- colMeans(is.na(data_merged)) * 100
print(missing_pct[missing_pct > 5])  # Variables with >5% missing
```

#### 3. Analysis (Step 3)

**For R regressions**:

```r
# Check if tables were created
list.files("output/tables/", pattern = "\\.tex$")

# Check if figures were created
list.files("output/figures/", pattern = "\\.png$")
```

**For Stata regressions**:

- Review log files for errors
- Check that coefficients are reasonable
- Verify standard errors and p-values

#### 4. Visualization (Step 4)

- Open each figure and verify:
  - Axes are labeled correctly
  - Legend is readable
  - Resolution is high enough (300 dpi)
  - No overlapping text

---

## Troubleshooting

### Common Issues and Solutions

#### Issue 1: Package installation fails

**Error**: `package 'X' is not available for this version of R`

**Solution**:
```r
# Update R to latest version
# Or install from CRAN archives
install.packages("X", repos = "http://cran.us.r-project.org")
```

#### Issue 2: File not found

**Error**: `cannot open file 'data/raw/X.csv': No such file or directory`

**Solution**:
- Verify raw data file exists in `data/raw/`
- Check file name spelling and extension
- Check file path in script (update if necessary)

#### Issue 3: Memory issues

**Error**: `Error: cannot allocate vector of size X Gb`

**Solution**:
```r
# Increase memory limit (Windows)
memory.limit(size = 16000)

# Use data.table instead of data.frame
library(data.table)
data <- fread("large_file.csv")  # Much faster than read.csv

# Process data in chunks
```

#### Issue 4: Merge produces unexpected results

**Error**: Too many/too few observations after merge

**Solution**:
```r
# Check keys before merging
library(data.table)
setDT(data1)
setDT(data2)

# Check for duplicates in merge keys
data1[, .N, by = .(cod_municipio, year)][N > 1]

# Use explicit merge with indicators
merged <- merge(data1, data2,
                by = c("cod_municipio", "year"),
                all.x = TRUE, all.y = FALSE)

# Check merge success
table(merged$merge_indicator)
```

#### Issue 5: Stata ado files not found

**Error**: `unrecognized command: ols_spatial_HAC`

**Solution**:
```stata
* Add directory to Stata ado path
adopath + "/full/path/to/03_analysis/econometric_models/stata_commands"

* Or copy ado files to personal ado directory
* (usually ~/ado/plus/ on Mac/Linux, or C:\ado\plus\ on Windows)
```

#### Issue 6: Figures not saving

**Error**: No error, but figures don't appear in output folder

**Solution**:
```r
# Check if output directory exists
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

# Use absolute paths
ggsave(filename = file.path(getwd(), "output/figures/myplot.png"),
       plot = myplot)

# Check write permissions
file.access("output/figures", mode = 2)  # Should return 0
```

---

## Advanced: Parallel Processing

To speed up data processing on multi-core machines:

```r
# In 00_setup.R or at start of script
library(future)
library(future.apply)

# Set up parallel processing
plan(multisession, workers = parallel::detectCores() - 1)

# Example: Process multiple municipalities in parallel
results <- future_lapply(municipality_list, function(mun) {
  # Process data for this municipality
  process_municipality(mun)
})
```

**Caution**: Parallel processing uses more memory. Monitor system resources.

---

## Next Steps After Workflow Completion

1. **Review outputs**: Check all tables and figures
2. **Quality checks**: Verify results match expectations
3. **Compile manuscript**: Incorporate tables and figures
4. **Sensitivity analysis**: Test alternative specifications
5. **Prepare presentation**: Create slides with key findings

---

## Additional Resources

- **R Documentation**: https://www.rdocumentation.org/
- **Stata Documentation**: https://www.stata.com/manuals/
- **fixest package**: https://lrberge.github.io/fixest/
- **geobr package**: https://ipeagit.github.io/geobr/

---

**Questions?** Contact the research team or open an issue on GitHub.
