# Project Structure Documentation

## Overview

This document provides a detailed explanation of the project's folder structure and organization principles.

**Last updated**: 2026-01-16

---

## Design Principles

The project structure follows these key principles:

1. **Sequential workflow**: Folders numbered 01-04 indicate execution order
2. **Separation of concerns**: Data processing, analysis, and visualization are separated
3. **Logical grouping**: Related files grouped by purpose (climate, green economy, etc.)
4. **Clear naming**: Descriptive folder and file names in Portuguese and English
5. **Documentation-first**: Comprehensive docs for reproducibility

---

## Directory Tree

```
green_innovation/
│
├── .git/                           # Git version control
├── .gitignore                      # Files to ignore in version control
│
├── README.md                       # Main project documentation
├── 00_setup.R                      # Environment setup script
├── run_all.R                       # Master pipeline orchestrator
│
├── 01_data_processing/             # STEP 1: Raw data cleaning
│   │
│   ├── climate/                    # Climate and weather data (9 scripts)
│   │   ├── processamento_dados_temperatura_maxima.R
│   │   ├── processamento_dados_temperatura_maxima_micro.R
│   │   ├── processamento_dados_precipitacao.R
│   │   ├── processamento_dados_precipitacao_micro.R
│   │   ├── processamento_dados_climaticos.R
│   │   ├── processamento_dados_climaticos_combinados.R
│   │   ├── processamento_dados_climaticos_combinados_micro.R
│   │   ├── processamento_dados_climaticos_educacionais.R
│   │   └── processamento_dados_climaticos_educacionais_micro.R
│   │
│   ├── green_economy/              # Green innovation indicators (7 scripts)
│   │   ├── processamento_dados_rais_economia_verde.R
│   │   ├── processamento_dados_patentes.R
│   │   ├── processamento_legenda_economia_verde.R
│   │   ├── processamento_taxonomia_verde_febrabran.R
│   │   ├── processamento_annel.R
│   │   ├── dados_rais.R
│   │   └── tabela_cnae_ambiental.R
│   │
│   ├── geography/                  # Geographic data (3 scripts)
│   │   ├── processamento_dados_municipios.R
│   │   ├── processamento_dados_mun_lat_lon.R
│   │   └── processamento_dados_regiao_imediata.R
│   │
│   ├── firms/                      # Firm-level data (2 scripts)
│   │   ├── processamento_dados_quadro_societario.R
│   │   └── dados_quadro_societario.R
│   │
│   └── controls/                   # Control variables (4 scripts)
│       ├── processamento_dados_censo_educacao.R
│       ├── processamento_dados_censo_educacao_micro.R
│       ├── processamento_desastres.R
│       └── prepare_control_variables.R
│
├── 02_data_merge/                  # STEP 2: Data integration (2 scripts)
│   ├── merge_weather_green_outcomes.R
│   └── merge_weather_green_outcomes_micro.R
│
├── 03_analysis/                    # STEP 3: Statistical analysis
│   │
│   ├── descriptive/                # Descriptive statistics (1 script)
│   │   └── descriptive_statistics.R
│   │
│   ├── climate_shocks/             # Climate impact analysis (6 scripts)
│   │   ├── climate_shocks_analysis.R
│   │   ├── long_shock.R
│   │   ├── long_shock_absolut.R
│   │   ├── long_shock_per_capita.R
│   │   ├── long_shock_robustness_1.R
│   │   └── long_shock_robustness_2.R
│   │
│   └── econometric_models/         # Regression models
│       │
│       ├── stata/                  # Stata regression scripts (3 scripts)
│       │   ├── reg.do
│       │   ├── reg_micro.do
│       │   └── reg_robustness.do
│       │
│       └── stata_commands/         # Custom Stata commands (3 ado files)
│           ├── ols_spatial_HAC.ado
│           ├── reg2hdfespatial.ado
│           └── DCdensity.ado
│
├── 04_visualization/               # STEP 4: Figures and maps (6 scripts)
│   ├── weather_maps.R
│   ├── weather_time_series.R
│   ├── grafico_patent.R
│   ├── grafico_rais.R
│   ├── grafico_socio.R
│   └── figura_data_patente.R
│
├── archive/                        # Archived/deprecated files
│   ├── reg_micro [Recovered].do
│   └── ~reg_micro [Recovered].do.stswp
│
├── docs/                           # Documentation
│   ├── project_structure.md        # This file
│   ├── workflow.md                 # Detailed workflow guide
│   └── data_dictionary.md          # Variable definitions
│
├── data/                           # Data files (NOT in version control)
│   ├── raw/                        # Raw data from sources
│   ├── processed/                  # Cleaned data (.rds files)
│   └── merged/                     # Analysis-ready datasets
│
└── output/                         # Output files (NOT in version control)
    ├── figures/                    # Generated plots and maps
    ├── tables/                     # Regression tables (LaTeX)
    └── logs/                       # Execution logs
```

---

## Folder Descriptions

### Root Level Files

| File | Purpose | When to Use |
|------|---------|-------------|
| `README.md` | Main project documentation | First file to read; overview of project |
| `00_setup.R` | Environment setup | Run once before starting analysis |
| `run_all.R` | Master pipeline script | Run complete analysis from start to finish |
| `.gitignore` | Version control exclusions | Automatically used by git |

### 01_data_processing/

**Purpose**: Transform raw data into clean, standardized formats

**Structure**: Organized by data type/source

**Subdirectories**:

| Folder | Contents | Input | Output |
|--------|----------|-------|--------|
| `climate/` | Temperature & precipitation processing | Raw climate files | Processed climate data (.rds) |
| `green_economy/` | Patents, employment, energy projects | Raw RAIS, INPI, ANNEL data | Green economy indicators (.rds) |
| `geography/` | Municipality identifiers & coordinates | IBGE shapefiles | Geographic data (.rds) |
| `firms/` | Corporate registry data | Quadro Societário files | Firm-level data (.rds) |
| `controls/` | Education, disasters, other controls | Census, disaster databases | Control variables (.rds) |

**File naming pattern**:
- `processamento_dados_*.R` - Main data processing scripts
- `dados_*.R` - Alternative/supplementary processing

**Key considerations**:
- Scripts can be run in parallel within each subfolder
- Each script reads from `data/raw/` and writes to `data/processed/`
- Check raw data file paths at the beginning of each script

### 02_data_merge/

**Purpose**: Integrate processed datasets into analysis-ready files

**Scripts**:

| Script | Purpose | Input | Output |
|--------|---------|-------|--------|
| `merge_weather_green_outcomes.R` | Merge aggregated (municipality-year) data | Multiple .rds from `data/processed/` | `data/merged/analysis_data_agregado.rds` |
| `merge_weather_green_outcomes_micro.R` | Merge micro (firm/individual-year) data | Multiple .rds from `data/processed/` | `data/merged/analysis_data_micro.rds` |

**Key considerations**:
- Run after ALL Step 1 scripts are complete
- Performs data quality checks (duplicates, missing values)
- Creates lagged variables and transformations

### 03_analysis/

**Purpose**: Statistical analysis and econometric modeling

**Subdirectories**:

#### `descriptive/`
- **Purpose**: Generate summary statistics and exploratory analysis
- **Scripts**: 1 script (`descriptive_statistics.R`)
- **Output**: LaTeX tables with means, standard deviations, correlations

#### `climate_shocks/`
- **Purpose**: Estimate impact of climate shocks on green innovation
- **Scripts**: 6 scripts with different specifications
- **Main script**: `long_shock.R` (baseline specification)
- **Variants**:
  - `long_shock_absolut.R` - Absolute values
  - `long_shock_per_capita.R` - Per capita outcomes
  - `long_shock_robustness_1.R` - Robustness check 1
  - `long_shock_robustness_2.R` - Robustness check 2
  - `climate_shocks_analysis.R` - Shock definitions and summary
- **Output**: Regression tables, coefficient plots, event studies

#### `econometric_models/stata/`
- **Purpose**: Stata regression specifications
- **Scripts**: 3 .do files
  - `reg.do` - Main regressions (aggregated level)
  - `reg_micro.do` - Micro-level regressions
  - `reg_robustness.do` - Alternative specifications
- **Output**: Regression tables (.tex), log files

#### `econometric_models/stata_commands/`
- **Purpose**: Custom Stata commands for specialized estimators
- **Files**: 3 .ado files
  - `ols_spatial_HAC.ado` - Spatial HAC-robust standard errors
  - `reg2hdfespatial.ado` - High-dimensional FE with spatial corrections
  - `DCdensity.ado` - McCrary density discontinuity test
- **Usage**: Add to Stata ado path before running .do files

### 04_visualization/

**Purpose**: Generate publication-quality figures

**Scripts**:

| Script | Purpose | Output |
|--------|---------|--------|
| `weather_maps.R` | Maps of temperature/precipitation patterns | Geographic maps (.png) |
| `weather_time_series.R` | Time series plots of climate variables | Time series plots (.png) |
| `grafico_patent.R` | Patent trends and distributions | Patent figures (.png) |
| `grafico_rais.R` | Green employment trends | Employment figures (.png) |
| `grafico_socio.R` | Socioeconomic indicators | Socioeconomic plots (.png) |
| `figura_data_patente.R` | Patent data visualization | Patent data figures (.png) |

**Output format**: PNG files (300 dpi, publication-ready)

**Key considerations**:
- Scripts use `ggplot2` for graphics
- Outputs saved to `output/figures/`
- Can be run in any order

### archive/

**Purpose**: Store deprecated or backup files that are no longer actively used

**Contents**:
- Old versions of scripts
- Recovered files from crashes
- Temporary Stata swap files

**Policy**:
- Files here are tracked by git but not used in analysis
- Periodically review and permanently delete if no longer needed

### docs/

**Purpose**: Comprehensive project documentation

**Files**:

| File | Purpose | Audience |
|------|---------|----------|
| `project_structure.md` | This file - explains folder organization | Developers, collaborators |
| `workflow.md` | Step-by-step execution guide | New users, reproducers |
| `data_dictionary.md` | Variable definitions and sources | All users, manuscript readers |

**Maintenance**: Update docs whenever:
- New folders/files are added
- Workflow changes
- New variables created

### data/ (Not in Git)

**Purpose**: Store all data files (raw and processed)

**Structure**:
```
data/
├── raw/                  # Original data from sources (never modified)
│   ├── climate/
│   ├── patents/
│   ├── rais/
│   └── ...
├── processed/            # Cleaned data from Step 1 scripts
│   ├── temperatura_maxima.rds
│   ├── patentes_verdes.rds
│   └── ...
└── merged/               # Analysis-ready data from Step 2 scripts
    ├── analysis_data_agregado.rds
    └── analysis_data_micro.rds
```

**Important**:
- This folder is listed in `.gitignore` (not tracked)
- Users must obtain and place raw data manually
- Processed data can be regenerated by running scripts

### output/ (Not in Git)

**Purpose**: Store all analysis outputs

**Structure**:
```
output/
├── figures/              # PNG files from visualization scripts
│   ├── weather_map_temperature.png
│   ├── patent_trends.png
│   └── ...
├── tables/               # LaTeX tables from analysis scripts
│   ├── descriptive_stats.tex
│   ├── long_shock_results.tex
│   └── ...
└── logs/                 # Execution logs
    ├── execution_log.txt
    └── stata_reg.log
```

**Important**:
- This folder is listed in `.gitignore` (not tracked)
- Outputs can be regenerated by running scripts
- Include relevant figures/tables in manuscripts

---

## File Naming Conventions

### General Patterns

**Scripts**:
- Portuguese: `processamento_dados_*.R` ("data processing")
- Portuguese: `grafico_*.R` ("graph/figure")
- English: `merge_*.R`, `descriptive_*.R`, `long_shock_*.R`

**Suffixes**:
- `*_micro`: Micro-level (firm/individual) analysis
- `*_agregado`: Aggregated (municipality) level
- `*_robustness_*`: Robustness check variants
- `*_lag*`: Lagged variable versions

**Data files**:
- Processed: `*.rds` (R data format)
- Stata: `*.dta`
- Tables: `*.tex` (LaTeX format)
- Figures: `*.png` (high resolution)

### Numbering System

Folders numbered to indicate execution order:
- `01_` - Run first
- `02_` - Run after 01
- `03_` - Run after 02
- `04_` - Run after 03

Within folders, no strict execution order unless dependencies exist (documented in `workflow.md`).

---

## Dependencies and Execution Flow

```
00_setup.R
    ↓
01_data_processing/
    ├── climate/  ────────┐
    ├── green_economy/  ──┤
    ├── geography/  ──────┤
    ├── firms/  ──────────┤
    └── controls/  ───────┤
                         ↓
02_data_merge/
    ├── merge_weather_green_outcomes.R  ────┐
    └── merge_weather_green_outcomes_micro.R ┤
                                            ↓
03_analysis/
    ├── descriptive/  ───────────┐
    ├── climate_shocks/  ────────┤
    └── econometric_models/  ────┤
                                ↓
04_visualization/
    └── (All scripts can run in parallel)
```

**Key points**:
- Step 1 scripts can run in parallel within subfolders
- Step 2 requires Step 1 to be complete
- Step 3 requires Step 2 to be complete
- Step 4 requires Step 3 for most figures (can run earlier for data exploration)

---

## Storage and Size Estimates

### Typical Storage Requirements

| Directory | Typical Size | Notes |
|-----------|--------------|-------|
| `data/raw/` | 1-50 GB | Depends on climate data resolution |
| `data/processed/` | 100 MB - 5 GB | Compressed .rds files |
| `data/merged/` | 50-500 MB | Analysis datasets |
| `output/figures/` | 10-100 MB | High-res PNG files |
| `output/tables/` | < 1 MB | LaTeX text files |
| `scripts/` | < 10 MB | R and Stata code |

**Total project size**: 2-60 GB (mostly raw data)

---

## Best Practices

### Adding New Scripts

1. **Determine appropriate folder**: Based on purpose (processing, merge, analysis, viz)
2. **Follow naming conventions**: Use descriptive names with appropriate prefixes/suffixes
3. **Add to `run_all.R`**: Include in the master pipeline if part of main workflow
4. **Document in `workflow.md`**: Describe what it does and when to run it
5. **Update this file**: Add to directory tree if new category

### Adding New Data Sources

1. **Place raw data in `data/raw/`**: Create subfolder if needed
2. **Create processing script in `01_data_processing/`**: Choose appropriate subfolder
3. **Output to `data/processed/`**: Use .rds format for R compatibility
4. **Document in `data_dictionary.md`**: Add variable definitions
5. **Update merge scripts**: Include in Step 2 integration

### Reorganizing Structure

If structure needs changes:
1. **Plan changes first**: Document proposed structure
2. **Use `git mv`**: Preserve file history when moving files
3. **Update all documentation**: README, workflow, this file
4. **Test full pipeline**: Ensure `run_all.R` still works
5. **Communicate with team**: Notify all collaborators

---

## Version Control Strategy

### What is Tracked (in Git)

✅ Scripts (.R, .do, .ado files)
✅ Documentation (.md files)
✅ Configuration files (.gitignore)
✅ README and setup scripts

### What is NOT Tracked

❌ Data files (data/)
❌ Output files (output/)
❌ Temporary files (.Rhistory, .RData, etc.)
❌ Large binary files

**Rationale**: Data and outputs can be regenerated from scripts; tracking them bloats repository size.

### Branch Strategy

- `main`: Stable, working code
- `claude/reorganize-project-structure-*`: Reorganization work (current)
- Feature branches: For new analyses or major changes

---

## Troubleshooting Structure Issues

### Problem: Can't find a script

**Solution**: Use this document or README to locate files by purpose, not by name

### Problem: Scripts fail with "file not found"

**Solution**:
1. Check that `data/raw/` contains required files
2. Verify working directory is project root
3. Check file paths in scripts (may need adjustment)

### Problem: Output directory doesn't exist

**Solution**: Run `00_setup.R` to create all output directories

### Problem: Git showing too many changes

**Solution**: Check that `.gitignore` is working; data/output files should not appear

---

## Future Improvements

Potential enhancements to structure:

1. **Add `tests/` folder**: Unit tests for data processing functions
2. **Add `config/` folder**: Centralized configuration files (paths, parameters)
3. **Add `notebooks/` folder**: Jupyter/R Markdown notebooks for exploration
4. **Add `manuscript/` folder**: LaTeX manuscript with auto-included tables/figures
5. **Modularize scripts**: Extract common functions to `utils/` or `R/` folder

---

## Contact and Contributions

For questions about the project structure:
- **Maintainer**: [Name]
- **Email**: [Email]
- **GitHub Issues**: Open issue for structure suggestions

When proposing structural changes:
1. Open a GitHub issue describing the proposed change
2. Explain the rationale (what problem does it solve?)
3. Provide example of new structure
4. Wait for discussion/approval before implementing

---

**Document version**: 1.0
**Last updated**: 2026-01-16
**Maintained by**: Project team
