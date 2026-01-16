# Green Innovation and Climate Shocks in Brazil

## Project Overview

This research project investigates the relationship between climate shocks and green innovation/economy development across Brazilian municipalities from 1940 to 2020. Using panel data econometric methods, we examine how temperature extremes, precipitation anomalies, and other weather events affect:

- Green patent applications
- Green sector employment (RAIS data)
- Renewable energy investments (solar and wind projects)
- Environmental sector firm formation

## Research Question

**How do climate shocks affect green innovation and the development of the green economy across Brazilian municipalities?**

## Key Features

- **Spatial econometrics**: Accounts for geographic dependence using Conley (spatial HAC) standard errors
- **Long-run effects**: Analyzes accumulated impacts of climate shocks over multiple years
- **Multi-level analysis**: Includes both municipality-aggregated and micro (firm/individual) level analyses
- **Robustness checks**: Multiple specifications and sensitivity analyses
- **High-dimensional fixed effects**: Municipality, year, and state-year fixed effects

## Project Structure

```
green_innovation/
│
├── 00_setup.R                      # Environment setup script
├── run_all.R                       # Master pipeline script
├── README.md                       # This file
├── .gitignore                      # Git ignore rules
│
├── 01_data_processing/             # Raw data cleaning and processing
│   ├── climate/                    # Climate and weather data
│   ├── green_economy/              # Green patents, employment, energy
│   ├── geography/                  # Municipality geographic data
│   ├── firms/                      # Firm-level data
│   └── controls/                   # Control variables (education, disasters)
│
├── 02_data_merge/                  # Data integration
│   ├── merge_weather_green_outcomes.R
│   └── merge_weather_green_outcomes_micro.R
│
├── 03_analysis/                    # Statistical analysis
│   ├── descriptive/                # Descriptive statistics
│   ├── climate_shocks/             # Climate shock impact analysis
│   └── econometric_models/         # Regression specifications
│       ├── stata/                  # Stata .do files
│       └── stata_commands/         # Custom Stata .ado files
│
├── 04_visualization/               # Figures and maps
│   ├── weather_maps.R
│   ├── weather_time_series.R
│   ├── grafico_patent.R
│   ├── grafico_rais.R
│   └── grafico_socio.R
│
├── archive/                        # Archived/deprecated files
│
└── docs/                           # Documentation
    ├── workflow.md                 # Detailed workflow guide
    ├── data_dictionary.md          # Variable definitions
    └── project_structure.md        # Folder structure documentation
```

## Data Sources

### Primary Data Sources

1. **Climate Data**
   - Temperature: Maximum daily temperature (1940-2020)
   - Precipitation: Daily precipitation levels (1940-2020)
   - Source: [Specify climate data source]

2. **Green Innovation Indicators**
   - **Patents**: Brazilian patent office (INPI) - green technology classifications
   - **RAIS Employment Data**: Formal employment in green economy sectors
   - **ANNEL**: Renewable energy projects (solar and wind installations)
   - **Green Sector Taxonomy**: FEBRABAN and custom CNAE-based classifications

3. **Geographic Data**
   - Municipality boundaries and identifiers
   - Latitude/longitude coordinates
   - Immediate region classifications
   - Source: IBGE (via `geobr` package)

4. **Control Variables**
   - Education: Census education data (municipality level)
   - Natural disasters: Disaster occurrence and severity
   - Firm characteristics: Corporate registry (Quadro Societário)

### Data Dictionary

See `docs/data_dictionary.md` for detailed variable definitions and data sources.

## Software Requirements

### R (version 4.0 or higher)

**Core packages:**
- `data.table` - Fast data manipulation
- `fixest` - High-dimensional fixed effects models
- `conleyreg` - Spatial HAC standard errors
- `geobr` - Brazilian geographic data
- `haven` - Stata file import/export

**Visualization:**
- `ggplot2` - Graphics
- `sf` - Spatial data visualization

**Full package list**: See `00_setup.R` for complete dependencies

### Stata (version 15 or higher)

**Custom commands included:**
- `ols_spatial_HAC.ado` - Spatial HAC-robust regressions
- `reg2hdfespatial.ado` - Spatial high-dimensional fixed effects
- `DCdensity.ado` - McCrary density discontinuity test

## Getting Started

### 1. Clone the Repository

```bash
git clone https://github.com/labreleon/green_innovation.git
cd green_innovation
```

### 2. Set Up the Environment

Open R and run:

```r
source("00_setup.R")
```

This will:
- Install all required R packages
- Create necessary output directories
- Load common functions and parameters

### 3. Prepare Your Data

Create the following directory structure and place your raw data files:

```
green_innovation/
├── data/
│   ├── raw/                  # Your raw data files go here
│   ├── processed/            # Processed data (created by scripts)
│   └── merged/               # Merged datasets (created by scripts)
```

**Note**: The `data/` directory is not tracked by git (see `.gitignore`). You must obtain and place the data files manually.

### 4. Run the Analysis Pipeline

#### Option A: Run the Complete Pipeline

```r
source("run_all.R")
```

This will execute all scripts in order:
1. Data processing (Step 1)
2. Data merging (Step 2)
3. Statistical analysis (Step 3)
4. Visualization (Step 4)

**Estimated runtime**: 2-4 hours (depending on data size and hardware)

#### Option B: Run Scripts Individually

You can run scripts step-by-step. See `docs/workflow.md` for the recommended execution order.

### 5. Run Stata Regressions

After completing the R pipeline, run the Stata specifications:

```stata
* Add custom ado files to your Stata path
adopath + "03_analysis/econometric_models/stata_commands"

* Run main regressions
do "03_analysis/econometric_models/stata/reg.do"
do "03_analysis/econometric_models/stata/reg_micro.do"
do "03_analysis/econometric_models/stata/reg_robustness.do"
```

## Workflow Overview

### Step 1: Data Processing (01_data_processing/)

Scripts clean and standardize raw data:

- **Climate data**: Process temperature and precipitation by municipality-year
- **Green economy data**: Clean patent, employment, and energy project data
- **Geographic data**: Prepare municipality identifiers and coordinates
- **Firm data**: Process corporate registry information
- **Control variables**: Prepare education and disaster data

**Output**: Processed `.rds` or `.dta` files in `data/processed/`

### Step 2: Data Merge (02_data_merge/)

Merge climate data with green economy outcomes:

- Aggregated level: Municipality-year panel
- Micro level: Firm-year or individual-year panel

**Output**: Analysis-ready datasets in `data/merged/`

### Step 3: Analysis (03_analysis/)

Statistical analysis and econometric models:

- **Descriptive statistics**: Summary tables and distributions
- **Climate shock analysis**: Estimate short-run and long-run effects
- **Econometric models**: Fixed effects regressions with spatial HAC standard errors

**Output**: Regression tables in `output/tables/`

### Step 4: Visualization (04_visualization/)

Generate publication-quality figures:

- Weather maps and time series
- Patent and employment trends
- Socioeconomic indicators

**Output**: Figures in `output/figures/`

## Key Methodological Features

### Climate Shock Definitions

- **Heat shock**: Temperature > 95th percentile of historical distribution
- **Cold shock**: Temperature < 5th percentile
- **Flood shock**: Precipitation > 95th percentile
- **Drought shock**: Precipitation < 5th percentile

Shocks are calculated relative to municipality-specific historical baselines (1940-2020).

### Econometric Specification

**Main specification**:

```
Y_it = β₁ × ClimateShock_it + β₂ × ClimateShock_it-1 + ... + β_k × ClimateShock_it-k
       + X_it × γ + α_i + δ_t + ε_it
```

Where:
- `Y_it`: Green innovation outcome (patents, employment, energy projects)
- `ClimateShock_it`: Current and lagged climate shock measures
- `X_it`: Control variables (education, disasters, etc.)
- `α_i`: Municipality fixed effects
- `δ_t`: Year fixed effects (or state-year fixed effects)
- `ε_it`: Error term (clustered at municipality level with spatial correction)

**Standard errors**: Conley (spatial HAC) with distance cutoff of [X km]

### Robustness Checks

Multiple specifications are estimated:

1. **Baseline**: Main specification (absolute values)
2. **Per capita**: Outcomes measured per 1,000 inhabitants
3. **Alternative lag structures**: Different numbers of lags
4. **Heterogeneity analysis**: By region, municipality size, etc.

See `03_analysis/climate_shocks/` for all specifications.

## Output Files

After running the pipeline, output files will be located in:

- **Figures**: `output/figures/`
  - Maps, time series plots, scatter plots
  - Format: PNG (high resolution, 300 dpi)

- **Tables**: `output/tables/`
  - Regression tables (LaTeX format)
  - Descriptive statistics tables

- **Logs**: `output/logs/`
  - Execution logs and script runtime information

## File Naming Conventions

### Scripts

- `processamento_dados_*`: Data processing scripts (Portuguese: "data processing")
- `merge_*`: Data merging scripts
- `long_shock_*`: Long-run climate shock analysis
- `grafico_*`: Visualization scripts (Portuguese: "graph")

### Suffixes

- `*_micro`: Micro-level (firm/individual) analysis
- `*_agregado`: Aggregated (municipality) level
- `*_robustness_*`: Robustness check specifications

## Troubleshooting

### Common Issues

**1. Missing packages**

```r
# Re-run setup to install missing packages
source("00_setup.R")
```

**2. Data files not found**

Ensure your raw data files are in `data/raw/` and match the expected file names in the processing scripts.

**3. Memory issues**

For large datasets, increase R memory:

```r
# Increase memory limit (Windows)
memory.limit(size = 16000)  # 16 GB

# Use data.table for efficiency
library(data.table)
setDTthreads(threads = 4)  # Adjust based on your CPU
```

**4. Stata ado files not found**

Make sure to add the custom Stata commands to your ado path:

```stata
adopath + "03_analysis/econometric_models/stata_commands"
```

## Contributing

This is a research project. For questions or suggestions:

1. Open an issue on GitHub
2. Contact the research team: [Add contact information]

## Citation

If you use this code or methodology, please cite:

```
[Author names]. ([Year]). "Green Innovation and Climate Shocks in Brazil."
[Journal/Working Paper]. [DOI or URL]
```

## License

[Specify license - e.g., MIT, GPL-3, or proprietary/academic use only]

## Acknowledgments

This research uses data from:
- IBGE (Brazilian Institute of Geography and Statistics)
- INPI (Brazilian Patent Office)
- Ministry of Labor (RAIS employment data)
- ANNEL (Brazilian Electric Energy Agency)
- [Other data sources]

Funding acknowledgments:
- [List funding sources if applicable]

## Contact

- **Principal Investigator**: [Name] ([email])
- **Research Team**: [Names and emails]
- **GitHub**: https://github.com/labreleon/green_innovation

---

**Last updated**: 2026-01-16
