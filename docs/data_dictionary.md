# Data Dictionary

## Overview

This document provides detailed definitions of all variables used in the Green Innovation and Climate Shocks project. Variables are organized by data source and category.

---

## Table of Contents

1. [Geographic Identifiers](#geographic-identifiers)
2. [Time Variables](#time-variables)
3. [Climate Variables](#climate-variables)
4. [Green Economy Outcomes](#green-economy-outcomes)
5. [Control Variables](#control-variables)
6. [Constructed Variables](#constructed-variables)

---

## Geographic Identifiers

| Variable | Type | Description | Source | Unit | Range/Values |
|----------|------|-------------|--------|------|--------------|
| `cod_municipio` | Integer | 7-digit IBGE municipality code | IBGE | - | 1100015-5300108 |
| `nome_municipio` | String | Municipality name | IBGE | - | Text |
| `cod_uf` | Integer | 2-digit state code | IBGE | - | 11-53 |
| `uf` | String | State abbreviation (2 letters) | IBGE | - | AC, AL, ..., TO |
| `regiao` | String | Geographic region | IBGE | - | Norte, Nordeste, Sudeste, Sul, Centro-Oeste |
| `cod_regiao_imediata` | Integer | Immediate region code | IBGE | - | Integer |
| `nome_regiao_imediata` | String | Immediate region name | IBGE | - | Text |
| `latitude` | Numeric | Municipality centroid latitude | IBGE/geobr | Degrees | -33.75 to 5.27 |
| `longitude` | Numeric | Municipality centroid longitude | IBGE/geobr | Degrees | -73.99 to -32.39 |
| `area_km2` | Numeric | Municipality area | IBGE | km² | Continuous |

**Notes**:
- Municipality codes follow IBGE's 7-digit standard
- Geographic coordinates are based on municipality centroids
- State codes: 11=Rondônia, 12=Acre, ..., 53=Distrito Federal

---

## Time Variables

| Variable | Type | Description | Source | Unit | Range/Values |
|----------|------|-------------|--------|------|--------------|
| `year` | Integer | Calendar year | - | Year | 1940-2020 |
| `month` | Integer | Month (if applicable) | - | Month | 1-12 |
| `date` | Date | Full date (if applicable) | - | Date | YYYY-MM-DD |
| `year_baseline` | Integer | Baseline year for shock calculations | Constructed | Year | 1940-1970 |

**Notes**:
- Most analyses use annual (year) data
- Some climate data available at daily/monthly frequency
- Baseline period typically 1940-1970 for calculating climate anomalies

---

## Climate Variables

### Temperature

| Variable | Type | Description | Source | Unit | Range/Values |
|----------|------|-------------|--------|------|--------------|
| `temp_max` | Numeric | Maximum daily temperature | [Climate data source] | °C | -10 to 45 |
| `temp_max_annual` | Numeric | Annual average of maximum temperature | Constructed | °C | 15 to 35 |
| `temp_max_baseline` | Numeric | Historical baseline (1940-1970 avg) | Constructed | °C | 15 to 35 |
| `temp_max_anomaly` | Numeric | Temperature deviation from baseline | Constructed | °C | -10 to 10 |
| `temp_max_percentile` | Numeric | Percentile in historical distribution | Constructed | Percentile | 0-100 |

### Precipitation

| Variable | Type | Description | Source | Unit | Range/Values |
|----------|------|-------------|--------|------|--------------|
| `precip` | Numeric | Daily precipitation | [Climate data source] | mm | 0 to 500 |
| `precip_annual` | Numeric | Annual total precipitation | Constructed | mm | 0 to 5000 |
| `precip_baseline` | Numeric | Historical baseline (1940-1970 avg) | Constructed | mm | 500 to 4000 |
| `precip_anomaly` | Numeric | Precipitation deviation from baseline | Constructed | mm | -2000 to 2000 |
| `precip_percentile` | Numeric | Percentile in historical distribution | Constructed | Percentile | 0-100 |

### Climate Shocks

| Variable | Type | Description | Source | Unit | Range/Values |
|----------|------|-------------|--------|------|--------------|
| `heat_shock` | Binary | 1 if temp > 95th percentile | Constructed | 0/1 | Binary |
| `cold_shock` | Binary | 1 if temp < 5th percentile | Constructed | 0/1 | Binary |
| `flood_shock` | Binary | 1 if precip > 95th percentile | Constructed | 0/1 | Binary |
| `drought_shock` | Binary | 1 if precip < 5th percentile | Constructed | 0/1 | Binary |
| `heat_shock_intensity` | Numeric | Magnitude of heat shock (if occurs) | Constructed | °C | 0 to 10 |
| `drought_shock_intensity` | Numeric | Magnitude of drought shock (if occurs) | Constructed | mm | -2000 to 0 |

**Notes**:
- Shocks defined relative to municipality-specific historical distribution (1940-2020)
- 95th/5th percentile thresholds calculated separately for each municipality
- Intensity measures the deviation beyond the threshold

---

## Green Economy Outcomes

### Patents

| Variable | Type | Description | Source | Unit | Range/Values |
|----------|------|-------------|--------|------|--------------|
| `patent_count` | Integer | Number of green patent applications | INPI | Count | 0 to 100+ |
| `patent_granted` | Integer | Number of green patents granted | INPI | Count | 0 to 100+ |
| `patent_green` | Integer | Patents in green technology classes | INPI | Count | 0 to 100+ |
| `patent_clean_energy` | Integer | Clean energy patents | INPI | Count | 0 to 50 |
| `patent_efficiency` | Integer | Energy efficiency patents | INPI | Count | 0 to 50 |
| `patent_transport` | Integer | Green transportation patents | INPI | Count | 0 to 50 |
| `patent_per_capita` | Numeric | Green patents per 1,000 inhabitants | Constructed | Per 1,000 | 0 to 10 |

**Notes**:
- Green patents identified using IPC (International Patent Classification) codes
- Classifications follow OECD ENV-TECH taxonomy
- Data from Brazilian Patent Office (INPI)

### Employment (RAIS)

| Variable | Type | Description | Source | Unit | Range/Values |
|----------|------|-------------|--------|------|--------------|
| `emp_green_total` | Integer | Total employment in green sectors | RAIS | Count | 0 to 100,000+ |
| `emp_renewable_energy` | Integer | Employment in renewable energy | RAIS | Count | 0 to 10,000+ |
| `emp_recycling` | Integer | Employment in recycling/waste mgmt | RAIS | Count | 0 to 10,000+ |
| `emp_sustainable_transport` | Integer | Employment in sustainable transport | RAIS | Count | 0 to 10,000+ |
| `emp_green_construction` | Integer | Employment in green construction | RAIS | Count | 0 to 10,000+ |
| `emp_green_share` | Numeric | Green employment / total employment | Constructed | Proportion | 0 to 1 |
| `emp_green_per_capita` | Numeric | Green jobs per 1,000 inhabitants | Constructed | Per 1,000 | 0 to 100 |

**Notes**:
- RAIS (Relação Anual de Informações Sociais) covers formal employment only
- Green sectors identified using CNAE (economic activity) codes
- Taxonomy based on FEBRABAN green economy classification

### Renewable Energy Projects (ANNEL)

| Variable | Type | Description | Source | Unit | Range/Values |
|----------|------|-------------|--------|------|--------------|
| `annel_projects_total` | Integer | Total renewable energy projects | ANNEL | Count | 0 to 500 |
| `annel_solar` | Integer | Solar energy projects | ANNEL | Count | 0 to 300 |
| `annel_wind` | Integer | Wind energy projects | ANNEL | Count | 0 to 200 |
| `annel_capacity_mw` | Numeric | Total installed capacity | ANNEL | MW | 0 to 5,000 |
| `annel_solar_capacity` | Numeric | Solar installed capacity | ANNEL | MW | 0 to 2,000 |
| `annel_wind_capacity` | Numeric | Wind installed capacity | ANNEL | MW | 0 to 3,000 |

**Notes**:
- ANNEL = Brazilian Electricity Regulatory Agency (Agência Nacional de Energia Elétrica)
- Data includes operational and approved projects
- Capacity measured in megawatts (MW)

### Firm Formation

| Variable | Type | Description | Source | Unit | Range/Values |
|----------|------|-------------|--------|------|--------------|
| `firm_green_new` | Integer | New green sector firms registered | Quadro Societário | Count | 0 to 1,000 |
| `firm_green_total` | Integer | Total active green sector firms | Quadro Societário | Count | 0 to 10,000 |
| `firm_green_share` | Numeric | Green firms / total firms | Constructed | Proportion | 0 to 1 |

**Notes**:
- Data from corporate registry (Quadro Societário)
- Firms classified by primary economic activity (CNAE)

---

## Control Variables

### Demographics

| Variable | Type | Description | Source | Unit | Range/Values |
|----------|------|-------------|--------|------|--------------|
| `population` | Integer | Total population | IBGE Census | Count | 100 to 10,000,000+ |
| `population_urban` | Integer | Urban population | IBGE Census | Count | 0 to 10,000,000+ |
| `population_rural` | Integer | Rural population | IBGE Census | Count | 0 to 500,000 |
| `urbanization_rate` | Numeric | Urban pop / total pop | Constructed | Proportion | 0 to 1 |
| `population_density` | Numeric | Population per km² | Constructed | People/km² | 0 to 10,000 |

### Education

| Variable | Type | Description | Source | Unit | Range/Values |
|----------|------|-------------|--------|------|--------------|
| `literacy_rate` | Numeric | Literacy rate (age 15+) | IBGE Census | Proportion | 0 to 1 |
| `years_schooling_avg` | Numeric | Average years of schooling | IBGE Census | Years | 0 to 15 |
| `enrollment_primary` | Integer | Primary school enrollment | INEP | Count | 0 to 100,000+ |
| `enrollment_secondary` | Integer | Secondary school enrollment | INEP | Count | 0 to 50,000+ |
| `enrollment_higher` | Integer | Higher education enrollment | INEP | Count | 0 to 100,000+ |

**Notes**:
- INEP = National Institute for Educational Studies and Research
- Census years: 1940, 1950, 1960, 1970, 1980, 1991, 2000, 2010, 2020
- Inter-census years use linear interpolation

### Economic Indicators

| Variable | Type | Description | Source | Unit | Range/Values |
|----------|------|-------------|--------|------|--------------|
| `gdp_total` | Numeric | Total GDP | IBGE | 1,000 BRL | 1,000 to 1,000,000,000 |
| `gdp_per_capita` | Numeric | GDP per capita | IBGE | BRL | 1,000 to 100,000 |
| `gdp_agriculture` | Numeric | Agricultural GDP | IBGE | 1,000 BRL | 0 to 10,000,000 |
| `gdp_industry` | Numeric | Industrial GDP | IBGE | 1,000 BRL | 0 to 100,000,000 |
| `gdp_services` | Numeric | Services GDP | IBGE | 1,000 BRL | 0 to 500,000,000 |
| `employment_total` | Integer | Total formal employment | RAIS | Count | 0 to 1,000,000+ |

**Notes**:
- GDP values in thousands of BRL (Brazilian Reais)
- Constant prices (base year: 2010)

### Natural Disasters

| Variable | Type | Description | Source | Unit | Range/Values |
|----------|------|-------------|--------|------|--------------|
| `disaster_occurred` | Binary | Any disaster in year | Disaster database | 0/1 | Binary |
| `disaster_flood` | Binary | Flood occurred | Disaster database | 0/1 | Binary |
| `disaster_drought` | Binary | Drought occurred | Disaster database | 0/1 | Binary |
| `disaster_landslide` | Binary | Landslide occurred | Disaster database | 0/1 | Binary |
| `disaster_count` | Integer | Number of disaster events | Disaster database | Count | 0 to 20 |
| `disaster_affected` | Integer | People affected by disasters | Disaster database | Count | 0 to 1,000,000 |

**Notes**:
- Disaster data from [specify source - e.g., EM-DAT, Civil Defense]
- Only disasters with significant impact recorded

---

## Constructed Variables

### Lagged Variables

| Variable | Type | Description | Construction | Unit | Range/Values |
|----------|------|-------------|--------------|------|--------------|
| `heat_shock_lag1` | Binary | Heat shock in t-1 | Lag of `heat_shock` | 0/1 | Binary |
| `heat_shock_lag2` | Binary | Heat shock in t-2 | Lag of `heat_shock` | 0/1 | Binary |
| `drought_shock_lag1` | Binary | Drought shock in t-1 | Lag of `drought_shock` | 0/1 | Binary |
| `patent_count_lag1` | Integer | Patents in t-1 | Lag of `patent_count` | Count | 0 to 100+ |

### Cumulative Effects

| Variable | Type | Description | Construction | Unit | Range/Values |
|----------|------|-------------|--------------|------|--------------|
| `heat_shock_5yr` | Integer | Heat shocks in past 5 years | Sum of lags 0-4 | Count | 0 to 5 |
| `drought_shock_5yr` | Integer | Drought shocks in past 5 years | Sum of lags 0-4 | Count | 0 to 5 |
| `climate_shock_cumulative` | Integer | Total climate shocks in past 5 years | Sum of all shock types | Count | 0 to 20 |

### Growth Rates

| Variable | Type | Description | Construction | Unit | Range/Values |
|----------|------|-------------|--------------|------|--------------|
| `patent_growth` | Numeric | Year-on-year patent growth | (Y_t - Y_t-1) / Y_t-1 | Proportion | -1 to 10 |
| `emp_green_growth` | Numeric | Green employment growth | (Y_t - Y_t-1) / Y_t-1 | Proportion | -1 to 10 |
| `gdp_growth` | Numeric | GDP growth rate | (Y_t - Y_t-1) / Y_t-1 | Proportion | -0.5 to 0.5 |

### Indices and Scores

| Variable | Type | Description | Construction | Unit | Range/Values |
|----------|------|-------------|--------------|------|--------------|
| `green_innovation_index` | Numeric | Composite green innovation score | Standardized PCA | Z-score | -3 to 3 |
| `climate_exposure_index` | Numeric | Long-run climate exposure | Cumulative shocks / years | Score | 0 to 1 |

**Notes**:
- Growth rates winsorized at 1st and 99th percentiles to remove outliers
- Indices standardized (mean=0, SD=1) for comparability

---

## Missing Data Codes

| Code | Meaning |
|------|---------|
| `NA` | Not available (standard R missing value) |
| `-999` | Not applicable (e.g., pre-measurement period) |
| `0` | True zero (e.g., no patents in municipality-year) |

**Important**: Distinguish between:
- **Missing (NA)**: Data should exist but is unavailable
- **Zero (0)**: Confirmed absence of the phenomenon
- **Not applicable (-999)**: Variable doesn't apply (e.g., renewable energy before 1990)

---

## Data Sources Summary

### Primary Sources

1. **IBGE (Brazilian Institute of Geography and Statistics)**
   - Website: https://www.ibge.gov.br/
   - Data: Geography, census, GDP, population

2. **INPI (Brazilian Patent Office)**
   - Website: https://www.gov.br/inpi/
   - Data: Patent applications and grants

3. **RAIS (Annual Social Information Report)**
   - Source: Ministry of Labor
   - Data: Formal employment by sector

4. **ANNEL (Brazilian Electricity Regulatory Agency)**
   - Website: https://www.gov.br/aneel/
   - Data: Renewable energy projects

5. **INEP (National Institute for Educational Studies)**
   - Website: https://www.gov.br/inep/
   - Data: Education statistics

6. **Climate Data**
   - Source: [Specify - e.g., INMET, CRU, ERA5, etc.]
   - Data: Temperature and precipitation

### Data Availability by Period

| Variable Category | Start Year | End Year | Frequency | Coverage |
|-------------------|------------|----------|-----------|----------|
| Climate | 1940 | 2020 | Daily/Monthly | All municipalities |
| Patents | 1990 | 2020 | Annual | Municipalities with applications |
| RAIS Employment | 1985 | 2020 | Annual | All municipalities |
| ANNEL Projects | 2000 | 2020 | Annual | Municipalities with projects |
| Census Data | 1940 | 2020 | Decennial | All municipalities |
| GDP | 1985 | 2020 | Annual | All municipalities |

---

## Variable Naming Conventions

**Prefixes**:
- `cod_`: Code/identifier
- `emp_`: Employment variable
- `patent_`: Patent-related variable
- `annel_`: Renewable energy variable
- `temp_`: Temperature variable
- `precip_`: Precipitation variable

**Suffixes**:
- `_total`: Sum across categories
- `_count`: Count of events
- `_per_capita`: Normalized by population
- `_share`: Proportion/percentage
- `_lag1`, `_lag2`: Lagged by 1, 2 years
- `_5yr`: 5-year cumulative/average
- `_baseline`: Historical baseline value
- `_anomaly`: Deviation from baseline
- `_growth`: Growth rate

---

## Notes on Data Quality

### Known Issues

1. **Municipality boundary changes**: Some municipalities were created/merged over the study period. We use consistent 2020 boundaries.

2. **Census interpolation**: Inter-census years use linear interpolation for population and demographic variables.

3. **Patent lag**: Patents have multi-year approval processes. Application year used, not grant year.

4. **RAIS coverage**: Only formal employment; informal sector not captured.

5. **Climate data gaps**: Some early years have sparse weather station coverage. Interpolation methods documented in processing scripts.

### Data Validation

Quality checks performed:
- Outlier detection (values > 3 SD from mean)
- Temporal consistency (implausible year-to-year changes)
- Geographic consistency (coordinates within Brazil boundaries)
- Merge validation (no duplicates, expected observation counts)

---

## Updates and Versioning

**Last updated**: 2026-01-16

**Data version**: 1.0

**Change log**:
- 2026-01-16: Initial data dictionary created
- [Future updates will be noted here]

---

## Contact

For questions about specific variables or data sources:
- **Technical questions**: [Email]
- **Data issues**: [Email]
- **General inquiries**: [Email]

---

**Note**: This data dictionary should be updated whenever new variables are added or definitions change. Document all modifications in the change log.
