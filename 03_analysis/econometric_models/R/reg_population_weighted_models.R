# ============================================================================
# POPULATION-WEIGHTED SHORT-RUN PANEL ESTIMATES (2000-2020)
# Wrapper script to run weighted versions of existing regression pipelines
# ============================================================================

# Use population weights in downstream scripts when available.
# Each sourced script will pick this up and apply weights to all estimations.
population_weight_var <- "population"

cat("Running population-weighted models using weight:", population_weight_var, "\n")

source("03_analysis/econometric_models/R/reg_employment_shortrun_with_state_trend.R")
source("03_analysis/econometric_models/R/reg_firms_municipal_cluster.R")
source("03_analysis/econometric_models/R/reg_patents_municipal_cluster.R")
