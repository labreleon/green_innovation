# ==============================================================================
# Master Analysis Pipeline
# ==============================================================================
# Green Innovation and Climate Shocks in Brazil
#
# This script runs the complete analysis pipeline from raw data processing
# to final figures and tables for publication.
#
# IMPORTANT: Before running this script:
# 1. Ensure all raw data files are in the data/raw/ directory
# 2. Run 00_setup.R first, or set run_setup = TRUE below
# 3. Adjust the pipeline steps as needed (set steps to TRUE/FALSE)
#
# Estimated run time: 2-4 hours (depending on data size and hardware)
# ==============================================================================

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Set which parts of the pipeline to run
run_setup <- TRUE                # Run setup script first
run_data_processing <- TRUE      # Step 1: Process raw data
run_data_merge <- TRUE           # Step 2: Merge datasets
run_analysis <- TRUE             # Step 3: Run statistical analysis
run_visualization <- TRUE        # Step 4: Generate figures

# Logging
log_output <- TRUE
start_time <- Sys.time()

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

# Function to run a script with error handling and logging
run_script <- function(script_path, description = "") {
  script_name <- basename(script_path)

  cat("\n")
  cat(rep("=", 80), "\n", sep = "")
  if (description != "") {
    cat("Running:", description, "\n")
  }
  cat("Script:", script_name, "\n")
  cat(rep("=", 80), "\n", sep = "")

  script_start <- Sys.time()

  tryCatch({
    source(script_path, echo = FALSE)
    script_end <- Sys.time()
    elapsed <- difftime(script_end, script_start, units = "mins")

    cat("\n✓ Completed:", script_name, "\n")
    cat("  Time elapsed:", round(elapsed, 2), "minutes\n")

    return(TRUE)
  },
  error = function(e) {
    cat("\n✗ ERROR in", script_name, ":\n")
    cat("  ", conditionMessage(e), "\n")
    cat("  Pipeline stopped.\n")
    return(FALSE)
  })
}

# ==============================================================================
# STEP 0: SETUP
# ==============================================================================

if (run_setup) {
  cat("\n")
  cat(rep("#", 80), "\n", sep = "")
  cat("# STEP 0: ENVIRONMENT SETUP\n")
  cat(rep("#", 80), "\n", sep = "")

  if (!run_script("00_setup.R", "Setting up R environment")) {
    stop("Setup failed. Please check error messages above.")
  }
} else {
  cat("\nSkipping setup. Make sure you've run 00_setup.R manually.\n")
}

# ==============================================================================
# STEP 1: DATA PROCESSING
# ==============================================================================

if (run_data_processing) {
  cat("\n")
  cat(rep("#", 80), "\n", sep = "")
  cat("# STEP 1: DATA PROCESSING\n")
  cat(rep("#", 80), "\n", sep = "")

  cat("\n>>> 1.1 Climate Data Processing <<<\n")

  # Climate data scripts
  climate_scripts <- c(
    "processamento_dados_temperatura_maxima.R",
    "processamento_dados_temperatura_maxima_micro.R",
    "processamento_dados_precipitacao.R",
    "processamento_dados_precipitacao_micro.R",
    "processamento_dados_climaticos.R",
    "processamento_dados_climaticos_combinados.R",
    "processamento_dados_climaticos_combinados_micro.R",
    "processamento_dados_climaticos_educacionais.R",
    "processamento_dados_climaticos_educacionais_micro.R"
  )

  for (script in climate_scripts) {
    script_path <- file.path("01_data_processing/climate", script)
    if (file.exists(script_path)) {
      if (!run_script(script_path)) stop("Pipeline halted.")
    } else {
      cat("Warning: Script not found:", script_path, "\n")
    }
  }

  cat("\n>>> 1.2 Green Economy Data Processing <<<\n")

  # Green economy scripts
  green_econ_scripts <- c(
    "processamento_dados_rais_economia_verde.R",
    "processamento_dados_patentes.R",
    "processamento_legenda_economia_verde.R",
    "processamento_taxonomia_verde_febrabran.R",
    "processamento_annel.R",
    "dados_rais.R",
    "tabela_cnae_ambiental.R"
  )

  for (script in green_econ_scripts) {
    script_path <- file.path("01_data_processing/green_economy", script)
    if (file.exists(script_path)) {
      if (!run_script(script_path)) stop("Pipeline halted.")
    }
  }

  cat("\n>>> 1.3 Geography Data Processing <<<\n")

  # Geography scripts
  geography_scripts <- c(
    "processamento_dados_municipios.R",
    "processamento_dados_mun_lat_lon.R",
    "processamento_dados_regiao_imediata.R"
  )

  for (script in geography_scripts) {
    script_path <- file.path("01_data_processing/geography", script)
    if (file.exists(script_path)) {
      if (!run_script(script_path)) stop("Pipeline halted.")
    }
  }

  cat("\n>>> 1.4 Firm Data Processing <<<\n")

  # Firm scripts
  firm_scripts <- c(
    "processamento_dados_quadro_societario.R",
    "dados_quadro_societario.R"
  )

  for (script in firm_scripts) {
    script_path <- file.path("01_data_processing/firms", script)
    if (file.exists(script_path)) {
      if (!run_script(script_path)) stop("Pipeline halted.")
    }
  }

  cat("\n>>> 1.5 Control Variables Processing <<<\n")

  # Control variable scripts
  control_scripts <- c(
    "processamento_dados_censo_educacao.R",
    "processamento_dados_censo_educacao_micro.R",
    "processamento_desastres.R",
    "prepare_control_variables.R"
  )

  for (script in control_scripts) {
    script_path <- file.path("01_data_processing/controls", script)
    if (file.exists(script_path)) {
      if (!run_script(script_path)) stop("Pipeline halted.")
    }
  }

  cat("\n✓ Step 1 Complete: All data processing scripts executed\n")

} else {
  cat("\nSkipping Step 1: Data Processing\n")
}

# ==============================================================================
# STEP 2: DATA MERGE
# ==============================================================================

if (run_data_merge) {
  cat("\n")
  cat(rep("#", 80), "\n", sep = "")
  cat("# STEP 2: DATA MERGE\n")
  cat(rep("#", 80), "\n", sep = "")

  merge_scripts <- c(
    "merge_weather_green_outcomes.R",
    "merge_weather_green_outcomes_micro.R"
  )

  for (script in merge_scripts) {
    script_path <- file.path("02_data_merge", script)
    if (file.exists(script_path)) {
      if (!run_script(script_path)) stop("Pipeline halted.")
    }
  }

  cat("\n✓ Step 2 Complete: Data merged successfully\n")

} else {
  cat("\nSkipping Step 2: Data Merge\n")
}

# ==============================================================================
# STEP 3: ANALYSIS
# ==============================================================================

if (run_analysis) {
  cat("\n")
  cat(rep("#", 80), "\n", sep = "")
  cat("# STEP 3: STATISTICAL ANALYSIS\n")
  cat(rep("#", 80), "\n", sep = "")

  cat("\n>>> 3.1 Descriptive Statistics <<<\n")

  desc_script <- "03_analysis/descriptive/descriptive_statistics.R"
  if (file.exists(desc_script)) {
    if (!run_script(desc_script)) stop("Pipeline halted.")
  }

  cat("\n>>> 3.2 Climate Shock Analysis <<<\n")

  shock_scripts <- c(
    "climate_shocks_analysis.R",
    "long_shock.R",
    "long_shock_absolut.R",
    "long_shock_per_capita.R",
    "long_shock_robustness_1.R",
    "long_shock_robustness_2.R"
  )

  for (script in shock_scripts) {
    script_path <- file.path("03_analysis/climate_shocks", script)
    if (file.exists(script_path)) {
      if (!run_script(script_path)) stop("Pipeline halted.")
    }
  }

  cat("\n>>> 3.3 Econometric Models (Stata) <<<\n")
  cat("Note: Stata scripts must be run separately in Stata\n")
  cat("  - 03_analysis/econometric_models/stata/reg.do\n")
  cat("  - 03_analysis/econometric_models/stata/reg_micro.do\n")
  cat("  - 03_analysis/econometric_models/stata/reg_robustness.do\n")
  cat("\nMake sure Stata ado files are in the Stata ado path:\n")
  cat("  - 03_analysis/econometric_models/stata_commands/*.ado\n")

  cat("\n✓ Step 3 Complete: Analysis finished\n")

} else {
  cat("\nSkipping Step 3: Analysis\n")
}

# ==============================================================================
# STEP 4: VISUALIZATION
# ==============================================================================

if (run_visualization) {
  cat("\n")
  cat(rep("#", 80), "\n", sep = "")
  cat("# STEP 4: VISUALIZATION\n")
  cat(rep("#", 80), "\n", sep = "")

  viz_scripts <- c(
    "weather_maps.R",
    "weather_time_series.R",
    "grafico_patent.R",
    "grafico_rais.R",
    "grafico_socio.R",
    "figura_data_patente.R"
  )

  for (script in viz_scripts) {
    script_path <- file.path("04_visualization", script)
    if (file.exists(script_path)) {
      if (!run_script(script_path)) stop("Pipeline halted.")
    }
  }

  cat("\n✓ Step 4 Complete: All visualizations generated\n")

} else {
  cat("\nSkipping Step 4: Visualization\n")
}

# ==============================================================================
# PIPELINE COMPLETE
# ==============================================================================

end_time <- Sys.time()
total_time <- difftime(end_time, start_time, units = "hours")

cat("\n")
cat(rep("=", 80), "\n", sep = "")
cat(rep("=", 80), "\n", sep = "")
cat("\n")
cat("   ✓✓✓ ANALYSIS PIPELINE COMPLETE ✓✓✓\n")
cat("\n")
cat(rep("=", 80), "\n", sep = "")
cat(rep("=", 80), "\n", sep = "")
cat("\n")
cat("Started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("Finished at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("Total time:", round(total_time, 2), "hours\n")
cat("\n")
cat("Output files saved to:\n")
cat("  - Figures: output/figures/\n")
cat("  - Tables: output/tables/\n")
cat("  - Logs: output/logs/\n")
cat("\n")
cat("Next steps:\n")
cat("  1. Review output figures and tables\n")
cat("  2. Run Stata regression scripts manually\n")
cat("  3. Compile results for manuscript\n")
cat("\n")
cat(rep("=", 80), "\n", sep = "")
cat("\n")
