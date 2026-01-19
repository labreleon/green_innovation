# ============================================================================
# MASTER SCRIPT - RUN ALL MUNICIPAL CLUSTERING REGRESSIONS
# Executa todas as análises com clustering municipal e efeitos fixos estado x ano
# ============================================================================

cat("\n")
cat(rep("=", 80), "\n", sep = "")
cat("RUNNING ALL MUNICIPAL CLUSTERING REGRESSIONS\n")
cat("Weather Shocks on Employment, Patents, and Firms (2000-2020)\n")
cat(rep("=", 80), "\n\n", sep = "")

# Set working directory (adjust if needed)
# setwd("/home/user/green_innovation/03_analysis/econometric_models/R")

# ============================================================================
# PART 1: EMPLOYMENT OUTCOMES
# ============================================================================

cat("\n")
cat(rep("#", 80), "\n", sep = "")
cat("# PART 1: EMPLOYMENT OUTCOMES\n")
cat(rep("#", 80), "\n", sep = "")

tryCatch({
  source("reg_employment_municipal_cluster.R")
  cat("\n✓ Employment regressions completed successfully!\n")
}, error = function(e) {
  cat("\n✗ Error in employment regressions:\n")
  cat(e$message, "\n")
})

# ============================================================================
# PART 2: PATENT OUTCOMES
# ============================================================================

cat("\n\n")
cat(rep("#", 80), "\n", sep = "")
cat("# PART 2: PATENT OUTCOMES\n")
cat(rep("#", 80), "\n", sep = "")

tryCatch({
  source("reg_patents_municipal_cluster.R")
  cat("\n✓ Patent regressions completed successfully!\n")
}, error = function(e) {
  cat("\n✗ Error in patent regressions:\n")
  cat(e$message, "\n")
})

# ============================================================================
# PART 3: FIRM OUTCOMES
# ============================================================================

cat("\n\n")
cat(rep("#", 80), "\n", sep = "")
cat("# PART 3: FIRM OUTCOMES\n")
cat(rep("#", 80), "\n", sep = "")

tryCatch({
  source("reg_firms_municipal_cluster.R")
  cat("\n✓ Firm regressions completed successfully!\n")
}, error = function(e) {
  cat("\n✗ Error in firm regressions:\n")
  cat(e$message, "\n")
})

# ============================================================================
# SUMMARY
# ============================================================================

cat("\n\n")
cat(rep("=", 80), "\n", sep = "")
cat("ALL REGRESSIONS COMPLETED\n")
cat(rep("=", 80), "\n\n", sep = "")

cat("Generated LaTeX tables:\n\n")
cat("EMPLOYMENT:\n")
cat("  1. table_employment_municipal_cluster_basic.tex\n")
cat("  2. table_employment_municipal_cluster_state_year.tex\n")
cat("  3. table_employment_municipal_cluster_comparison.tex\n\n")

cat("PATENTS:\n")
cat("  4. table_patents_municipal_cluster_comparison.tex\n\n")

cat("FIRMS:\n")
cat("  5. table_firms_municipal_cluster_comparison.tex\n\n")

cat(rep("=", 80), "\n", sep = "")
cat("METHOD: Municipal clustering (NO Conley spatial correction)\n")
cat("SPECIFICATIONS: (1) Year FE  (2) State×Year FE\n")
cat(rep("=", 80), "\n\n", sep = "")
