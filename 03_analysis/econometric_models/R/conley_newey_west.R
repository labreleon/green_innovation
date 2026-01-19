# ============================================================================
# CONLEY SPATIAL HAC STANDARD ERRORS (Replicates ols_spatial_HAC.ado)
# ============================================================================
#
# This function replicates EXACTLY the Stata ols_spatial_HAC.ado behavior:
# 1. Calculates spatial correlation (Conley 1999)
# 2. Adds serial correlation (Newey-West 1987)
# 3. Returns HAC variance-covariance matrix
#
# Based on ols_spatial_HAC.ado by Solomon Hsiang
# ============================================================================

conley_newey_west_hac <- function(Y, X, lat, lon, time, panel,
                                   dist_cutoff = 250,
                                   lag_cutoff = 6,
                                   bartlett_spatial = FALSE) {

  # =========================================================================
  # STEP 1: OLS REGRESSION (WITHOUT CONSTANT)
  # =========================================================================

  # Remove NAs
  complete_cases <- complete.cases(Y, X, lat, lon, time, panel)
  Y <- Y[complete_cases]
  X <- as.matrix(X[complete_cases, ])
  lat <- lat[complete_cases]
  lon <- lon[complete_cases]
  time <- time[complete_cases]
  panel <- panel[complete_cases]

  n <- length(Y)
  k <- ncol(X)

  # OLS coefficients
  invXX <- solve(t(X) %*% X)
  b <- invXX %*% t(X) %*% Y

  # Residuals
  e <- Y - X %*% b

  # Initialize variance-covariance matrix
  XeeX <- matrix(0, nrow = k, ncol = k)

  # =========================================================================
  # STEP 2: SPATIAL CORRELATION (Conley 1999)
  # =========================================================================

  cat("Calculating spatial correlation (Conley)...\n")

  timeUnique <- unique(time)
  Ntime <- length(timeUnique)

  for (ti in 1:Ntime) {
    if (ti %% 5 == 0) cat("  Processing time period", ti, "of", Ntime, "\n")

    # Select observations in time period ti
    rows_ti <- time == timeUnique[ti]
    Y1 <- Y[rows_ti]
    X1 <- X[rows_ti, , drop = FALSE]
    lat1 <- lat[rows_ti]
    lon1 <- lon[rows_ti]
    e1 <- e[rows_ti]

    n1 <- length(Y1)

    # Loop over all observations in time period ti
    for (i in 1:n1) {

      # Calculate spatial distance in kilometers
      # 1 degree latitude = 111 km
      # 1 degree longitude = 111 km * cos(latitude)
      lat_scale <- 111
      lon_scale <- cos(lat1[i] * pi / 180) * 111

      distance_i <- sqrt((lat_scale * (lat1[i] - lat1))^2 +
                        (lon_scale * (lon1[i] - lon1))^2)

      # Uniform kernel (default)
      window_i <- (distance_i <= dist_cutoff) * 1.0

      # Bartlett kernel (optional)
      if (bartlett_spatial) {
        weight_i <- pmax(0, 1 - distance_i / dist_cutoff)
        window_i <- window_i * weight_i
      }

      # Construct X'e'eX for this observation
      # XeeXh = (X[i,]' * e[i]) * (e' * diag(window_i)) * X
      XeeXh <- (X1[i, , drop = FALSE] %*% t(X1[i, , drop = FALSE]) * e1[i]) %*%
               (matrix(e1 * window_i, nrow = 1) %*% X1)

      # Add to total matrix
      XeeX <- XeeX + XeeXh
    }
  }

  # Save spatial-only VCE
  XeeX_spatial <- XeeX / n

  # =========================================================================
  # STEP 3: SERIAL CORRELATION (Newey-West 1987)
  # =========================================================================

  cat("Calculating serial correlation (Newey-West)...\n")

  panelUnique <- unique(panel)
  Npanel <- length(panelUnique)

  for (pi in 1:Npanel) {
    if (pi %% 100 == 0) cat("  Processing panel", pi, "of", Npanel, "\n")

    # Select observations in panel pi
    rows_pi <- panel == panelUnique[pi]
    Y1 <- Y[rows_pi]
    X1 <- X[rows_pi, , drop = FALSE]
    time1 <- time[rows_pi]
    e1 <- e[rows_pi]

    n1 <- length(Y1)

    # Loop over all observations in panel pi
    for (t in 1:n1) {

      # Calculate temporal distance
      time_diff <- abs(time1[t] - time1)

      # Bartlett weight for serial correlation
      # CRITICAL: Uses lag_cutoff+1 in denominator (Stata convention)
      weight <- pmax(0, 1 - time_diff / (lag_cutoff + 1))

      # Window: include if time_diff <= lag_cutoff
      window_t <- (time_diff <= lag_cutoff) * weight

      # CRITICAL: Exclude diagonal to avoid double-counting
      # (diagonal was already counted in spatial correlation step)
      window_t <- window_t * (time1[t] != time1)

      # Construct X'e'eX for this observation
      XeeXh <- (X1[t, , drop = FALSE] %*% t(X1[t, , drop = FALSE]) * e1[t]) %*%
               (matrix(e1 * window_t, nrow = 1) %*% X1)

      # Add to total matrix (already contains spatial correlation)
      XeeX <- XeeX + XeeXh
    }
  }

  # Final HAC VCE (spatial + serial)
  XeeX_spatial_HAC <- XeeX / n

  # =========================================================================
  # STEP 4: COMPUTE FINAL VARIANCE-COVARIANCE MATRIX
  # =========================================================================

  V_spatial_HAC <- invXX %*% XeeX_spatial_HAC %*% invXX / n

  # Ensure symmetry (may not be exact due to rounding)
  V_spatial_HAC <- (V_spatial_HAC + t(V_spatial_HAC)) / 2

  # Standard errors
  se_HAC <- sqrt(diag(V_spatial_HAC))

  # =========================================================================
  # RETURN RESULTS
  # =========================================================================

  cat("Done!\n")

  return(list(
    coefficients = as.vector(b),
    vcov = V_spatial_HAC,
    se = se_HAC,
    residuals = e,
    n_obs = n,
    k_vars = k
  ))
}
