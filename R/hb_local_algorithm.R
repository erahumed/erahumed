propagate_ditch <- function(., lag) {
  ord <- match(.$cluster_id, lag$cluster_id)

  .$lag_accum_drain <- lag$accum_drain[ord]
  .$lag_accum_drain <- pmax(
    .$lag_accum_drain + (.$petp < 0 & .$lag_accum_drain > 0) * .$petp_m3_s,
    0)

  .$lag_accum_rain <- lag$accum_rain[ord]
  .$lag_accum_rain <- .$lag_accum_rain +
    (.$petp < 0 & .$lag_accum_rain > 0 & .$lag_accum_drain <= 0) * .$petp_m3_s

  .$lag_accum_rain_cm <- .$lag_accum_rain * 100 * erahumed:::s_per_day() / .$area
  .$lag_accum_rain <- pmax(.$lag_accum_rain, 0)

  .$Evap_mismatch <-
    (.$irrigation & .$draining & lag$accum_rain[ord] > 0 & .$petp < 0) *
    pmin(.$lag_accum_rain_cm + .$petp_cm, 0)

  .$corrected <- isTRUE(.$lag_accum_drain > 0 & (!.$draining | .$irrigation))

  idxs <- cumsum(.$corrected) > 0
  .$irrigation[idxs] <- lag$irrigation[ord][idxs]
  .$draining[idxs] <- lag$draining[ord][idxs]
  .$height_diff_cm[idxs] <- lag$height_diff_cm[ord][idxs]

  return(.)
}

compute_accum_pt1 <- function(.) {
  .$inflow <- .$irrigation * (
    .$draining * 5 + (1-.$draining) * (.$height_diff_cm - .$petp_cm)
  )
  .$outflow <- ifelse(.$lag_accum_drain > 0 & .$petp < 0,
                      pmax(.$inflow - .$height_diff_cm, 0),
                      pmax(.$inflow + .$petp_cm - .$height_diff_cm, 0)
  )
  .$outflow_m3 <- .$outflow * .$area / 100 / erahumed:::s_per_day()
  .$outflow_rain <- (.$petp > 0) * .$petp * .$area / 1000 / erahumed:::s_per_day()
  .$outflow_phys <- .$draining * (.$outflow_m3 - .$outflow_rain)
  .$outflow_flux <- .$irrigation * .$outflow_phys
  .$outflow_drain <- (1-.$irrigation) * .$outflow_phys
  .$outflow_drain <-
    .$corrected * .$lag_accum_drain + (1 - .$corrected) * .$outflow_drain
  .$outflow_rain <- .$outflow_rain + .$lag_accum_rain

  .
}
compute_real_outflows <- function(.) {
  ### Emptying clusters
  n_clusters <- length(.$cluster_id)
  # Permute the rows of ., so that clusters in the same ditch
  # are emptied in a random order every day

  sorted_clusters <- sample(n_clusters)

  capacity <- .$flowpoint[1]  # Extract this value outside of FOR

  cum_flows <- pmin(cumsum(c(0, .$outflow_rain[sorted_clusters])), capacity)
  .$real_outflow_rain[sorted_clusters] <- diff(cum_flows)
  capacity <- capacity - cum_flows[length(cum_flows)]

  cum_flows <- pmin(cumsum(c(0, .$outflow_drain[sorted_clusters])), capacity)
  .$real_outflow_drain[sorted_clusters] <- diff(cum_flows)
  capacity <- capacity - cum_flows[length(cum_flows)]

  cum_flows <- pmin(cumsum(c(0, .$outflow_flux[sorted_clusters])), capacity)
  .$real_outflow_flux[sorted_clusters] <- diff(cum_flows)
  #capacity <- capacity - cum_flows[length(cum_flows)]

  .$outflow_m3 <- .$real_outflow_rain + .$real_outflow_drain + .$real_outflow_flux
  .$outflow <- .$outflow_m3 * erahumed:::s_per_day() * 100 / .$area

  .
}
compute_accum_values <- function(.) {
  .$accum_rain <- .$outflow_rain - .$real_outflow_rain
  .$accum_drain <- .$outflow_drain - .$real_outflow_drain
  .$accum_flux <- .$outflow_flux - .$real_outflow_flux

  .$accum_rain <- ifelse(.$accum_rain > 1e-5, .$accum_rain, 0)           # Required???
  .$accum_drain <- ifelse(.$accum_drain > 1e-5, .$accum_drain, 0)
  .$accum_flux <- ifelse(.$accum_flux > 1e-5, .$accum_flux, 0)

  .
}
compute_real_inflow <- function(.) {
  .$inflow <-
    (1 - .$irrigation) * (1 - .$draining) * (
      .$inflow
    ) +
    .$irrigation * (1 - .$draining) * (
      5 + (.$mm >= 11) * 5 + (.$lag_accum_rain <= 0) * (.$petp < 0) * .$petp_cm
    ) +
    .$irrigation * .$draining * (
      (.$accum_flux > 0) *(.$petp < 0) *(.$lag_accum_rain <= 0) *(-.$petp_cm) +
        (.$accum_flux <= 0) * 5
    )
  .$inflow <- ifelse(.$Evap_mismatch != 0, abs(.$Evap_mismatch), .$inflow)

  .
}

compute_accum <- function(.) {
  . <- compute_accum_pt1(.)
  . <- compute_real_outflows(.)
  . <- compute_accum_values(.)
  . <- compute_real_inflow(.)

  return(.)
}
