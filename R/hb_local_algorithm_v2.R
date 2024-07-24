compute_lags_order_v2 <- function(cluster_id, lag_cluster_id) {
  match(cluster_id, lag_cluster_id)
}

# We assume that the real height will be initialized to planned_height_ms
algo_v2 <- function(
    planned_height_m,
    cluster_is_in_flow,  # Could be equal to irrigation * draining
    real_height_m_lag,
    petp_m,
    area_m2,
    capacity,
    ideal_flow_rate = 5
    ) {


  res <- list()

  # Real height at the end of day without draining / irrigating
  real_height_m_wo <- real_height_m_lag + petp_m

  ideal_inflow <- ifelse(cluster_is_in_flow,
                         ideal_flow_rate,
                         pmax(real_height_m_wo - planned_height_m)
                         )

  ideal_outflow <- ifelse(cluster_is_in_flow,
                          ideal_flow_rate,
                          pmax(planned_height_m - real_height_m_wo)
                          )

  res$accum_drain <- lag_residual_drain
  res$accum_rain <- lag_residual_rain

  # In the case in which evapotranspiration prevails on precipitation (petp < 0)
  # we subtract the evaporated water from the accumulated drain water if there's
  # enough of this, otherwise from the accumulated rain water if there's enough
  # of this.
  #
  # TODO: here the water balance is not guaranteed to check out. For example
  # lag$accum_drain = .5, lag$accum_rain = 1.5, and petp = -1
  # we end up with
  # res$lag_accum_drain = 0, res$lag_accum_rain = .5.,
  # so we have subtracted 1.5 in total

  # Subtract from residual water only if ETP prevails on P.
  # We switch the sign of petp for convenience.
  # TODO: not equivalent to the original defs.
  petp_m3_s <- -(petp_m3_s < 0) * petp_m3_s
  petp_m3_s_drain <- pmin(petp_m3_s, lag_residual_drain)
  petp_m3_s_rain <- pmin(petp_m3_s-petp_m3_s_drain, lag_residual_rain)

  res$accum_drain <- res$accum_drain - petp_m3_s_drain
  res$accum_rain <- res$accum_rain - petp_m3_s_rain
  # TODO: this is NOT equivalent to the original definition
  res$Evap_mismatch_m3_s <- -(petp_m3_s - petp_m3_s_drain - petp_m3_s_rain)

  return(res)
}


# {
#   # TODO: Why do we shift forward these variables?
#   res$has_accum_drain <- res$lag_accum_drain > 0
#   idxs <- .$has_accum_drain
#   .$irrigation[idxs] <- .lag$irrigation[ord][idxs]
#   .$draining[idxs] <- .lag$draining[ord][idxs]
#   # TODO: Does it really make sense to just shift the height_diff_cm?
#   # Example:
#   # Yesterday we were supposed to drain 5 cm of water, but the ditch was able to
#   # support only 3 cm. Today, we don't have to drain 5 cm, but only 2!
#   #
#   .$height_diff_cm[idxs] <- .lag$height_diff_cm[ord][idxs]
#
#   return(.)
# }

compute_ideal_outflows_v2 <- function(., ideal_flow_rate_cm) {
  .$ideal_inflow_cm <- .$irrigation * (
    .$draining * ideal_flow_rate_cm +
      (1-.$draining) * (.$height_diff_cm - .$petp_cm)  # TODO: Is the outflow supposed to be zero here?
  )

  # TODO: Why does the rain term below depend on .$has_accum_drain??
  .$ideal_outflow_cm <- .$ideal_inflow_cm - .$height_diff_cm + # In the absence of rain
    (!.$has_accum_drain | .$petp > 0) * .$petp_cm
  .$ideal_outflow_cm <- pmax(.$ideal_outflow_cm, 0)

  .$ideal_outflow_m3_s <- (.$ideal_outflow_cm / 100) * .$area  / s_per_day()
  .$ideal_outflow_rain <- (.$petp > 0) * .$petp_m3_s
  .$ideal_outflow_phys <- .$draining * (.$ideal_outflow_m3_s - .$ideal_outflow_rain)
  .$ideal_outflow_flux <- .$irrigation * .$ideal_outflow_phys
  .$ideal_outflow_drain <- (1 - .$irrigation) * .$ideal_outflow_phys

  # TODO: If I get the main idea, here we are setting:
  # 1. the ideal "drain" outflow is the accumulated drain water if there was any,
  # and if in this day we were not supposed to be draining, or if we were
  # irrigating.
  # 2. The ideal "rain" outflow is the accumulated rain from today plus that of
  # yesterday.
  .$ideal_outflow_drain <-
    .$has_accum_drain * .$lag_accum_drain + (1 - .$has_accum_drain) * .$ideal_outflow_drain
  .$ideal_outflow_rain <- .$ideal_outflow_rain + .$lag_accum_rain

  # TODO: Why isn't the ideal_outflow_flux updated here? It seems like in the
  # case irrigation * draining = TRUE, the ideal outflow is independent of accum
  # values.

  .
}

compute_real_outflows_v2 <- function(.) {
  # Permute the rows of ., so that clusters in the same ditch
  # are emptied in a random order every day.
  #
  # For testing purposes, this behavior is overridden through the environment
  # variable 'erahumed_hb_sort_clusters' which, when TRUE, sorts clusters in
  # alphabetic order.
  .alphabetic <- Sys.getenv("erahumed_hb_sort_clusters", "FALSE") |>
    as.logical()
  ord <- order(.$cluster_id)
  ord <- ifelse(.alphabetic, ord, sample(ord))

  capacity <- .$flowpoint[1]

  cum_flows <- pmin(cumsum(c(0, .$ideal_outflow_rain[ord])), capacity)
  .$real_outflow_rain[ord] <- diff(cum_flows)
  capacity <- capacity - cum_flows[length(cum_flows)]

  cum_flows <- pmin(cumsum(c(0, .$ideal_outflow_drain[ord])), capacity)
  .$real_outflow_drain[ord] <- diff(cum_flows)
  capacity <- capacity - cum_flows[length(cum_flows)]

  cum_flows <- pmin(cumsum(c(0, .$ideal_outflow_flux[ord])), capacity)
  .$real_outflow_flux[ord] <- diff(cum_flows)

  .$real_outflow_m3_s <-
    .$real_outflow_rain + .$real_outflow_drain + .$real_outflow_flux
  .$real_outflow_cm <- .$real_outflow_m3_s * 100 * s_per_day() / .$area

  .
}

compute_accum_values_v2 <- function(.) {
  .$accum_rain <- .$ideal_outflow_rain - .$real_outflow_rain
  .$accum_drain <- .$ideal_outflow_drain - .$real_outflow_drain
  .$accum_flux <- .$ideal_outflow_flux - .$real_outflow_flux

  .$accum_rain <- ifelse(.$accum_rain > 1e-5, .$accum_rain, 0)           # Required???
  .$accum_drain <- ifelse(.$accum_drain > 1e-5, .$accum_drain, 0)
  .$accum_flux <- ifelse(.$accum_flux > 1e-5, .$accum_flux, 0)

  .
}

compute_real_inflow_v2 <- function(., ideal_flow_rate_cm) {
  .$real_inflow_cm <- .$irrigation * (
    (1 - .$draining) * (
      # TODO: should this be done in the "ideal" inflow computation?
      ideal_flow_rate_cm + (.$mm >= 11) * ideal_flow_rate_cm + (.$lag_accum_rain <= 0) * (.$petp < 0) * .$petp_cm
    ) +
      .$irrigation * .$draining * (
        (.$accum_flux > 0) *(.$petp < 0) *(.$lag_accum_rain <= 0) *(-.$petp_cm) +
          (.$accum_flux <= 0) * ideal_flow_rate_cm
      )
  )
  .$real_inflow_cm <- ifelse(.$Evap_mismatch != 0, abs(.$Evap_mismatch), .$real_inflow_cm)

  .
}

compute_hb_daily_v2 <- function(current, previous, ideal_flow_rate_cm = 5) {
  current |>
    compute_lags_v2(previous) |>
    compute_ideal_outflows_v2(ideal_flow_rate_cm = ideal_flow_rate_cm) |>
    compute_real_outflows_v2() |>
    compute_accum_values_v2() |>
    compute_real_inflow_v2(ideal_flow_rate_cm = ideal_flow_rate_cm)
}
