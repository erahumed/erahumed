compute_lags <- function(., .lag) {
  ord <- match(.$cluster_id, .lag$cluster_id)

  .$lag_accum_drain <- .lag$accum_drain[ord]
  .$lag_accum_rain <- .lag$accum_rain[ord]

  # In the case in which evapotranspiration prevails on precipitation (petp < 0)
  # we subtract the evaporated water from the accumulated drain water if there's
  # enough of this, otherwise from the accumulated rain water.
  #
  # TODO: here the water balance is not guaranteed to check out. For example
  # lag$accum_drain = .5, lag$accum_rain = 1.5, and petp = -1
  # we end up with
  # .$lag_accum_drain = 0, .$lag_accum_rain = .5.,
  # so we have subtracted 1.5 in total

  .$lag_accum_drain <- pmax(
    .$lag_accum_drain + (.$petp < 0 & .$lag_accum_drain > 0) * .$petp_m3_s,
    0)
  .$lag_accum_rain <- .$lag_accum_rain +
    (.$petp < 0 & .$lag_accum_rain > 0 & .$lag_accum_drain <= 0) * .$petp_m3_s

  # TODO: Why do we do this def. before pmax()ing the variable?
  .$lag_accum_rain_cm <- .$lag_accum_rain * 100 * s_per_day() / .$area
  .$lag_accum_rain <- pmax(.$lag_accum_rain, 0)


  .$Evap_mismatch <-  # TODO: What is the purpose of this variable
    (.$irrigation & .$draining & .lag$accum_rain[ord] > 0 & .$petp < 0) *
    pmin(.$lag_accum_rain_cm + .$petp_cm, 0) # Why are we adding again "petp"?

  # TODO: Why the condition is always on lag_accum_drain?
  # Also, what does the (!.$draining | .$irrigation) condition mean?
  # Can we rename this variable so that it's name is a bit more expressive? E.g. shift_drain plan

  .$corrected <- .$lag_accum_drain > 0


  # TODO: Why do we shift forward these variables?
  idxs <- .$corrected
  .$irrigation[idxs] <- .lag$irrigation[ord][idxs]
  .$draining[idxs] <- .lag$draining[ord][idxs]
  # TODO: Does it really make sense to just shift the height_diff_cm?
  # Example:
  # Yesterday we were supposed to drain 5 cm of water, but the ditch was able to
  # support only 3 cm. Today, we don't have to drain 5 cm, but only 2!
  #
  .$height_diff_cm[idxs] <- .lag$height_diff_cm[ord][idxs]

  return(.)
}

compute_ideal_outflows <- function(., ideal_flow_rate_cm) {
  .$ideal_inflow_cm <- .$irrigation * (
    .$draining * ideal_flow_rate_cm +
    (1-.$draining) * (.$height_diff_cm - .$petp_cm)  # TODO: Is the outflow supposed to be zero here?
    )

  # TODO: is the outflow here supposed to include p-etp?
  # also, why are we conditioning on .$lag_accum_drain > 0
  .$ideal_outflow_cm <- ifelse(.$lag_accum_drain > 0 & .$petp < 0,
                      pmax(.$ideal_inflow_cm - .$height_diff_cm, 0),
                      pmax(.$ideal_inflow_cm + .$petp_cm - .$height_diff_cm, 0)
                      )
  .$ideal_outflow_m3_s <- (.$ideal_outflow_cm / 100) * .$area  / s_per_day()
  # TODO: This is an "outflow" because if it rains it is supposed to flow away
  # through the ditch, is this understanding correct
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
    .$corrected * .$lag_accum_drain + (1 - .$corrected) * .$ideal_outflow_drain
  .$ideal_outflow_rain <- .$ideal_outflow_rain + .$lag_accum_rain

  # TODO: Why isn't the ideal_outflow_flux updated here? It seems like in the
  # case irrigation * draining = TRUE, the ideal outflow is independent of accum
  # values.

  .
}

compute_real_outflows <- function(.) {
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
compute_accum_values <- function(.) {
  .$accum_rain <- .$ideal_outflow_rain - .$real_outflow_rain
  .$accum_drain <- .$ideal_outflow_drain - .$real_outflow_drain
  .$accum_flux <- .$ideal_outflow_flux - .$real_outflow_flux

  .$accum_rain <- ifelse(.$accum_rain > 1e-5, .$accum_rain, 0)           # Required???
  .$accum_drain <- ifelse(.$accum_drain > 1e-5, .$accum_drain, 0)
  .$accum_flux <- ifelse(.$accum_flux > 1e-5, .$accum_flux, 0)

  .
}
compute_real_inflow <- function(., ideal_flow_rate_cm) {
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

compute_hb_daily <- function(current, previous, ideal_flow_rate_cm = 5) {
  current |>
    compute_lags(previous) |>
    compute_ideal_outflows(ideal_flow_rate_cm = ideal_flow_rate_cm) |>
    compute_real_outflows() |>
    compute_accum_values() |>
    compute_real_inflow(ideal_flow_rate_cm = ideal_flow_rate_cm)
}
