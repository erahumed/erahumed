#' Global Hydrological Balance
#'
#' Documentation TBD
#'
#' @export
hydro_balance_global <- function(
    level,
    P,
    ETP,
    outflows,
    storage_curve = erahumed::linear_storage_curve(slope = 1, intercept = 0),
    petp_surface = erahumed::linear_petp_surface(surface_P = 1, surface_ETP = 1)
    )
{

  res <- data.frame(level, P, ETP)

  res$volume <- storage_curve(level)
  res$volume_change <- c(diff(res$volume), NA)

  res$outflow_total <- 0
  for (n in names(outflows)) {
    res[[ paste0("outflow_", n) ]] <- outflows[[n]]
    res$outflow_total <- res$outflow_total + outflows[[n]]
  }

  res$petp_change <- petp_surface(P, ETP)

  res$inflow_total <- res$outflow_total +
    (res$volume_change - res$petp_change) / s_per_day()
  res$outflow_extra <- pmax(-res$inflow_total, 0)
  res$inflow_total <- pmax(res$inflow_total, 0)

  res$residence_time_days <- residence_time(res$volume, res$inflow_total)

  return(res)
}


