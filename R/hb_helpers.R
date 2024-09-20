lhb_var_labs <- function(invert = FALSE) {
  res <- c(
    level = "Lake Level [m]",
    volume = "Lake Volume [m\u{00B3}]",
    outflow_total = "Total Outflow [m\u{00B3} / s]",
    outflow_pujol = "Pujol Outflow [m\u{00B3} / s]",
    outflow_perellonet = "Perellonet Outflow [m\u{00B3} / s]",
    outflow_perello = "Perello Outflow [m\u{00B3} / s]",
    outflow_extra = "Extra Outflow [m\u{00B3} / s]",
    inflow_total = "Total Inflow [m\u{00B3} / s]",
    residence_time_days = "Residence Time [Days]"
  )

  if (!invert)
    return(res)

  res_inv <- names(res)
  names(res_inv) <- res

  return(res_inv)
}

lhb_is_imputed_var <- function(variable) {
  imp_var <- if (variable %in% c("level", "volume")) {
    "is_imputed_level"
  } else {
    "is_imputed_outflow"
  }
}
