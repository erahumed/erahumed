hb_var_labels <- function(invert = FALSE) {
  res <- c(
    level = "Lake Level [m]",
    volume = "Lake Volume [m\u{00B3}]",
    outflow_total = "Total Outflow [m\u{00B3} / s]",
    outflow_pujol = "Pujol Outflow [m\u{00B3} / s]",
    outflow_perellonet = "Perellonet Outflow [m\u{00B3} / s]",
    outflow_perello = "Perello Outflow [m\u{00B3} / s]",
    inflow_total = "Total Inflow [m\u{00B3} / s]",
    residence_time_days = "Residence Time [Days]"
  )

  if (!invert)
    return(res)

  res_inv <- names(res)
  names(res_inv) <- res

  return(res_inv)
}

hb_get_imp_var <- function(variable) {
  imp_var <- if (variable %in% c("level", "volume")) {
    "level_is_imputed"
  } else {
    "outflow_is_imputed"
  }
}
