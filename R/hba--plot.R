#' Plot HBA simulation layer output
#'
#' @description
#' Plot method for \link{hba} simulation layers.
#'
#' @param x An object of class `hba`.
#' @param variable The variable to be plotted. Can be any numeric column of
#' `get_layer_output(x)`.
#' @param ... Not used.
#'
#' @return A \link[dygraphs]{dygraph} plot.
#'
#' @export
plot.erahumed_hba <- function(x, variable, ...) {
  df <- get_layer_output(x)

  plot_hba_argcheck(df, variable, ...)

  var_lab <- hba_var_labs()[variable] |> unname()

  is_imputed <- df[[hba_is_imputed_var(variable)]]

  df_obs <- df_imp <- df[, ]
  df_obs[[variable]][is_imputed] <- NA
  df_imp[[variable]][!is_imputed] <- NA

  time_series_obs <- xts::xts(df_obs[[variable]], order.by = df_obs$date)
  time_series_imp <- xts::xts(df_imp[[variable]], order.by = df_imp$date)
  combined_ts <- cbind(Observed = time_series_obs, Imputed = time_series_imp)

  dygraphs::dygraph(combined_ts, main = paste("Time Series of", var_lab)) |>
    dygraphs::dySeries("Observed", color = "blue", strokePattern = "solid") |>
    dygraphs::dySeries("Imputed", color = "red", strokePattern = "dashed") |>
    dygraphs::dyAxis("x", label = "Date") |>
    dygraphs::dyAxis("y", label = var_lab, axisLabelWidth = 80) |>
    dygraphs::dyLegend(show = "always")
}

plot_hba_argcheck <- function(x, variable, ...) {

  tryCatch(
    {
      assert_string(variable)
      if (!variable %in% colnames(x)) {
        stop(paste(variable, "is not a column of", deparse(substitute(x))))
      }
      for (name in names(list(...))) {
        warning(paste0("Argument '", name, "' not used."))
      }
    },
    error = function(cnd) {
      class(cnd) <- c("plot.hba_error", class(cnd))
      stop(cnd)
    },
    warning = function(cnd) {
      class(cnd) <- c("plot.hba_warning", class(cnd))
      warning(cnd)
    })

}

hba_var_labs <- function(invert = FALSE) {
  res <- c(
    level = "Lake Level [m]",
    volume = "Lake Volume [m\u{00B3}]",
    outflow_total = "Total Outflow [m\u{00B3} / s]",
    outflow_pujol = "Pujol Outflow [m\u{00B3} / s]",
    outflow_perellonet = "Perellonet Outflow [m\u{00B3} / s]",
    outflow_perello = "Perello Outflow [m\u{00B3} / s]",
    outflow_recirculation = "Water Recirculation Outflow [m\u{00B3} / s]",
    inflow_total = "Total Inflow [m\u{00B3} / s]",
    residence_time_days = "Residence Time [Days]"
  )

  if (!invert)
    return(res)

  res_inv <- names(res)
  names(res_inv) <- res

  return(res_inv)
}

hba_is_imputed_var <- function(variable) {
  imp_var <- if (variable %in% c("level", "volume")) {
    "is_imputed_level"
  } else {
    "is_imputed_outflow"
  }
}
