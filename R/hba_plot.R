#' Plotting global hydrological balance data
#'
#' @description
#' Plot method for global hydrological balance data, generated through
#' \link{hba}. A simple wrapper around \link[plotly]{plot_ly} to generate
#' time series plots of the calculated quantities.
#'
#' @param x The object of class `hba` containing the data to be plotted.
#' @param variable The variable to be plotted. Can be any numeric column of `x`.
#' @param ... Not used.
#'
#' @return A plotly plot.
#'
#' @export
plot.erahumed_hba <- function(x, variable, ...) {

  plot_hba_argcheck(x, variable, ...)

  var_lab <- hba_var_labs()[variable]

  is_imputed <- x[[hba_is_imputed_var(variable)]]

  df_obs <- df_imp <- x[, ]
  df_obs[[variable]][is_imputed] <- NA
  df_imp[[variable]][!is_imputed] <- NA

  plotly::plot_ly() |>
    plotly::add_trace(
      data = df_obs, x = ~date, y = ~get(variable),
      type = "scatter", mode = "lines",
      line = list(color = "blue", width = 2, dash = "solid"),
      name = "Observed Data"
    ) |>
    plotly::add_trace(
      data = df_imp, x = ~date, y = ~get(variable),
      type = "scatter", mode = "lines",
      line = list(color = "red", width = 2, dash = "dash"),
      name = "Imputed Data"
    ) |>
    plotly::layout(
      title = paste("Time Series of", var_lab),
      xaxis = list(title = "Date"),
      yaxis = list(title = var_lab)
    )
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

hba_is_imputed_var <- function(variable) {
  imp_var <- if (variable %in% c("level", "volume")) {
    "is_imputed_level"
  } else {
    "is_imputed_outflow"
  }
}
