#' Plotting global hydrological balance data
#'
#' @description
#' Plot method for global hydrological balance data, generated through
#' \link{lhb}. A simple wrapper around \link[plotly]{plot_ly} to generate
#' time series plots of the calculated quantities.
#'
#' @param x The object of class `lhb` containing the data to be plotted.
#' @param variable The variable to be plotted. Can be any numeric column of `x`.
#' @param ... Not used.
#'
#' @return A plotly plot.
#'
#' @export
plot.erahumed_lhb <- function(x, variable, ...) {

  plot_lhb_argcheck(x, variable, ...)

  var_lab <- lhb_var_labs()[variable]

  is_imputed <- x[[lhb_is_imputed_var(variable)]]

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

plot_lhb_argcheck <- function(x, variable, ...) {

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
      class(cnd) <- c("plot.lhb_error", class(cnd))
      stop(cnd)
      },
    warning = function(cnd) {
      class(cnd) <- c("plot.lhb_warning", class(cnd))
      warning(cnd)
    })

}

#' Plotting local hydrological balance data
#'
#' @description
#' Plot method for local Hydrological balance data, generated through
#' \link{hb_local}. A simple wrapper around \link[plotly]{plot_ly} to generate
#' time series plots of the calculated quantities.
#'
#' @param x The object of class `lhb` containing the data to be plotted.
#' @param type Type of plot to be generated. Currently, only the
#' `"cluster_levels"` plot is implemented.
#' @param ... Further plotting parameters, associated with the plot type
#' specified by `type`; See details.
#'
#' @details
#' The `"cluster_levels"` plot generates a time series plot of the daily
#' water levels (in cm) of an individual cluster. In order to use this plotting
#' method, the user must provide an additional `cluster_id` argument, a string
#' specifying the identifier of the cluster whose levels are to be plotted (see
#' example below).
#'
#' @examples
#' plot(albufera_hb_local(),
#'      type = "cluster_levels",
#'      cluster_id = "02_Carrera_del_Saler0-2_0"
#'      )
#'
#' @return A plotly plot.
#'
#' @export
plot.hb_local <- function(x, type = c("cluster_levels", "map"), ...) {
  type <- match.arg(type)

  if (type == "map")
    stop("'map' plot not yet implemented.")

  args <- list(...)

  if (type == "cluster_levels") {
    if ( !("cluster_id" %in% names(args)) )
      stop("Please specify cluster to plot through the 'cluster_id' argument.")

    return(plot_hb_local_cluster_levels(data = x,
                                        cluster_id = args$cluster_id)
           )
  }
}

plot_hb_local_cluster_levels <- function(data, cluster_id)
{
  data_cluster <- data[data$cluster_id == cluster_id, ]

  ditch <- data_cluster$ditch[1]
  tancat <- data_cluster$tancat[1]
  variety <- data_cluster$variety[1]

  data_ditch <- data[
    data$ditch == ditch & data$tancat == tancat & data$variety == variety,
    ] |>
    stats::aggregate(real_height_cm ~ date, data = _, FUN = mean)


  plotly::plot_ly(x = ~date) |>
    plotly::add_trace(
      data = data_cluster,
      y = ~ideal_height_cm,
      hoverinfo = "skip",
      type = "scatter",
      mode = "lines",
      line = list(width = 1.5, color = "#0000BB", dash = "dash"),
      name = "Ideal"
    ) |>
    plotly::add_trace(
      data = data_ditch,
      y = ~real_height_cm,
      hoverinfo = "skip",
      type = "scatter",
      mode = "lines",
      line = list(width = 1.5, color = "#BB0000", dash = "dot"),
      name = "Average",
      visible = "legendonly"
    ) |>
    plotly::add_trace(
      data = data_cluster,
      y = ~real_height_cm,
      text = ~paste0("Date: ", date,
                     "<br>Height [cm]: ", real_height_cm,
                     "<br>Ideal Height [cm]: ", ideal_height_cm,
                     "<br>Ideal Irrigation: ", ideal_irrigation,
                     "<br>Ideal Draining: ", ideal_draining,
                     "<br>Real Irrigation: ", real_irrigation,
                     "<br>Real Draining: ", real_draining,
                     "<br>Plan Delay: ", plan_delay
      ),
      hoverinfo = "text",
      type = "scatter",
      mode = "lines",
      line = list(width = 2, color = "black"),
      name = "Simulated"
    ) |>
    plotly::layout(
      title = paste("Time Series of Height [cm]"),
      xaxis = list(title = "Date"),
      yaxis = list(title = "Height [cm]")
    )
}
