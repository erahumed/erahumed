#' Hydrological Balance of the Albufera Lake
#'
#' @author Pablo Amador Crespo, Valerio Gherardi
#'
#' @description
#' Wrapper around \link{hba}, used to run the global hydrological balance
#' calculations with the data for the Albufera lake packed up in the
#' `data.frame`s exported by `erahumed`.
#'
#' @inheritParams hba
#' @param outflows_df,petp_df `data.frame`s, whose structures follow the
#' templates of \link{albufera_outflows} and \link{albufera_petp}, respectively;
#' see details.
#'
#' @details
#' The numeric inputs for the linear storage curve are taken from the CHJ report
#' [Modelo de seguimiento de l’Albufera de Valencia con AQUATOOLDMA.](https://www.chj.es/Descargas/ProyectosOPH/Consulta%20publica/PHC-2015-2021/ReferenciasBibliograficas/HumedalesZonasProtegidas/CHJ,2012.Aquatool_Albufera.pdf).
#' The values used as the arguments of `petp_surface()` were calculated by the
#' package authors, and correspond to the total study area (`surface_P`) and
#' the flooded surface (`surface_ETP`).
#'
#' The `outflows_df` data.frame is supposed to have all columns of
#' \link{albufera_outflows} whose names do not start by `outflow_`, with the
#' appropriate type. In addition, `outflows_df` can have an arbitrary number of
#' numeric columns named `outflow_*`, which represent the measured outflows for
#' the system. It is fundamental that outflow columns follow this particualr
#' naming scheme, as these are automatically recognized by
#' `hba()` and passed down to low level functions.
#'
#' @return An object of class `hba`, a lightweight wrapper of `data.frame`
#' with a few additional visualization methods (most prominently
#' \link{plot.hba}). The underlying data-frame contains as columns the
#' input time series, as well as the following calculated columns:
#' * `volume` Volume time series, obtained from the storage curve.
#' * `volume_change` Differenced time series of volume. The \eqn{n}-th is given
#' by \eqn{\Delta V _n \equiv V_{n+1}-V_n}, where \eqn{V_n} is volume at time
#' step \eqn{n}.
#' * `petp_change` Time series. The amount \eqn{\Delta V _n ^\text{P-ETP}} of
#' volume change due to precipitation and evapotranspiration, obtained from the
#' P-ETP surface.
#' * `outflow_total` Time series of total outflows, measured in cube meters per
#' second. This is the sum
#' \eqn{\sum _i O_i} of time series passed through the `outflows` argument,
#' plus an extra term \eqn{\delta O} of unaccounted outflow. The correction term
#' is defined by
#' \eqn{\delta O = \frac{\Delta V _n - \Delta V _n ^\text{P-ETP}}{24 \times 60 \times 60} -\sum _i O_i}
#' and is chosen in such a way to ensure that the total inflow is always
#' non-negative.
#' * `outflow_extra` Time series. The unaccounted outflow term \eqn{\delta O}
#' described above.
#' * `inflow_total` Time serie of total inflows, in cube meters per second.
#' This is computed as \eqn{I = \sum _{i} O_i + \delta O + \frac{\Delta V _n - \Delta V _n ^\text{P-ETP}}{24 \times 60 \times 60}}.
#' * `residence_time_days`. Residence time, as modeled by \link{hba_residence_time}.
#'
#' @export
hba <- function(
    outflows_df = erahumed::albufera_outflows,
    petp_df = erahumed::albufera_petp,
    storage_curve = linear_storage_curve(intercept = 16.7459 * 1e6,
                                         slope = 23.6577 * 1e6),
    petp_surface = linear_petp_surface(surface_P = 114.225826072 * 1e6,
                                       surface_ETP = 79.360993685 * 1e6)
)
{
  hba_argcheck(outflows_df, petp_df)

  # Just to get intersection of dates
  input <- merge(outflows_df, petp_df, by = "date", sort = TRUE)

  .hba(
    level = input$level,
    rain_mm = input$rain_mm,
    evapotranspiration_mm = input$evapotranspiration_mm,
    outflows = input[, grepl("^outflow_", colnames(input))],
    date = input$date,
    is_imputed_level = input$is_imputed_level,
    is_imputed_outflow = input$is_imputed_outflow,
    storage_curve = storage_curve,
    petp_surface = petp_surface
  )
}

hba_argcheck <- function(outflows_df, petp_df) {
  tryCatch(
    {
      outflow_required_cols <- c("date",
                                 "level",
                                 "is_imputed_level",
                                 "is_imputed_outflow")
      assert_data.frame(
        outflows_df,
        template = erahumed::albufera_outflows[, outflow_required_cols]
        )

      assert_data.frame(petp_df, template = erahumed::albufera_petp)
    },
    error = function(e) {
      class(e) <- c("hba_argcheck_error", class(e))
      stop(e)
    }
  )

}
