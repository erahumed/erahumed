#' Albufera Global Hydrological Balance
#'
#' @author Pablo Amador Crespo, Valerio Gherardi
#'
#' @description
#' Wrapper around \link{hb_global}, used to run the global hydrological balance
#' calculations with the data for the Albufera lake packed up in the
#' `data.frame`s exported by `erahumed`.
#'
#' @inheritParams hb_global
#' @param outflows_df,petp_df `data.frame`s, whose structures follow the
#' templates of \link{albufera_outflows} and \link{albufera_petp}, respectively.
#'
#' @details
#' The numeric inputs for the linear storage curve are taken from the CHJ report
#' [Modelo de seguimiento de l’Albufera de Valencia con AQUATOOLDMA.](https://www.chj.es/Descargas/ProyectosOPH/Consulta%20publica/PHC-2015-2021/ReferenciasBibliograficas/HumedalesZonasProtegidas/CHJ,2012.Aquatool_Albufera.pdf).
#' The values used as the arguments of `petp_surface()` were calculated by the
#' package authors, and correspond to the total study area (`surface_P`) and
#' the flooded surface (`surface_ETP`).
#'
#' @return Same as \link{hb_global}.
#'
#' @export
albufera_hb_global <- function(
    outflows_df = erahumed::albufera_outflows,
    petp_df = erahumed::albufera_petp,
    storage_curve = linear_storage_curve(intercept = 16.7459 * 1e6,
                                         slope = 23.6577 * 1e6),
    petp_surface = linear_petp_surface(surface_P = 114.225826072 * 1e6,
                                       surface_ETP = 79.360993685 * 1e6)
)
{
  albufera_hb_global_datacheck(outflows_df, petp_df)

  # Just to get intersection of dates
  input <- merge(outflows_df, petp_df, by = "date", sort = TRUE)

  hb_global(
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

albufera_hb_global_datacheck <- function(outflows_df, petp_df) {
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
      class(e) <- c("albufera_hb_global_datacheck_error", class(e))
      stop(e)
    }
  )

}
