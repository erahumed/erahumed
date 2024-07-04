#' Albufera Hydrological Balance
#'
#' Documentation TBD
#'
#' @export
albufera_hydro_balance <- function() {
  s_per_day <- 24 * 60 * 60

  res <- merge(albufera_outflows, meteo_beni_2023, by = "date", sort = TRUE)
  res$data_is_imputed <-
    res$level_is_imputed |
    res$pujol_is_imputed |
    res$perellonet_is_imputed |
    res$perello_is_imputed

  res$volume <- albufera_storage_curve(res$level)
  res$volume_is_imputed <- res$level_is_imputed

  res$volume_change <- dplyr::lead(res$volume) - res$volume  # TODO: avoid dplyr
  res$volume_change_is_imputed <- res$volume_is_imputed

  res$petp_change <- petp_volume_change(res$P, res$ETP)
  res$petp_change_is_imputed <- FALSE

  res$total_outflow <- res$pujol + res$perellonet +
    ifelse(res$perello > 0, res$perello, 0)
  res$total_outflow_is_imputed <-
    res$pujol_is_imputed |
    res$perellonet_is_imputed |
    res$perello_is_imputed

  res$total_inflow <- res$total_outflow +
    (res$volume_change - res$petp_change) / s_per_day
  res$total_inflow_is_imputed <- res$data_is_imputed

  # Temporary model for residence time

  .k <- 60
  res$residence_time_days <-
    stats::filter(res$volume, rep(1, .k), sides = 2) /
    stats::filter(res$total_inflow, rep(1, .k), sides = 2) /
    (24 * 60 * 60)
  res$residence_time_days <- ifelse(res$residence_time_days > 0,
                                    res$residence_time_days,
                                    NA)
  res$residence_time_days_is_imputed <- res$data_is_imputed

  # TODO: where does this data come from??
  ditch_pct <- c(0.057643743067731, 0.00883863938396492, 0.0078845588682654,
              0.00269092756575864, 0.00237140628156962, 0.00627824524730914,
              0.00482132954767113, 0.0120486656499934, 0.00786119582767117,
              0.00718144428973257, 0.00733430242551832, 0.0201041621013415,
              0.00403304986097902, 0.0105762920202592, 0.03427139459209, 0.0900729323859258,
              0.0729408481704095, 0.197688023651317, 0.338954620928877, 0.0103468287597039,
              0.0154755388184108, 0.00855543033410601, 0.0212517125150535,
              0.0148638326914231, 0.0124741228873672, 0.0234367521275505)
  ditch <- c("d1", "d10", "d11", "d12", "d13", "d14", "d15",
          "d16", "d17", "d18", "d19", "d2", "d20", "d21",
          "d22", "d23", "d24", "d25", "d26", "d3", "d4",
          "d5", "d6", "d7", "d8", "d9")

  # TODO: there are probably better ways of doing this
  for (i in seq_along(ditch)) {
    res[, ditch[i]] <- ditch_pct[i] * res$total_inflow
  }

  # TODO: avoid using tidyr
  res <- tidyr::drop_na(res)

  # TODO: what is this line supposed to do?
  # Pablo: ojo cuando en clean y fill eliminemos los factores
  res[res$total_inflow < 0, colnames(res) %in% ditch] <- 0

  # What is the meaning of the / 6? Does this work correctly given the previous
  # Eq.?
  res$tancats_inflow <- ifelse(res$total_inflow < 0, res$total_inflow / 6, 0)

  return(res)
}


#' Albufera Lake storage curve
#'
#' @author Pablo Amador Crespo, Valerio Gherardi
#'
#' @description
#' Computes the Albufera lake's volume from level using a linear storage curve,
#' *i.e.* \eqn{\text{Volume} = \text{slope} \times \text{Level} + \text{intercept}}.
#'
#' @param level numeric vector. Levels of the lake (in meters above sea level).
#' Accepts vectorized input.
#' @param intercept a number. Intercept of the linear storage curve.
#' @param slope a number. Slope of the linear storage curve
#'
#' @return A numeric vector of the same length of `level`. Computed volumes (in
#' \eqn{\text{m}^3}).
#'
#' @details
#' The default values are taken from the CHJ report
#' [Modelo de seguimiento de l’Albufera de Valencia con AQUATOOLDMA.](https://www.chj.es/Descargas/ProyectosOPH/Consulta%20publica/PHC-2015-2021/ReferenciasBibliograficas/HumedalesZonasProtegidas/CHJ,2012.Aquatool_Albufera.pdf)
#'
#' @export
albufera_storage_curve <- function(
    level, intercept = 16.7459 * 1e6, slope = 23.6577 * 1e6
    )
{
  level * slope + intercept
}




#' Volume daily changes due to Precipitation and Evapotranspiration
#'
#' @author Pablo Amador Crespo, Valerio Gherardi
#'
#' @description
#' Computes the volume daily changes due to precipitation and evapotranspiration
#' as \eqn{\Delta V = \Delta h_\text{P} \cdot S_\text{P} - \Delta h_\text{ETP} \cdot S_\text{ETP}}, where
#' \eqn{\Delta h _\text{P,ETP}} are precipitation and evapotranspiration in
#' expressed in millimiters, and \eqn{S_{\text{P,ETP}}} are effective surfaces.
#'
#' @param P,ETP numeric vectors of same length. Values of precipitation and
#' evapotranspiration in \eqn{\text{mm}}.
#' @param surface_P,surface_ETP positive numbers. The effective surfaces (
#' expressed in \eqn{\text{m}^2}) for volume changes due to precipitation and
#' evapotranspiration (see description).
#'
#' @return A dataframe containing the same columns as `input`, and an additional
#' column (see `output_col` argument) with the computed values of volume
#' daily changes, expressed in \eqn{\text{m} ^3 / \text{s}}.
#'
#' @details
#' TODO: document the extraction of effective surface values.
#' TODO: other options to obtain volume changes?
#'
#' @export
petp_volume_change <- function(
    P, ETP, surface_P = 114.225826072 * 1e6, surface_ETP = 79.360993685 * 1e6
    )
{
  (P * surface_P - ETP * surface_ETP) / 1000
}
