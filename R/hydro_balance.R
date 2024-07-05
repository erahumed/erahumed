#' Albufera Hydrological Balance
#'
#' Documentation TBD
#'
#' @export
albufera_hydro_balance <- function(
    outflows_data = albufera_outflows,
    meteo_data = meteo_beni_2023
    )
{
  # TODO: Check that outflows_data and meteo_data have the correct format

  res <- merge(outflows_data, meteo_data, by = "date", sort = TRUE)
  res$data_is_imputed <-
    res$level_is_imputed |
    res$pujol_is_imputed |
    res$perellonet_is_imputed |
    res$perello_is_imputed

  res$volume <- albufera_storage_curve(res$level)
  res$volume_is_imputed <- res$level_is_imputed

  res$volume_change <- c(res$volume[-1], NA) - res$volume
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
    (res$volume_change - res$petp_change) / s_per_day()
  res$total_inflow_is_imputed <- res$data_is_imputed

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

  res <- na.omit(res)  # Why do we omit NAs?

  # We assume that there are six big "tancats" that suck water from the lake
  # This accounts for the negative "total inflow", which we set to zero
  # thereafter
  res$tancats_outflow <- ifelse(res$total_inflow < 0, -res$total_inflow / 6, 0)
  res[res$total_inflow < 0, colnames(res) %in% c(ditch, "total_inflow")] <- 0
  res$tancats_outflow_is_imputed <- res$total_inflow_is_imputed

  # Temporary model for residence time
  res$residence_time_days <- residence_time(res$volume, res$total_inflow)
  res$residence_time_days_is_imputed <- res$data_is_imputed

  return(res)
}



#' Residence Time
#'
#' @author Pablo Amador Crespo, Valerio Gherardi
#'
#' @description
#' Computes residence times as
#' \eqn{t = s_k(\text{Volume}) / s_k(\vert \text{Inflow}\vert) },
#' where \eqn{s_k(\cdot)} denotes a moving average with a smoothing window of
#' size \eqn{k}, centered at the current observation, and
#' \eqn{\vert \cdot \vert} is the absolute value (to deal with cases in which
#' the inflow becomes negative).
#'
#' @param volume numeric vector. Time series of volumes in \eqn{\text{m}^3}.
#' @param total_inflow numeric vector. Time series of total inflow in
#' \eqn{\text{m}^3 / \text{s}}.
#' @param k positive integer. Size of the window in the moving average. The
#' default is of the order of magnitude of actual residence time for the
#' Albufera Lake.
#' @param units either \code{"days"} or \code{"seconds"}. Units of measure for
#' the returned time series.
#'
#' @return A numeric vector. Time series of residence times, in the units of
#' measure specified by the \code{units} argument (assuming \code{volume} and
#' \code{total_inflow} are provided in the correct units).
#'
#' @export
residence_time <- function(
    volume, total_inflow, k = 61, units = c("days", "seconds")
    )
{
  assert_positive_vector(volume)
  assert_positive_vector(total_inflow)
  assert_positive_integer(k)
  units <- match.arg(units)

  norm <- switch(units,
                 days = s_per_day(),
                 seconds = 1
                 )

  vol_smooth <- moving_average(volume, k)
  inflow_smooth <- moving_average(total_inflow, k)

  return(vol_smooth / inflow_smooth / norm)
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
  assert_numeric_vector(level)
  assert_positive_number(intercept)
  assert_positive_number(slope)

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
  assert_numeric_vector(P)
  assert_numeric_vector(ETP)
  assert_positive_number(surface_P)
  assert_positive_number(surface_ETP)

  (P * surface_P - ETP * surface_ETP) / 1000
}
