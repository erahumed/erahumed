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
    volume, inflow_total, k = 61, units = c("days", "seconds")
)
{
  assert_numeric_vector(volume)
  assert_numeric_vector(inflow_total)
  assert_positive_integer(k)
  units <- match.arg(units)

  norm <- switch(units,
                 days = s_per_day(),
                 seconds = 1
  )

  vol_smooth <- moving_average(volume, k)
  inflow_smooth <- moving_average(inflow_total, k)

  return(vol_smooth / inflow_smooth / norm)
}



#' Linear Storage Curve
#'
#' @author Pablo Amador Crespo, Valerio Gherardi
#'
#' @description
#' Creates a linear storage curve with a given slope and intercept,
#' *viz.* \eqn{\text{Volume} = \text{slope} \times \text{Level} + \text{intercept}}.
#'
#' @param intercept a number. Intercept of the linear storage curve.
#' @param slope a number. Slope of the linear storage curve
#'
#' @return A function with signature `function(level)`, whose argument is
#' assumed to be a numeric vector, that computes volumes from lake levels
#' according to the prescribed linear relationship.
#'
#' @details
#' The handling of units of measurement is left to the user.
#'
#' @export
linear_storage_curve <- function(intercept, slope)
{
  assert_positive_number(intercept)
  assert_positive_number(slope)

  function(level) {
    assert_numeric_vector(level)
    level * slope + intercept
  }
}



#' Volume daily changes due to Precipitation and Evapotranspiration
#'
#' @author Pablo Amador Crespo, Valerio Gherardi
#'
#' @description
#' Creates a (homogeneous) linear fucntion that computes volume daily changes
#' due to precipitation and evapotranspiration
#' as \eqn{\Delta V = \Delta h_\text{P} \cdot S_\text{P} - \Delta h_\text{ETP} \cdot S_\text{ETP}}, where
#' \eqn{\Delta h _\text{P,ETP}} are precipitation and evapotranspiration in
#' expressed in millimiters, and \eqn{S_{\text{P,ETP}}} are effective surfaces.
#'
#' @param surface_P,surface_ETP positive numbers. The effective surfaces (
#' expressed in \eqn{\text{m}^2}) for volume changes due to precipitation and
#' evapotranspiration (see description).
#'
#' @return A function with signature `function(P, ETP)`, whose arguments are
#' assumed to be numeric vectors, that computes volumes changes due to
#' precipitation and evapotranspiration, according to the prescribed linear
#' relationship.
#'
#' @export
linear_petp_surface <- function(surface_P, surface_ETP)
{
  assert_positive_number(surface_P)
  assert_positive_number(surface_ETP)

  function(P, ETP) {
    assert_numeric_vector(P)
    assert_numeric_vector(ETP)
    (P * surface_P - ETP * surface_ETP) / 1000
  }
}



#' Compute Ditch Inflow Percent
#'
#' @author Pablo Amador Crespo, Valerio Gherardi
#'
#' @description Computes the fraction of water that flows through each ditch, as
#' the fraction of total surface covered by the clusters adjacent to said ditch.
#'
#' @param clusters_df A dataframe containing two columns `ditch` and `area`.
#' Each row is assumed to represent a distinct cluster pertaining to `ditch`,
#' and with the surface specified by `area`.
#'
#' @return A dataframe containing two columns, `ditch` and `inflow_pct`,
#' specifying a ditch and the amount of inflow corresponding to it,
#' respectively.
#'
#' @details
#' TODO: other options to obtain ditch percents (e.g. empirical data by Soria et al.)?
#'
#' @export
compute_ditch_inflow_pct <- function(cluster_data)
{
  res <- aggregate(area ~ ditch, data = cluster_data, FUN = sum)
  res$area <- res$area / sum(res$area)
  names(res)[names(res) == "area"] <- "inflow_pct"
  res <- res[order(res$ditch), ]
  return(res)
}
