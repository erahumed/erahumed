#' Define a pesticide with physico-chemical and toxicity properties
#'
#' Creates a list of parameters that define a pesticide or chemical substance,
#' including its identity, degradation rates, sorption properties, and toxicity
#' to aquatic organisms.
#'
#' @param display_name `[character(1)]` \cr
#' The common name of the chemical (e.g., `"Acetamiprid"`).
#' @param tmoa_id `[character(1)]` \cr
#' Identifier for the toxic mode of action (e.g., `"NicotinicAcetylcholine"`).
#' @param MW `[numeric(1)]` \cr
#' Molecular weight in grams per mole (g/mol).
#' @param ksetl_m_day `[numeric(1)]` \cr
#' Settling rate (m/day), representing particle-bound transfer to sediment.
#' @param kvolat_m_day `[numeric(1)]` \cr
#' Volatilization rate from water to air (m/day).
#' @param sol_ppm `[numeric(1)]` \cr
#' Water solubility (ppm).
#' @param koc_cm3_g `[numeric(1)]` \cr
#' Organic carbon–water partition coefficient (Koc) in cm³/g.
#' @param dinc_m `[numeric(1)]` \cr
#' Depth of incorporation into soil/sediment (m).
#' @param fet_cm `[numeric(1)]` \cr
#' Film thickness for exchange at the sediment–water interface (cm).
#' @param kf_day `[numeric(1)]` \cr
#' Degradation rate in floodwater (1/day).
#' @param kw_day `[numeric(1)]` \cr
#' Degradation rate in the water column (1/day).
#' @param ks_sat_day `[numeric(1)]` \cr
#' Degradation rate in saturated sediment (1/day).
#' @param ks_unsat_day `[numeric(1)]` \cr
#' Degradation rate in unsaturated sediment (1/day).
#' @param kw_temp `[numeric(1)]` \cr
#' Reference temperature for \code{kw_day} (°C).
#' @param ks_sat_temp `[numeric(1)]` \cr
#' Reference temperature for \code{ks_sat_day} (°C).
#' @param ks_unsat_temp `[numeric(1)]` \cr
#' Reference temperature for \code{ks_unsat_day} (°C).
#' @param Q10_kw `[numeric(1)]` \cr
#' Q10 coefficient for temperature correction of degradation in water.
#' @param Q10_ks_sat `[numeric(1)]` \cr
#' Q10 coefficient for temperature correction in saturated sediment.
#' @param Q10_ks_unsat `[numeric(1)]` \cr
#' Q10 coefficient for temperature correction in unsaturated sediment.
#' @param ssd_acute_mu `[numeric(1)]` \cr
#' Mean (log₁₀ scale) of the acute species sensitivity distribution.
#' @param ssd_acute_sigma `[numeric(1)]` \cr
#' Standard deviation (log₁₀ scale) of the acute species sensitivity
#' distribution.
#' @param ssd_chronic_mu `[numeric(1)]` \cr
#' Mean (log₁₀ scale) of the chronic species sensitivity distribution.
#' @param ssd_chronic_sigma `[numeric(1)]` \cr
#' Standard deviation (log₁₀ scale) of the chronic species sensitivity
#' distribution.
#'
#' @return An object of class `erahumed_chemical`.
#' @export
chemical <- function(display_name,
                     tmoa_id,
                     MW,
                     ksetl_m_day,
                     kvolat_m_day,
                     sol_ppm,
                     koc_cm3_g,
                     dinc_m,
                     fet_cm,
                     kf_day,
                     kw_day,
                     ks_sat_day,
                     ks_unsat_day,
                     kw_temp,
                     ks_sat_temp,
                     ks_unsat_temp,
                     Q10_kw,
                     Q10_ks_sat,
                     Q10_ks_unsat,
                     ssd_acute_mu,
                     ssd_acute_sigma,
                     ssd_chronic_mu,
                     ssd_chronic_sigma
)
{
  tryCatch(
    {
      assert_string(display_name)
      assert_string(tmoa_id)
      assert_positive_number(MW)
      assert_positive_number(ksetl_m_day)
      assert_positive_number(kvolat_m_day)
      assert_positive_number(sol_ppm)
      assert_positive_number(koc_cm3_g)
      assert_positive_number(dinc_m)
      assert_positive_number(fet_cm)
      assert_positive_number(kf_day)
      assert_positive_number(kw_day)
      assert_positive_number(ks_sat_day)
      assert_positive_number(ks_unsat_day)
      assert_positive_number(kw_temp)
      assert_positive_number(ks_sat_temp)
      assert_positive_number(ks_unsat_temp)
      assert_positive_number(Q10_kw)
      assert_positive_number(Q10_ks_sat)
      assert_positive_number(Q10_ks_unsat)
      assert_positive_number(Q10_ks_sat)
      assert_positive_number(ssd_acute_mu)
      assert_positive_number(ssd_acute_sigma)
      assert_positive_number(ssd_chronic_mu)
      assert_positive_number(ssd_chronic_sigma)
    },
    error = function(e) {
      class(e) <- c("erahumed_chemical_error", class(e))
      stop(e)
    })

  res <- list(display_name = display_name,
              tmoa_id = tmoa_id,
              MW = MW,
              ksetl_m_day = ksetl_m_day,
              kvolat_m_day = kvolat_m_day,
              sol_ppm = sol_ppm,
              koc_cm3_g = koc_cm3_g,
              dinc_m = dinc_m,
              fet_cm = fet_cm,
              kf_day = kf_day,
              kw_day = kw_day,
              ks_sat_day = ks_sat_day,
              ks_unsat_day = ks_unsat_day,
              kw_temp = kw_temp,
              ks_sat_temp = ks_sat_temp,
              ks_unsat_temp = ks_unsat_temp,
              Q10_kw = Q10_kw,
              Q10_ks_sat = Q10_ks_sat,
              Q10_ks_unsat = Q10_ks_unsat,
              ssd_acute_mu = ssd_acute_mu,
              ssd_acute_sigma = ssd_acute_sigma,
              ssd_chronic_mu = ssd_chronic_mu,
              ssd_chronic_sigma = ssd_chronic_sigma)

  class(res) <- "erahumed_chemical"

  return(res)
}


