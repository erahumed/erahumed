#' Define a pesticide with physico-chemical and toxicity properties
#'
#' Creates a list of parameters that define a pesticide or chemical substance,
#' including its identity, degradation rates, sorption properties, and toxicity
#' to aquatic organisms.
#'
#' @param display_name `r chemical_prop_roxy("display_name")`
#' @param tmoa_id `r chemical_prop_roxy("tmoa_id")`
#' @param MW `r chemical_prop_roxy("MW")`
#' @param ksetl_m_day `r chemical_prop_roxy("ksetl_m_day")`
#' @param kvolat_m_day `r chemical_prop_roxy("kvolat_m_day")`
#' @param sol_ppm `r chemical_prop_roxy("sol_ppm")`
#' @param koc_cm3_g `r chemical_prop_roxy("koc_cm3_g")`
#' @param fet_cm `r chemical_prop_roxy("fet_cm")`
#' @param kf_day `r chemical_prop_roxy("kf_day")`
#' @param kw_day `r chemical_prop_roxy("kw_day")`
#' @param ks_sat_day `r chemical_prop_roxy("ks_sat_day")`
#' @param ks_unsat_day `r chemical_prop_roxy("ks_unsat_day")`
#' @param kw_temp `r chemical_prop_roxy("kw_temp")`
#' @param ks_sat_temp `r chemical_prop_roxy("ks_sat_temp")`
#' @param ks_unsat_temp `r chemical_prop_roxy("ks_unsat_temp")`
#' @param Q10_kw `r chemical_prop_roxy("Q10_kw")`
#' @param Q10_ks_sat `r chemical_prop_roxy("Q10_ks_sat")`
#' @param Q10_ks_unsat `r chemical_prop_roxy("Q10_ks_unsat")`
#' @param ssd_acute_mu `r chemical_prop_roxy("ssd_acute_mu")`
#' @param ssd_acute_sigma `r chemical_prop_roxy("ssd_acute_sigma")`
#' @param ssd_chronic_mu `r chemical_prop_roxy("ssd_chronic_mu")`
#' @param ssd_chronic_sigma `r chemical_prop_roxy("ssd_chronic_sigma")`
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
      assert_numeric_vector(ssd_acute_mu)
      assert_length_one(ssd_acute_mu)
      assert_no_na(ssd_acute_mu)
      assert_positive_number(ssd_acute_sigma)
      assert_numeric_vector(ssd_chronic_mu)
      assert_length_one(ssd_chronic_mu)
      assert_no_na(ssd_chronic_mu)
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

is_erahumed_chemical <- function(x) {
  inherits(x, "erahumed_chemical")
}

#' @export
print.erahumed_chemical <- function(x, ...) {
  cat("<erahumed_chemical>\n")
  cat("Name:       ", x$display_name, "\n")
  cat("TMoA ID:    ", x$tmoa_id, "\n")
  cat("MW:         ", x$MW, "g/mol\n\n")

  cat("Physico-chemical properties:\n")
  cat(sprintf("  Solubility:      %.2f ppm\n", x$sol_ppm))
  cat(sprintf("  Koc:             %.2f cm\u{00B3}/g\n", x$koc_cm3_g))
  cat(sprintf("  Film thickness:  %.3f cm\n", x$fet_cm))
  cat(sprintf("  Settling rate:   %.3f m/day\n", x$ksetl_m_day))
  cat(sprintf("  Volatilization:  %.3f m/day\n\n", x$kvolat_m_day))

  cat("Degradation rates:\n")
  cat(sprintf("  kf (floodwater):           %.4f 1/day\n", x$kf_day))
  cat(sprintf("  kw (water column):         %.4f 1/day @ %.1f\u{00B3}C (Q10 = %.2f)\n", x$kw_day, x$kw_temp, x$Q10_kw))
  cat(sprintf("  ks (saturated sediment):   %.4f 1/day @ %.1f\u{00B3}C (Q10 = %.2f)\n", x$ks_sat_day, x$ks_sat_temp, x$Q10_ks_sat))
  cat(sprintf("  ks (unsaturated sediment): %.4f 1/day @ %.1f\u{00B3}C (Q10 = %.2f)\n\n", x$ks_unsat_day, x$ks_unsat_temp, x$Q10_ks_unsat))

  cat("Toxicity (SSD, log\u{2081}\u{2080} scale):\n")
  cat(sprintf("  Acute   mean \u{00B1} sd: %.2f \u{00B1} %.2f\n", x$ssd_acute_mu, x$ssd_acute_sigma))
  cat(sprintf("  Chronic mean \u{00B1} sd: %.2f \u{00B1} %.2f\n", x$ssd_chronic_mu, x$ssd_chronic_sigma))
  invisible(x)
}

#' @export
summary.erahumed_chemical <- function(object, ...) {
  print.erahumed_chemical(object, ...)
}

