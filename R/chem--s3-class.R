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
              ssd_chronic_sigma = ssd_chronic_sigma
  )

  class(res) <- "erahumed_chemical"

  return(res)
}


