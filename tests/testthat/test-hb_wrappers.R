test_that("hydro_balance_local_snapshot", {
  set.seed(840)
  res <- albufera_hydro_balance_local(date_min = "2010-01-01",
                                      date_max = "2010-01-10"
                                      )

  res <- res[order(res$date, res$cluster_id), ]
  cols_to_test <- c(
    "petp", "petp_cm", "petp_m_s", "mm", "dd",
    "cluster_id", "ditch", "area", "irrigation", "draining",
    "accum_drain", "accum_rain", "accum_flux", "height_cm", "height_diff_cm",
    "flowpoint", "inflow", "outflow", "outflow_m3", "outflow_rain",
    "outflow_phys", "outflow_flux", "outflow_drain",
    "real_outflow_rain", "real_outflow_drain", "real_outflow_flux",
    "corrected", "petp_m3_s", ".lag_accum_rain", "lag_accum_drain",
    "lag_accum_rain", "lag_accum_rain_cm", "Evap_mismatch", "condition"
    )

  for (col in cols_to_test)
    expect_snapshot(digest::digest(res[[col]]))
})
