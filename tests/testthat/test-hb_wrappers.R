test_that("hydro_balance_local_snapshot", {
  set.seed(840)
  res <- albufera_hydro_balance_local(date_min = "2010-01-01",
                                      date_max = "2012-01-01"
                                      )

  res <- res[order(res$date, res$cluster_id), ]
  cols_to_test <- c(
    # Basics
    "date", "petp_m3_s",
    "cluster_id", "area", "ditch", "flowpoint",

    # Modified by functions
    "irrigation", "draining", "height_cm", "height_diff_cm",
    "inflow", "outflow", "outflow_m3", "outflow_rain",
    "outflow_phys", "outflow_flux", "outflow_drain",
    "real_outflow_rain", "real_outflow_drain", "real_outflow_flux",
    "accum_rain", "accum_drain", "accum_flux",
    "Evap_mismatch"
    )

  for (col in cols_to_test)
    expect_snapshot(digest::digest(res[[col]]))
})
