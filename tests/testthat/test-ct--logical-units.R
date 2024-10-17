test_that("ct_porosity(): result is a percentage with the standard inputs", {
  arg_names <- names(formals(ct_porosity))
  args <- formals(compute_ct)[arg_names]
  res <- do.call(ct_porosity, args)

  expect_gte(res, 0)
  expect_lte(res, 1)
})
test_that("ct_porosity(): result is a percentage with random inputs", {
  set.seed(840)
  n <- 1e3

  wilting <- runif(n)
  fc <- runif(n, min = wilting)
  res <- ct_porosity(fc = fc, wilting = wilting)

  expect_true(all( 0 <= res & res <= 1 ))
})


test_that("ct_fds(): result is a percentage with the standard inputs", {
  global_params <- formals(compute_ct)

  bd_g_cm3 <- global_params$bd_g_cm3

  fc <- global_params$fc
  wilting <- global_params$wilting
  pos <- ct_porosity(fc = fc, wilting = wilting)

  chemicals <- unique(albufera_ca_schedules$chemical)
  for (chemical in chemicals) {
    kd_cm3_g <- ct_get_param(chemical, "kd_cm3_g")

    res <- ct_fds(pos = pos, kd_cm3_g = kd_cm3_g, bd_g_cm3 = bd_g_cm3)
    expect_gte(res, 0)
    expect_lte(res, 1)
  }

})
test_that("ct_fds(): result is a percentage with random inputs", {
  set.seed(840)
  n <- 1e3

  pos <- runif(n, 0, 1)
  kd_cm3_g <- runif(n, 0, 100)
  bd_g_cm3 <- runif(n, 0, 100)

  res <- ct_fds(pos = pos, kd_cm3_g = kd_cm3_g, bd_g_cm3 = bd_g_cm3)

  expect_true(all( 0 <= res & res <= 1 ))
})


test_that("ct_fdw(): result is a percentage with the standard inputs", {
  css_ppm <- formals(compute_ct)$css_ppm

  chemicals <- unique(albufera_ca_schedules$chemical)
  for (chemical in chemicals) {
    kd_cm3_g <- ct_get_param(chemical, "kd_cm3_g")

    res <- ct_fdw(kd_cm3_g = kd_cm3_g, css_ppm = css_ppm)
    expect_gte(res, 0)
    expect_lte(res, 1)
  }

})
test_that("ct_fdw(): result is a percentage with random inputs", {
  set.seed(840)
  n <- 1e3

  kd_cm3_g <- runif(n, 0, 100)
  css_ppm <- runif(n, 0, 1e-5)

  res <- ct_fdw(kd_cm3_g = kd_cm3_g, css_ppm = css_ppm)

  expect_true(all( 0 <= res & res <= 1 ))
})


test_that("ct_kdifus_m_day(): is always positive with the standard inputs", {
  global_params <- formals(compute_ct)

  fc <- global_params$fc
  wilting <- global_params$wilting
  pos <- ct_porosity(fc = fc, wilting = wilting)

  chemicals <- unique(albufera_ca_schedules$chemical)
  for (chemical in chemicals) {
    MW <- ct_get_param(chemical, "MW")

    res <- ct_kdifus_m_day(pos = pos, MW = MW)
    expect_gte(res, 0)
  }

})
test_that("ct_kdifus_m_day(): is always positive", {
  skip("ct_kdifus_m_day() can become negative outside of the approx regime")
  expect_gte(ct_kdifus_m_day(pos = 1, MW = 1), 0)
})

test_that("ct_cover(): is a percentage series with the standard inputs", {
  global_params <- formals(compute_ct)

  jgrow <- global_params$jgrow
  covmax <- global_params$covmax

  seed_day <- -150 + 1:365

  res <- ct_cover(seed_day = seed_day, jgrow = jgrow, covmax = covmax)

  expect_vector(res, ptype = numeric(), size = length(seed_day))
  expect_true(all( 0 <= res & res <= 1 ) )

})

test_that("ct_setl_fac(): is a percentage with the standard inputs", {
  # Generate a mock but realistic height_sod_m
  set.seed(840)
  n <- 100
  height_sod_m <-
    sample(c(0, 10), size = n, replace = TRUE) +
    10 * arima.sim(list(ar = 0.6), n = n)
  height_sod_m <- pmax(height_sod_m, 0) + runif(n, max = 1e-6) * (runif(n) > .3)

  css_ppm <- formals(compute_ct)$css_ppm

  chemicals <- unique(albufera_ca_schedules$chemical)
  for (chemical in chemicals) {
    ksetl_m_day <- ct_get_param(chemical, "ksetl_m_day")
    kd_cm3_g <- ct_get_param(chemical, "kd_cm3_g")

    fdw <- ct_fdw(kd_cm3_g = kd_cm3_g, css_ppm = css_ppm)
    fpw <- 1-fdw

    res <- ct_setl_fac(ksetl_m_day = ksetl_m_day,
                       fpw = fpw,
                       height_sod_m = height_sod_m)
    expect_true(all( 0 <= res & res <= 1 ))
  }
})

test_that("ct_diff_s(): is always a percentage with the standard inputs", {
  global_params <- formals(compute_ct)

  bd_g_cm3 <- global_params$bd_g_cm3
  dact_m <- global_params$dact_m
  fc <- global_params$fc
  wilting <- global_params$wilting
  pos <- ct_porosity(fc = fc, wilting = wilting)

  chemicals <- unique(albufera_ca_schedules$chemical)
  for (chemical in chemicals) {
    kd_cm3_g <- ct_get_param(chemical, "kd_cm3_g")
    MW <- ct_get_param(chemical, "MW")
    fds <- ct_fds(pos = pos, kd_cm3_g = kd_cm3_g, bd_g_cm3 = bd_g_cm3)
    kdifus_m_day <- ct_kdifus_m_day(pos = pos, MW = MW)

    res <- ct_diff_s(
      kdifus_m_day = kdifus_m_day, fds = fds, pos = pos, dact_m = dact_m)

    expect_gte(res, 0)
    expect_lte(res, 1)
  }

})

test_that("ct_diff_w(): is always a percentage with the standard inputs", {
  set.seed(840)
  n <- 100
  height_sod_m <-
    sample(c(0, 10), size = n, replace = TRUE) +
    10 * arima.sim(list(ar = 0.6), n = n)
  height_sod_m <- pmax(height_sod_m, 0) + runif(n, max = 1e-6) * (runif(n) > .3)

  global_params <- formals(compute_ct)

  css_ppm <- formals(compute_ct)$css_ppm
  fc <- global_params$fc
  wilting <- global_params$wilting
  pos <- ct_porosity(fc = fc, wilting = wilting)

  chemicals <- unique(albufera_ca_schedules$chemical)
  for (chemical in chemicals) {
    kd_cm3_g <- ct_get_param(chemical, "kd_cm3_g")
    MW <- ct_get_param(chemical, "MW")
    fdw <- ct_fdw(kd_cm3_g = kd_cm3_g, css_ppm = css_ppm)
    kdifus_m_day <- ct_kdifus_m_day(pos = pos, MW = MW)

    res <- ct_diff_w(kdifus_m_day = kdifus_m_day, fdw = fdw,
                     height_sod_m = height_sod_m)

    expect_true(all( 0 <= res & res <= 1 ))
  }

})

test_that("ct_outflow_fac(): is always a percentage", {
  set.seed(840)
  n <- 1e3
  volume_eod_m3 <- runif(n, 0, 1e6) * (runif(n) > .8)
  outflow_m3 <- runif(n, 0, 1e6) * (runif(n) > .8)

  res <- ct_outflow_fac(volume_eod_m3 = volume_eod_m3, outflow_m3 = outflow_m3)

  expect_vector(res, numeric(), n)
  expect_false(any( is.na(res) ))
  expect_false(any( is.nan(res) ))
  expect_true(all( 0 <= res & res <= 1 ))
})

test_that("ct_msapp(): result is always a fraction of application",{
  chemicals <- unique(albufera_ca_schedules$chemical)
  for (chemical in chemicals) {
    res <- ct_msapp(
      application_kg = 1,
      drift = 0,
      cover = 0,
      SNK = 0,
      is_empty = TRUE,
      dinc_m = ct_get_param(chemical, "dinc_m"),
      dact_m = formals(compute_ct)$dact_m
      )
    expect_true(all( 0 <= res & res <= 1 ))
  }

})
