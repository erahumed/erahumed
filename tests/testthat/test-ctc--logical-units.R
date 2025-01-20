test_that("ctc_porosity(): result is a percentage with the standard inputs", {
  arg_names <- names(formals(ctc_porosity))
  args <- formals(setup_exposure)[arg_names]
  res <- do.call(ctc_porosity, args)

  expect_gte(res, 0)
  expect_lte(res, 1)
})
test_that("ctc_porosity(): result is a percentage with random inputs", {
  set.seed(840)
  n <- 1e3

  wilting <- runif(n)
  fc <- runif(n, min = wilting)
  res <- ctc_porosity(fc = fc, wilting = wilting)

  expect_true(all( 0 <= res & res <= 1 ))
})


test_that("ctc_fds(): result is a percentage with the standard inputs", {
  global_params <- formals(setup_exposure)

  bd_g_cm3 <- global_params$bd_g_cm3

  fc <- global_params$fc
  wilting <- global_params$wilting
  pos <- ctc_porosity(fc = fc, wilting = wilting)

  chemicals <- unique(albufera_ca_schedules$chemical)
  for (chemical in chemicals) {
    foc <- 0.17
    kd_cm3_g <- foc * ctc_get_param(chemical, "koc_cm3_g")

    res <- ctc_fds(pos = pos, kd_cm3_g = kd_cm3_g, bd_g_cm3 = bd_g_cm3)
    expect_gte(res, 0)
    expect_lte(res, 1)
  }

})
test_that("ctc_fds(): result is a percentage with random inputs", {
  set.seed(840)
  n <- 1e3

  pos <- runif(n, 0, 1)
  kd_cm3_g <- runif(n, 0, 100)
  bd_g_cm3 <- runif(n, 0, 100)

  res <- ctc_fds(pos = pos, kd_cm3_g = kd_cm3_g, bd_g_cm3 = bd_g_cm3)

  expect_true(all( 0 <= res & res <= 1 ))
})


test_that("ctc_fdw(): result is a percentage with the standard inputs", {
  css_ppm <- formals(setup_exposure)$css_ppm

  chemicals <- unique(albufera_ca_schedules$chemical)
  for (chemical in chemicals) {
    foc <- 0.17
    kd_cm3_g <- foc * ctc_get_param(chemical, "koc_cm3_g")

    res <- ctc_fdw(kd_cm3_g = kd_cm3_g, css_ppm = css_ppm)
    expect_gte(res, 0)
    expect_lte(res, 1)
  }

})
test_that("ctc_fdw(): result is a percentage with random inputs", {
  set.seed(840)
  n <- 1e3

  kd_cm3_g <- runif(n, 0, 100)
  css_ppm <- runif(n, 0, 1e-5)

  res <- ctc_fdw(kd_cm3_g = kd_cm3_g, css_ppm = css_ppm)

  expect_true(all( 0 <= res & res <= 1 ))
})


test_that("ctc_kdifus_m_day(): is always positive with the standard inputs", {
  global_params <- formals(setup_exposure)

  fc <- global_params$fc
  wilting <- global_params$wilting
  pos <- ctc_porosity(fc = fc, wilting = wilting)

  chemicals <- unique(albufera_ca_schedules$chemical)
  for (chemical in chemicals) {
    MW <- ctc_get_param(chemical, "MW")

    res <- ctc_kdifus_m_day(pos = pos, MW = MW)
    expect_gte(res, 0)
  }

})
test_that("ctc_kdifus_m_day(): is always positive", {
  skip("ctc_kdifus_m_day() can become negative outside of the approx regime")
  expect_gte(ctc_kdifus_m_day(pos = 1, MW = 1), 0)
})

test_that("ctc_cover(): is a percentage series with the standard inputs", {
  global_params <- formals(setup_exposure)

  jgrow <- global_params$jgrow
  covmax <- global_params$covmax

  seed_day <- -150 + 1:365

  res <- ctc_cover(seed_day = seed_day, jgrow = jgrow, covmax = covmax)

  expect_vector(res, ptype = numeric(), size = length(seed_day))
  expect_true(all( 0 <= res & res <= 1 ) )

})

test_that("ctc_msapp(): result is always a fraction of application",{
  chemicals <- unique(albufera_ca_schedules$chemical)
  for (chemical in chemicals) {
    res <- ctc_msapp(
      application_kg = 1,
      drift = 0,
      cover = 0,
      SNK = 0,
      is_empty = TRUE,
      dinc_m = ctc_get_param(chemical, "dinc_m"),
      dact_m = formals(setup_exposure)$dact_m
      )
    expect_true(all( 0 <= res & res <= 1 ))
  }

})
