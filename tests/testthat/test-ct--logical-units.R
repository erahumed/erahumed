test_that("ct_fds(): result is a percentage with the standard inputs", {
  global_params <- formals(erahumed_simulation)

  bd_g_cm3 <- global_params$bd_g_cm3

  pos <- 0.11

  chemicals <- unique(albufera_ca_schedules$chemical)
  for (chemical in chemicals) {
    foc <- 0.17
    kd_cm3_g <- foc * ct_get_param(chemical, "koc_cm3_g")

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
  css_ppm <- formals(erahumed_simulation)$css_ppm

  chemicals <- unique(albufera_ca_schedules$chemical)
  for (chemical in chemicals) {
    foc <- 0.17
    kd_cm3_g <- foc * ct_get_param(chemical, "koc_cm3_g")

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
  global_params <- formals(erahumed_simulation)

  pos <- 0.11

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
  global_params <- formals(erahumed_simulation)

  jgrow <- global_params$jgrow
  covmax <- global_params$covmax

  seed_day <- -150 + 1:365

  res <- ct_cover(seed_day = seed_day, jgrow = jgrow, covmax = covmax)

  expect_vector(res, ptype = numeric(), size = length(seed_day))
  expect_true(all( 0 <= res & res <= 1 ) )

})

test_that("ct_msapp(): result is always a fraction of application",{
  chemicals <- unique(albufera_ca_schedules$chemical)
  for (chemical in chemicals) {
    res <- ct_msapp(
      application_kg = 1,
      drift = 0,
      cover = 0,
      is_empty = TRUE
      )
    expect_true(all( 0 <= res & res <= 1 ))
  }

})
