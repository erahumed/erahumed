test_that("compute_hbp() does not raise an error with valid inputs", {
  expect_no_error( compute_hbp(test_mod_small()) )
})

test_that("compute_hbp() error if invalid ideal_flow_rate_cm", {
  expect_error(
    compute_hbp(test_mod_small(), ideal_flow_rate_cm = -1),
    class = "compute_hbp_argcheck_error"
  )

  expect_error(
    compute_hbp(test_mod_small(), ideal_flow_rate_cm = "one"),
    class = "compute_hbp_argcheck_error"
  )

  expect_error(
    compute_hbp(test_mod_small(), ideal_flow_rate_cm = NA),
    class = "compute_hbp_argcheck_error"
  )

  expect_error(
    compute_hbp(test_mod_small(), ideal_flow_rate_cm = Inf),
    class = "compute_hbp_argcheck_error"
  )

  expect_error(
    compute_hbp(test_mod_small(), ideal_flow_rate_cm = NaN),
    class = "compute_hbp_argcheck_error"
  )


})

test_that("compute_hbp() error if invalid hba_res", {
  skip("Adapt to new implementation")

  m <- erahumed_model()

  # remove one required column
  expect_error(
    compute_hbp(m),
    class = "compute_hbp_argcheck_error"
  )



  expect_error(
    hbp(hba_res = inp() |> hba(),
        management_df = erahumed::albufera_management[,-1]),
    class = "compute_hbp_argcheck_error"
  )

  expect_error(
    hbp(hba_res = inp() |> hba(),
        clusters_df = erahumed::albufera_clusters[,-1]),
    class = "compute_hbp_argcheck_error"
  )

})


# Precomputed results

test_that("hbp_precomputed() does not return NULL normally", {
  skip("To adapt to new implementation")
  formals <- formals(hbp)
  call <- substitute(hbp())

  expect_s3_class(hbp_precomputed(formals, call), "erahumed_hbp")
})

test_that("hbp_precomputed() is NULL if envvar set to FALSE", {
  skip("To adapt to new implementation")
  formals <- formals(hbp)
  call <- substitute(hbp())
  withr::with_envvar(c(erahumed_use_precomputed = FALSE),
                     expect_null(hbp_precomputed(formals, call))
                     )

})

test_that("Parquet precomputed file coincides with would-be default value", {
  skip("To adapt to new implementation")
  skip_if_not(is_checking())

  expected <- hbp()
  actual <- withr::with_seed(840,
            withr::with_envvar(c(erahumed_use_precomputed = FALSE),
              hbp()
            ))

  expect_identical(actual, expected)

  expect_true(all(
    sapply(colnames(expected), \(c) all.equal(actual[[c]], expected[[c]]))
  ))
})


