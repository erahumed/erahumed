test_that("albufera_hb_local(): Parquet cache coincides with default", {
  skip_if_not(is_checking())

  expected <- albufera_hb_local()
  actual <- withr::with_seed(840,
            withr::with_envvar(c("erahumed_use_precomputed" = "FALSE"),
              albufera_hb_local()
            ))

  expect_identical(actual, expected,)

  expect_true(all(
    sapply(colnames(expected), \(c) all.equal(actual[[c]], expected[[c]]))
    ))
})
