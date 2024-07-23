withr::with_envvar(c(erahumed_hb_sort_clusters = TRUE), {
  test_df <- albufera_hydro_balance_local(date_min = "2010-01-01",
                                          date_max = "2011-12-31")
  tol <- 1e-10
  })


test_that("outflows are always non-negative", {
  res <- test_df |>
    dplyr::filter(
      ideal_outflow_drain < 0 | real_outflow_drain < 0 |
      ideal_outflow_flux < 0  | real_outflow_flux < 0  |
      ideal_outflow_rain < 0  | real_outflow_rain < 0
      )

  expect_equal(nrow(res), 0)
})

test_that("sum of cluster outflows does never exceed total ditch outflow", {
  res <- test_df |>
    dplyr::group_by(date, ditch) |>
    dplyr::summarise(
      flowpoint = flowpoint[1], outflow = sum(real_outflow_m3_s), .groups = "drop"
      ) |>
    dplyr::filter(outflow > flowpoint + tol)

  expect_equal(nrow(res), 0)
  })

test_that("real outflows never exceed their ideal counterparts", {
  res <- test_df |>
    dplyr::filter(
      real_outflow_rain > ideal_outflow_rain + tol |
      real_outflow_drain > ideal_outflow_drain + tol |
      real_outflow_flux > ideal_outflow_flux + tol
      )

  expect_equal(nrow(res), 0)
})

test_that("ideal inflow is never greater than ideal height difference", {
  res <- test_df |>
    dplyr::filter(draining,
                  height_diff_cm - ideal_inflow_cm > mean(abs(ideal_inflow_cm)) * tol
                  )

  expect_equal(nrow(res), 0)
})

test_that("ideal inflow is zero if not irrigating", {
  res <- test_df |>
    dplyr::filter(!irrigation,
                  abs(ideal_inflow_cm) / mean(abs(ideal_inflow_cm)) > tol
    )

  expect_equal(nrow(res), 0)
})

test_that("real inflow is zero if not irrigating", {
  res <- test_df |>
    dplyr::filter(!irrigation,
                  abs(real_inflow_cm) / mean(abs(real_inflow_cm)) > tol
                  )

  skip("Inflow can actually be != 0 if Evap_mismatch != 0") # TODO
  expect_equal(nrow(res), 0)
})

test_that("If ideal_outflow_drain > 0 the cluster must be draining", {
  res <- test_df |>
    dplyr::filter(ideal_outflow_drain > 0, !draining)

  expect_equal(nrow(res), 0)
})

test_that("If accum_drain is greater than zero the cluster must be draining", {
  res <- test_df |>
    dplyr::filter(accum_drain > 0, !draining)

  expect_equal(nrow(res), 0)
})

test_that("simple snapshot is constant", {
  hash <- digest::digest(test_df)

  expect_snapshot(hash)
})
