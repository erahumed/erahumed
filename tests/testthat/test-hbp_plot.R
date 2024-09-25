hbp_obj <-
  model <- erahumed_model() |>
  compute_inp(
    outflows_df = albufera_outflows |>
      dplyr::filter("2020-01-01" <= date, date <= "2020-01-10")
  ) |>
  compute_hba() |>
  compute_hbp() |>
  hbp()


test_that("plot.hbp() succeeds w/ type='cluster_levels' and valid input", {
  expect_no_error(
    plot(hbp_obj, type = "cluster_levels", cluster_id = hbp_obj$cluster_id[1])
  )
})

test_that("plot.hbp() error w/ type='map'", {
  expect_error( plot(hbp_obj, type = "map") )
})

test_that("plot.hbp throws an error if no cluster is specified", {
  expect_error( plot(hbp_obj, type = "cluster_levels") )  # No cluster_id passed
})

