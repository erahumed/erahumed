hbp_obj <- erahumed_model() |>
  compute_inp(
    outflows_df = albufera_outflows |>
      dplyr::filter("2020-01-01" <= date, date <= "2020-01-10")
  ) |>
  compute_hba() |>
  compute_hbp() |>
  hbp()


test_that("plot.hbp() succeeds w/ type='cluster_levels' and valid input", {
  cluster_id <- component_output(hbp_obj)$cluster_id[1]
  expect_no_error(
    plot(hbp_obj, type = "cluster_view", cluster_id = cluster_id)
  )
})

test_that("plot.hbp() error w/ type='map'", {
  expect_error( plot(hbp_obj, type = "map_view") )
})

test_that("plot.hbp throws an error if no cluster is specified", {
  expect_error( plot(hbp_obj, type = "cluster_view") )  # No cluster_id passed
})

