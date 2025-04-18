library(shiny, quietly = TRUE)

test_that("Server runs without error", {
  expect_no_error( shiny::testServer(dss_run_server, {}) )
})

parameters <- formals(erahumed_simulation) |>
  as.list() |>
  (\(.) {.$date_end <- "2020-01-10"; .})() |>
  shiny::reactive()

test_that("Server returns a erahumed_simulation", {
  shiny::testServer(dss_run_server, {
    expect_no_error(res <- session$returned)
    expect_no_error(res())
    expect_s3_class(res, class(shiny::reactive(NULL)))
    expect_s3_class(res(), "erahumed_simulation")
  },
  args = list(parameters = parameters, run = shiny::reactive(0))
  )
})
