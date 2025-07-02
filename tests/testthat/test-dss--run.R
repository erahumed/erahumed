library(shiny, quietly = TRUE)

test_that("Server runs without error", {
  expect_no_error( shiny::testServer(dss_run_server, {}) )
})



skip("Dependencies on too many inputs that are not easily defined, testing too cumbersome")

mock_sim <- erahumed_simulation(date_end = "2020-01-10")
parameters <- shiny::reactive(mock_sim$inputs)

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
