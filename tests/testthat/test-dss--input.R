library(shiny, quietly = TRUE)

test_that("UI succeeds", {
  expect_no_error(dss_input_ui("ui"))
})

test_that("Server runs without error", {
  expect_no_error( shiny::testServer(dss_input_server, {}) )
})

test_that("Server returns without error", {
  skip("Triggers a warning due to plot_albufera_clusters(), skipping for now")

  shiny::testServer(dss_input_server, {
    do.call(session$setInputs, dss_input_defaults())
    expect_no_error(session$returned)
  })
})

test_that("Server returns a reactive expression", {
  skip("Triggers a warning due to plot_albufera_clusters(), skipping for now")

  shiny::testServer(dss_input_server, {
    do.call(session$setInputs, dss_input_defaults())
    suppressWarnings( # Silence warning from plot_albufera_clusters()
      res <- session$returned
      )
    expect_s3_class(res, "reactiveExpr")
  })
})

test_that("Return contains the list of simulation parameters", {
  skip("TODO: waiting for custom chemical GUI implementation")
  shiny::testServer(dss_input_server, {
    do.call(session$setInputs, dss_input_defaults())
    res <- session$returned
    expect_type(res(), "list")
    expect_setequal(names(res()), names(formals(erahumed_simulation)))
  })
})

skip("Dependencies on too many inputs that are not easily defined, testing too cumbersome")

test_that("Altering a few input parameters works", {
  shiny::testServer(dss_input_server, {
    do.call(session$setInputs, dss_input_defaults())

    new_seed <- 841
    new_date_range <- c("2019-01-01", "2019-12-31")
    session$setInputs(seed = new_seed, date_range = new_date_range)
    expect_no_error(res <- session$returned)
    expect_equal(res()$seed, new_seed)
    expect_equal(res()$date_start, new_date_range[[1]])

  })
})

