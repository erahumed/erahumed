library(shiny, quietly = TRUE)

test_that("UI succeeds", {
  expect_no_error(dss_ui())
})

test_that("Server runs without error", {
  expect_no_error( shiny::testServer(dss_server, {}) )
})

test_that("Internal simulation object is correctly initialized", {
  expect_no_error( shiny::testServer(dss_server, {
    # For this test, we need to initialize the inputs defined in the UI of the
    # dss_input module, whose id inside the main app is 'dss_input'.
    inputs <- dss_input_defaults()
    inputs$date_end <- "2020-01-10"  # To speed up the test
    names(inputs) <- shiny::NS("dss_input")(names(inputs))
    do.call(session$setInputs, inputs)

    simulation()
  }) )
})

test_that("Internal map of the albufera lake is correctly initialized ", {
  shiny::testServer(dss_server, {
    # For this test, we need to initialize the inputs defined in the UI of the
    # dss_input module, whose id inside the main app is 'dss_input'.
    inputs <- dss_input_defaults()
    inputs$date_end <- "2020-01-10"  # To speed up the test
    names(inputs) <- shiny::NS("dss_input")(names(inputs))
    do.call(session$setInputs, inputs)

    expect_no_error(output$map)
    expect_true(!is.null(output$map))
  })
})
