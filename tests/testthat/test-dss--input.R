test_that("UI succeeds", {
  expect_no_error(dss_input_ui("ui"))
})


shiny::testServer(dss_input_server, {
  do.call(session$setInputs, dss_input_defaults())

  # Server returns correctly
  expect_no_error(res <- session$returned)

  # Results is a reactive expression that contains the list of 'erahumed_simulation' arguments
  expect_s3_class(res, "reactiveExpr")
  expect_type(res(), "list")
  expect_setequal(names(res()), names(formals(erahumed_simulation)))

  # Altering a few parameters works fine
  new_seed <- 841
  new_date_range <- c("2019-01-01", "2019-12-31")
  session$setInputs(seed = new_seed, date_range = new_date_range)
  expect_no_error(session$returned)
  expect_equal(session$returned()$seed, new_seed)
  expect_equal(session$returned()$date_start, new_date_range[[1]])

}) |> suppressMessages()

library(shinytest2)

test_that("'reset' button works", {
  skip("'reset' button test skipped")
  # This test does not work because shinyjs::reset() is apparently not
  # compatible with shinytest2 (in order to make it work, we should explicitly
  # call "set_inputs()", which defeats the purpose of the whole test.
  # The techniques used here, however, may prove useful in other contexts.
  skip_on_ci()

  mock_app_ui <- function() { erahumed:::dss_input_ui("test") }
  mock_app_server <- function(input, output, session) {
    erahumed:::dss_input_server("test")
  }

  app <- shiny::shinyApp(ui = mock_app_ui, server = mock_app_server)
  drv <- AppDriver$new(app, name = "DSS input reset test")

  drv$set_inputs("test-sc_slope" = 42)
  sc_slope <- drv$get_value(input = "test-sc_slope")
  expect_equal(sc_slope, 42)

  drv$click("test-reset")
  sc_slope <- drv$get_value(input = "test-sc_slope")
  expect_equal(sc_slope, dss_input_defaults()[["sc_slope"]])

})
