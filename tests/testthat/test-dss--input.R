test_that("UI succeeds", {
  expect_no_error(dss_input_ui("ui"))
})

shiny::testServer(dss_input_server, {
  do.call(session$setInputs, dss_input_defaults())

  expect_no_error(session$returned())
}) |> suppressMessages()
