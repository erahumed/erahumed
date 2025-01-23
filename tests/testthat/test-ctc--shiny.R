test_that("UI succeeds", {
  expect_no_error(ctc_input_ui("ui"))
})

shiny::testServer(ctc_input_server, {

  expect_no_error(session$returned())
  expect_vector(session$returned(), list())

}) |> suppressMessages()
