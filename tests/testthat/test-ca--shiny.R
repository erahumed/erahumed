test_that("UI succeeds", {
  expect_no_error(ca_input_ui("ui"))
})

shiny::testServer(ca_input_server, {

  expect_no_error(session$returned())
  expect_vector(session$returned(), list())

}) |> suppressMessages()
