test_that("UI succeeds", {
  expect_no_error(ct_input_ui("ui"))
})

shiny::testServer(ct_input_server, {

  expect_no_error(session$returned())
  expect_vector(session$returned(), list())

}) |> suppressMessages()
