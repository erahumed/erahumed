test_that("UI succeeds", {
  expect_no_error(hbl_input_ui("ui"))
})

shiny::testServer(hbl_input_server, {

  expect_no_error(session$returned())
  expect_vector(session$returned(), list())

}) |> suppressMessages()
