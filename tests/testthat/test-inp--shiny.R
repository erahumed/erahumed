test_that("UI succeeds", {
  expect_no_error(inp_input_ui("ui"))
})

shiny::testServer(inp_input_server, {
  skip("inp_input_server() needs input initialization, think of a better strategy")

  expect_no_error(session$returned())
  expect_vector(session$returned(), list())

}) |> suppressMessages()
