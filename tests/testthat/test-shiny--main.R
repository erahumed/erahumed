test_that("UI succeeds", {
  expect_no_error(shiny_ui())
})

shiny::testServer(shiny_server, {
  expect_no_error(simulation())
}) |> suppressMessages()



