shiny::testServer(shiny_server, {
  expect_no_error(simulation())
}) |> suppressMessages()



