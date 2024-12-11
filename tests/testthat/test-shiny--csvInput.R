test_that("UI succeeds", {
  expect_no_error(csvInputUI("ui"))
})

args <- list(initial_df = albufera_weather)
shiny::testServer(csvInputServer, args = args, {
  session$setInputs(rows = 5)

  # Test that server returns an object of class erahumed_simulation()
  expect_no_error(session$returned())
  expect_s3_class(session$returned(), class(data.frame()))

  # Test that download button works correctly
  expect_no_error(output$downloadDataCSV)
  expect_no_error(output$downloadDataExcel)
}) |> suppressMessages()
