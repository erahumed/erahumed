test_that("UI succeeds", {
  expect_no_error(hbaUI("ui"))
})

args <- list(simulation = shiny::reactive(test_sim_small()))
shiny::testServer(hbaServer, args = args, {
  # Test that server returns an object of class erahumed_simulation()
  expect_no_error(session$returned())
  expect_s3_class(session$returned(), class(erahumed_simulation()))

  # Test that plotting succeeds for all variables
  vars <- hba_var_labs(invert = TRUE)
  for (var in vars) {
    session$setInputs(variable = var)
    expect_no_error(output$plot)
  }

  # Test that download button works correctly
  expect_no_error(output$downloadData)
}) |> suppressMessages()

