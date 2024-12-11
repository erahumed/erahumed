test_that("UI succeeds", {
  expect_no_error(hbaUI("ui"))
})

s <- test_sim_small()
args <- list(
  inp = shiny::reactiveVal(get_layer(s, "inp")),
  shared = shiny::reactiveValues(
    selected_cluster_id = albufera_clusters$cluster_id[1]
  )
)

shiny::testServer(hbaServer, args = args, {
  session$setInputs(
    sc_intercept = 16.7459,
    sc_slope = 23.6577,
    p_surface = 114.226,
    etp_surface = 79.361
  )

  # Test that server returns an object of class erahumed_simulation()
  expect_no_error(session$returned())
  expect_s3_class(session$returned(), "erahumed_hba")

  # Test that plotting succeeds for all variables
  vars <- hba_var_labs(invert = TRUE)
  for (var in vars) {
    session$setInputs(variable = var)
    expect_no_error(output$plot)
  }

  # Test that download button works correctly
  expect_no_error(output$downloadData)
}) |> suppressMessages()

