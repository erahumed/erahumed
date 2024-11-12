test_that("UI succeeds", {
  expect_no_error(hbpUI("ui"))
})

args <- list(simulation = shiny::reactive(test_sim_small()))
shiny::testServer(hbpServer, args = args, {
  session$setInputs(ideal_flow_rate_cm = 5, seed = 840)

  # Test that server returns an object of class erahumed_simulation()
  expect_no_error(session$returned())
  expect_s3_class(session$returned(), class(erahumed_simulation()))

  # Test that map is created correctly
  expect_no_error(output$albufera_map)

  session$setInputs(cluster_id = albufera_clusters$cluster_id[1])
  expect_no_error(output$plot)

  # Test that download button works correctly
  expect_no_error(output$downloadData)
}) |> suppressMessages()
