test_that("UI succeeds", {
  expect_no_error(caUI("ui"))
})

args <- list(
  simulation = shiny::reactive(test_sim_small()),
  shared = shiny::reactiveValues(
    map = plot_albufera_clusters(),
    selected_cluster_id = albufera_clusters$cluster_id[1]
  )
)

shiny::testServer(caServer, args = args, {
  session$setInputs(seed = 840, cluster_id = albufera_clusters$cluster_id[1])

  # Test that server returns an object of class erahumed_simulation()
  expect_no_error(session$returned())
  expect_s3_class(session$returned(), class(erahumed_simulation()))

  # Test that map is created correctly
  expect_no_error(output$map)

  expect_no_error(output$plot)

  # Test that download button works correctly
  expect_no_error(output$downloadData)
}) |> suppressMessages()
