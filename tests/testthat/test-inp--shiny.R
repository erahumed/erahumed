test_that("UI succeeds", {
  expect_no_error(inpUI("ui"))
})

args <- list(
  simulation = shiny::reactive(test_sim_small()),
  shared = shiny::reactiveValues(
    map = plot_albufera_clusters(),
    selected_cluster_id = albufera_clusters$cluster_id[1]
    )
  )

shiny::testServer(inpServer, args = args, {
  session$setInputs(date_range = as.Date(c("2020-01-01", "2020-12-31")),
                    seed = 840)

  # Test that server returns an object of class erahumed_simulation()
  expect_no_error(session$returned())
  expect_s3_class(session$returned(), class(erahumed_simulation()))
}) |> suppressMessages()
