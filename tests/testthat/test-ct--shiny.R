test_that("UI succeeds", {
  expect_no_error(ctUI("ui"))
})

args <- list(
  simulation = shiny::reactive(test_sim_small()),
  shared = shiny::reactiveValues(
    selected_cluster_id = albufera_clusters$cluster_id[1]
  )
)

shiny::testServer(ctServer, args = args, {
  session$setInputs(seed = 840, cluster_id = albufera_clusters$cluster_id[1])
  do.call(session$setInputs, formals(setup_ct)[-1])

  expect_no_error(session$returned())
  expect_s3_class(session$returned(), class(erahumed_simulation()))
  expect_no_error(output$plot)

  expect_no_error(output$downloadData)
}) |> suppressMessages()
