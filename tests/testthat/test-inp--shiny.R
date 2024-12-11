test_that("UI succeeds", {
  expect_no_error(inpUI("ui"))
})

args <- list(
  shared = shiny::reactiveValues(
    selected_cluster_id = albufera_clusters$cluster_id[1]
    )
  )

shiny::testServer(inpServer, args = args, {
  session$setInputs(date_range = as.Date(c("2020-01-01", "2020-12-31")),
                    seed = 840,
                    prop_jsendra = 8,
                    prop_bomba = 1,
                    prop_clearfield = 1)

  expect_no_error(session$returned())
  expect_s3_class(session$returned(), "erahumed_inp")
}) |> suppressMessages()
