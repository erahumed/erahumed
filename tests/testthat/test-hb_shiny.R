library(shiny)

testServer(hbGlobalServer, {
  session$setInputs(
    date_range = as.Date(c("2013-01-01", "2014-12-31")),
    variable = "level"
    )

  expect_equal(min(hb_data()$date), input$date_range[1])
  expect_equal(max(hb_data()$date), input$date_range[2])

  output$hb_plot
})

testServer(hbLocalServer, {

  output$albufera_map  # Plotted without error (this line takes ~3s)
  expect_null(hb_data())

  session$setInputs(date_range = as.Date(c("2013-01-01", "2013-01-31")))

  expect_null(hb_data())  # Still NULL when only date range is set

  session$setInputs(cluster_id = albufera_clusters$cluster_id[1])

  expect_null(hb_data())  # Still NULL because not yet ran

  session$setInputs(run_button = 1)

  expect_s3_class(hb_data(), "hb_local")  # Still NULL because not yet ran

  expect_equal(min(hb_data()$date), input$date_range[1])
  expect_equal(max(hb_data()$date), input$date_range[2])

  output$hb_plot

  skip("Testing click interaction of leaflet not currently implemented.")
  session$setInputs(albufera_map_shape_click = list(id = "NewpointB2-98_5"))

  expect_equal(input$cluster_id, input$albufera_map_shape_click$id)

})
