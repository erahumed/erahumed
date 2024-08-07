library(shiny)

test_that("UIs are return shiny objects without errors", {

  expect_no_error(hbUI("id"))
  expect_no_error(hbSetupUI("id"))
  expect_no_error(hbGlobalUI("id"))
  expect_no_error(hbLocalUI("id"))

})


testServer(hbGlobalServer, {
  session$setInputs(variable = "level")

  expect_equal(min(hb_data()$date), setup$date_range[1])
  expect_equal(max(hb_data()$date), setup$date_range[2])

  output$hb_plot
  },
  args = list(
    setup = list(date_range = as.Date(c("2013-01-01", "2014-12-31"))),
    data = function() list(outflows_df = albufera_outflows,
                           petp_df = albufera_petp,
                           management_df = albufera_management)
    )
  )

testServer(hbLocalServer, {

  output$albufera_map  # Plotted without error (this line takes ~3s)
  expect_null(hb_data())

  session$setInputs(cluster_id = albufera_clusters$cluster_id[1])
  expect_null(hb_data())  # Still NULL because not yet ran

  session$setInputs(run_button = 1)
  expect_s3_class(hb_data(), "hb_local")

  expect_equal(min(hb_data()$date), setup$date_range[1])
  expect_equal(max(hb_data()$date), setup$date_range[2])

  output$hb_plot

  skip("Testing click interaction of leaflet not currently implemented.")
  session$setInputs(albufera_map_shape_click = list(id = "NewpointB2-98_5"))

  expect_equal(input$cluster_id, input$albufera_map_shape_click$id)

  },
  args = list(
    setup = list(date_range = as.Date(c("2013-01-01", "2013-01-10")),
                 ideal_flow_rate_cm = 5,
                 hbl_seed = 840
                 ),
    data = function() list(outflows_df = albufera_outflows,
                           petp_df = albufera_petp,
                           management_df = albufera_management)
    )
  )
