library(shiny, quietly = TRUE)

test_that("UI succeeds", {
  expect_no_error(dss_output_ui("ui"))
})

test_that("Server runs without error", {
  expect_no_error( shiny::testServer(dss_output_server, {}) )
})

test_that("Plots are generated without any error", {
  shiny::testServer(dss_output_server, {
    session$setInputs(water_body = "lake", ct_chemical_ids = "1", risk_chemical_ids = "1")
    expect_no_error(output$hb_plot_storage)
    expect_no_error(output$hb_plot_flows)
    expect_no_error(output$ct_plot_water)
    expect_no_error(output$ct_plot_sediment)
    expect_no_error(output$r_plot)
  },
  args = list(simulation = shiny::reactive(test_sim_small()))
  )
})

test_that("Cluster informative text is generated correctly", {
  shiny::testServer(dss_output_server, {
    session$setInputs(water_body = info_clusters()$element_id[[1]])
    expect_no_error(output$selected_wb_info)
  },
  args = list(simulation = shiny::reactive(test_sim_small()))
  )
})
