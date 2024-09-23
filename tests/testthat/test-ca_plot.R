test_that("plot.erahumed_ca does not produce an error with valid inputs", {
  input <- ca(
    hbp(date_min = "2020-04-01", date_max = "2020-08-01")
    )

  expect_no_error(plot(input, cluster_id = input$cluster_id[1]))
})
