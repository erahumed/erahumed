test_that("plot.erahumed_ca does not produce an error with valid inputs", {
  input <- raw() |>
    hba() |>
    hbp(date_min = "2010-01-01", date_max = "2011-12-31") |>
    ca()

  expect_no_error(plot(input, cluster_id = input$cluster_id[1]))
})
