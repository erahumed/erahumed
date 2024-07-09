library(shiny)

testServer(hbServer, {
  session$setInputs(
    date_range = as.Date(c("2013-01-01", "2014-12-31")),
    variable = "level"
    )

  d <- hb_data()
  expect_equal(min(d$date), input$date_range[1])
  expect_equal(max(d$date), input$date_range[2])

  output$hb_plot
})
