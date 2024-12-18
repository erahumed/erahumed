library(shinytest2)

test_that("Initial Shiny values are consistent", {
  skip_on_ci()

  app <- dss()
  drv <- AppDriver$new(app, name = "ERAHUMED", load_timeout = 60000)

  drv$expect_values()
})
