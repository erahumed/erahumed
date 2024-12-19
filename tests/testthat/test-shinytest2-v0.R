library(shinytest2)

test_that("Initial Shiny values are consistent", {
  skip_on_ci()

  app <- dss_v0()
  drv <- AppDriver$new(app, name = "ERAHUMED")

  drv$expect_values()
})
