library(shinytest2)

test_that("Initial Shiny values are consistent", {
  skip_on_ci()
  skip_on_covr()

  app <- dss()
  drv <- AppDriver$new(app, name = "ERAHUMED", load_timeout = 60000)

  drv$click("run", wait_ = FALSE)


  drv$expect_values()
})
