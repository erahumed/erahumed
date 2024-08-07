library(shinytest2)

test_that("Initial Shiny values are consistent", {
  app <- shiny_app()
  drv <- AppDriver$new(app, name = "ERAHUMED")

  drv$expect_values()
})
