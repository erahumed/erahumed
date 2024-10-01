library(shinytest2)

test_that("Initial Shiny values are consistent", {
  skip("Reimplement with new API")
  skip_on_ci()

  app <- shiny_app()
  drv <- AppDriver$new(app, name = "ERAHUMED")

  drv$expect_values()
})
