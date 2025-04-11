test_that("dss() returns a shiny app object", {
  app <- dss()
  expect_s3_class(app, "shiny.appobj")
})
