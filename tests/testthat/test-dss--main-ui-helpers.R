test_that("dss_title() returns a string", {
  expect_no_error(dss_title())
  expect_vector(dss_title(), ptype = character(), size = 1)
})

test_that("dss_favicon() returns a shiny tag", {
  expect_no_error(dss_favicon())
  expect_s3_class(dss_favicon(), class(shiny::tags$div()))
})


test_that("dss_footer() returns a tagList", {
  expect_no_error(dss_footer())
  expect_s3_class(dss_footer(), class = class(shiny::tagList()))
})

test_that("dss_header() returns a tagList", {
  expect_no_error(dss_header())
  expect_s3_class(dss_header(), class(shiny::tagList()))
})

test_that("dss_about_modal() returns a modal", {
  expect_no_error(dss_about_modal())
  expect_s3_class(dss_about_modal(), class(shiny::tags$div()))
})
