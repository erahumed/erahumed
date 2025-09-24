test_that("chemical_editor_ui() succeeds", {
  expect_no_error( chemical_editor_ui("id") )
})

test_that("chemical_editor_ui() returns a shiny tag", {
  obj <- chemical_editor_ui("id")
  cl <- class(shiny::tagList())
  expect_s3_class(obj, cl)
})
