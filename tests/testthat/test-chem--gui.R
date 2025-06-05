library(shiny, quietly = TRUE)

test_that("UI succeeds", {
  expect_no_error(chemical_db_ui("ui"))
})

test_that("Server runs without error", {
  expect_no_error( shiny::testServer(chemical_db_server, {}) )
})

test_that("Server returns without error", {
  shiny::testServer(chemical_db_server, {
    expect_no_error(session$returned)
  })
})

test_that("Server returns a reactive expression", {
  shiny::testServer(chemical_db_server, {
    expect_s3_class(session$returned, class(reactiveVal()))
  })
})

test_that("Return contains by default the list of predefined chemicals", {
  shiny::testServer(chemical_db_server, {
    res <- session$returned
    expect_identical(res(), default_chemical_db())
  })
})
