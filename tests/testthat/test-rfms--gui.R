library(shiny, quietly = TRUE)

test_that("UI succeeds", {
  expect_no_error(rfms_ui("ui"))
})

skip("Dependencies on too many inputs that are not easily defined, testing too cumbersome")

test_that("Server runs without error", {
  expect_no_error(
    shiny::testServer(rfms_server, {

    }, args = list(get_etc(test_sim_small(), "chemical_db")))
    )
})


test_that("Server returns without error", {
  shiny::testServer(rfms_server, {
    expect_no_error(session$returned)
  }, args = list(get_etc(test_sim_small(), "chemical_db")))
})

test_that("Server returns a reactive expression", {
  shiny::testServer(rfms_server, {
    expect_s3_class(session$returned, class(reactiveVal()))
  }, args = list(get_etc(test_sim_small(), "chemical_db")))
})

test_that("Return value is a RFMS", {
  shiny::testServer(rfms_server, {
    res <- session$returned
    expect_s3_class(res(), class(new_management_system()))
  }, args = list(get_etc(test_sim_small(), "chemical_db")))
})
