test_that("parse_docs_yml() succeeds", {
  expect_no_error( parse_docs_yml() )
})

test_that("parse_docs_yml() returns a list", {
  obj <- parse_docs_yml()
  expect_vector(obj, ptype = list())
})



test_that("get_param_docs() succeeds for a valid parameter", {
  expect_no_error( get_param_docs("seed", "simulation") )
})

test_that("get_param_docs() returns a list for a valid parameter", {
  obj <- get_param_docs("seed", "simulation")
  expect_vector(obj, ptype = list())
})

test_that("get_param_docs() warns for an invalid parameter", {
  expect_warning( get_param_docs("non_existing_parameter", "simulation") )
})

test_that("get_param_docs() returns NULL for an invalid parameter", {
  suppressWarnings(
    obj <- get_param_docs("non_existing_parameter", "simulation")
  )
  expect_true(is.null(obj))
})



test_that("get_param_roxy() succeeds for a valid parameter", {
  expect_no_error( get_param_roxy("seed", "simulation") )
})

test_that("get_param_roxy() returns a string for a valid parameter", {
  obj <- get_param_roxy("seed", "simulation")
  expect_vector(obj, ptype = character(), size = 1)
})



test_that("get_param_desc() succeeds for a valid parameter", {
  expect_no_error( get_param_desc("seed", "simulation") )
})

test_that("get_param_desc() returns a string for a valid parameter", {
  obj <- get_param_desc("seed", "simulation")
  expect_vector(obj, ptype = character(), size = 1)
})



test_that("get_dataset_format_roxy() succeeds for a valid parameter", {
  expect_no_error( get_dataset_format_roxy("weather_df", "simulation") )
})

test_that("get_dataset_format_roxy() returns a string for a valid parameter", {
  obj <- get_dataset_format_roxy("weather_df", "simulation")
  expect_vector(obj, ptype = character(), size = 1)
})

test_that("get_dataset_format_roxy() returns a string also for invalid df", {
  obj <- get_dataset_format_roxy("seed", "simulation")
  expect_vector(obj, ptype = character(), size = 1)
})




test_that("get_param_docs_df() succeeds for a valid 'fun' arguments", {
  expect_no_error( get_param_docs_df("simulation") )
  expect_no_error( get_param_docs_df("chemical") )
})


test_that("get_param_docs_df() returns a data.frame", {
  obj <- get_param_docs_df("simulation")
  expect_s3_class(obj, class(data.frame()))
})
