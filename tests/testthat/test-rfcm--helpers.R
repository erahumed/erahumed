test_that("get_management_df() succeeds", {
  expect_no_error(get_management_df(default_cluster_map()))
})

test_that("get_applications_df() succeeds", {
  expect_no_error(get_applications_df(default_cluster_map()))
})

test_that("get_chemical_db() succeeds", {
  expect_no_error(get_chemical_db(default_cluster_map()))
})

test_that("get_chemical_db() has the correct length in fixed case", {
  obj <- get_chemical_db(default_cluster_map())

  expect_length(obj, 8)
})

test_that("match_chemical() works correctly in fixed case", {
  obj <- get_chemical_db(default_cluster_map())

  expect_no_error( match_chemical(bentazone(), db = obj) )
  expect_true( !is.na(match_chemical(bentazone(), db = obj)) )
})
